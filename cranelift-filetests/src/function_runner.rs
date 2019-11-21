use core::fmt::{self, Display, Formatter};
use core::mem;
use core::str::FromStr;
use cranelift_codegen::binemit::{NullRelocSink, NullStackmapSink, NullTrapSink};
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, ConstantData, Function};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{settings, Context};
use cranelift_native::builder as host_isa_builder;
use cranelift_reader::parse_constant_data;
use memmap::Mmap;
use memmap::MmapMut;
use std::convert::TryInto;

/// Run a function on a host
pub struct FunctionRunner {
    function: Function,
    isa: Box<dyn TargetIsa>,
    action: FunctionRunnerAction,
}

/// Build a function runner
pub struct FunctionRunnerBuilder {
    function: Function,
    isa: Option<Box<dyn TargetIsa>>,
    action: FunctionRunnerAction,
}

#[derive(Clone, Debug)]
pub enum FunctionRunnerAction {
    Print,
    Test,
    TestWithResult {
        operator: Operator,
        expected: ConstantData,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    EQ,
    NEQ,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Operator::EQ => "==",
            Operator::NEQ => "!=",
        })
    }
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "==" => Ok(Operator::EQ),
            "!=" => Ok(Operator::NEQ),
            _ => Err(format!("Unsupported operator {}", s)),
        }
    }
}

impl FunctionRunner {
    /// Build a function runner from an action, a function and the ISA to run on (must be the host machine's ISA).
    pub fn new(function: Function, isa: Box<dyn TargetIsa>, action: FunctionRunnerAction) -> Self {
        Self {
            function,
            isa,
            action,
        }
    }

    fn invoke<T>(code_page: &Mmap) -> T
    where
        T: std::fmt::Display,
    {
        let callable_fn: fn() -> T = unsafe { mem::transmute(code_page.as_ptr()) };
        callable_fn()
    }

    fn check_assertion<T>(
        value: T,
        operator: Operator,
        expected: T,
        print_only: bool,
    ) -> Result<(), String>
    where
        T: std::fmt::Display,
        T: std::cmp::PartialEq,
        T: Copy,
    {
        if print_only {
            return Err(format!("Return value is: {}", value));
        }

        let ok = match operator {
            Operator::EQ => value == expected,
            Operator::NEQ => value != expected,
        };

        if ok {
            return Ok(());
        }
        return Err(format!(
            "Check failed, expected {} {} {}",
            expected, operator, value,
        ));
    }

    fn check(
        code_page: memmap::Mmap,
        ret_value_type: types::Type,
        operator: Operator,
        expected: ConstantData,
        print_only: bool,
    ) -> Result<(), String> {
        macro_rules! gen_match {
            ($x:ident, $($y: path => $z: ty), +) => {
                match $x {
                    $($y => {
                        let expected_value: $z = expected.try_into().unwrap();
                        Self::check_assertion(Self::invoke::<$z>(&code_page), operator, expected_value, print_only)
                    }),+ ,
                    types::I128 => {
                        let expected_value: u128 = expected.try_into().unwrap();
                        let expected_value = expected_value.rotate_left(64);
                        Self::check_assertion(Self::invoke::<u128>(&code_page), operator, expected_value, print_only)
                    }
                    _ => Err(format!("Unknown return type for check: {}", $x)),
                };
            };
        }

        gen_match!(ret_value_type,
            types::I8 => u8,
            types::I16 => u16,
            types::I32 => u32,
            types::I64 => u64,
            types::B1 => bool,
            types::B8 => bool,
            types::B16 => bool,
            types::B32 => bool,
            types::B64 => bool,
            types::B128 => bool,
            types::F32 => f32,
            types::F64 => f64
        )
    }

    /// Compile and execute a single function, expecting a boolean to be returned; a 'true' value is
    /// interpreted as a successful test execution and mapped to Ok whereas a 'false' value is
    /// interpreted as a failed test and mapped to Err.
    pub fn run(&self) -> Result<(), String> {
        let func = self.function.clone();

        if !(func.signature.params.is_empty() && func.signature.returns.len() == 1) {
            return Err(format!(
                "Functions must have a signature like: () -> ty, found {}",
                func.name
            ));
        }

        if func.signature.call_conv != self.isa.default_call_conv() {
            return Err(format!(
                "Functions only run on the host's default calling convention; remove the specified calling convention in the function signature to use the host's default, in {}",
                func.name
            ));
        }

        // set up the context
        let mut context = Context::new();
        context.func = func;

        // compile and encode the result to machine code
        let relocs = &mut NullRelocSink {};
        let traps = &mut NullTrapSink {};
        let stackmaps = &mut NullStackmapSink {};
        let code_info = context
            .compile(self.isa.as_ref())
            .map_err(|e| e.to_string())?;
        let mut code_page =
            MmapMut::map_anon(code_info.total_size as usize).map_err(|e| e.to_string())?;

        unsafe {
            context.emit_to_memory(
                self.isa.as_ref(),
                code_page.as_mut_ptr(),
                relocs,
                traps,
                stackmaps,
            );
        };

        let code_page = code_page.make_exec().map_err(|e| e.to_string())?;

        // Merge two i64 to i128 if necessary.
        let ret_value_type = return_type(&context.func)?;

        match &self.action {
            FunctionRunnerAction::Test => {
                if !ret_value_type.is_bool() {
                    return Err(format!(
                        "Function {} return type is not boolean",
                        context.func.name
                    ));
                }
                if Self::invoke::<bool>(&code_page) {
                    Ok(())
                } else {
                    Err(format!("Failed: {}", context.func.name))
                }
            }
            FunctionRunnerAction::Print => Self::check(
                code_page,
                ret_value_type,
                Operator::EQ,
                ConstantData::default().append(1),
                true,
            ),
            FunctionRunnerAction::TestWithResult { operator, expected } => {
                let expected = if expected.len() < (ret_value_type.bytes() as usize) {
                    expected.clone().expand_to(ret_value_type.bytes() as usize)
                } else {
                    expected.clone()
                };

                Self::check(code_page, ret_value_type, *operator, expected, false)
            }
        }
    }
}

fn return_type(func: &Function) -> Result<types::Type, String> {
    let normal_rets: Vec<&AbiParam> = func
        .signature
        .returns
        .iter()
        .filter(|r| r.purpose == ArgumentPurpose::Normal)
        .collect();
    if normal_rets.len() > 2 || normal_rets.len() == 0 {
        return Err(String::from("Function return type is not supported"));
    }
    if normal_rets.len() == 2 {
        // Only two i64 return arguments supported and they are evaluated as a i128.
        let both_i64 = normal_rets
            .into_iter()
            .map(|p| p.value_type)
            .all(|ty| ty == types::I64);

        if !both_i64 {
            return Err(String::from(
                "Function return type with two parameters can only return i64, i64.",
            ));
        }

        return Ok(types::I128);
    }
    Ok(normal_rets[0].value_type)
}

impl FunctionRunnerBuilder {
    /// Create a new function runner builder with the specified function.
    pub fn new(function: Function) -> Self {
        Self {
            action: FunctionRunnerAction::Test,
            function: function,
            isa: None,
        }
    }

    /// Use the specified action.
    pub fn with_action(mut self, action: FunctionRunnerAction) -> Self {
        self.action = action;
        self
    }

    /// Parse and use the specified action.
    pub fn with_action_text(mut self, action: &str) -> Self {
        self.action = Self::parse_action(action, return_type(&self.function).unwrap());
        self
    }

    /// Use the specified ISA (must be the host machine's ISA).
    pub fn with_isa(mut self, isa: Box<dyn TargetIsa>) -> Self {
        self.isa = Some(isa);
        self
    }

    /// Use the host ISA with the specified flags.
    pub fn with_host_isa(self, flags: settings::Flags) -> Self {
        let builder = host_isa_builder().expect("Unable to build a TargetIsa for the current host");
        self.with_isa(builder.finish(flags))
    }

    /// Use the default host ISA.
    pub fn with_default_host_isa(self) -> Self {
        let flags = settings::Flags::new(settings::builder());
        self.with_host_isa(flags)
    }

    /// Build a function runner from an action, a function and the ISA to run on.
    pub fn finish(self) -> FunctionRunner {
        FunctionRunner::new(self.function, self.isa.unwrap(), self.action)
    }

    /// Parse a function runner action from a comment string such as "; run print".
    fn parse_action(comment: &str, ty: types::Type) -> FunctionRunnerAction {
        let comment = comment.trim();
        let comment = comment.trim_start_matches(";");
        let comment = comment.trim_start();
        let comment = comment.trim_start_matches("run");
        let comment = comment.trim_start_matches(":");
        let comment = comment.trim_start();

        if comment.len() == 0 {
            // Don't print test return value.
            return FunctionRunnerAction::Test;
        }

        if comment == "print" {
            // Print test return value, if return type is not b1, always fail the test.
            return FunctionRunnerAction::Print;
        }

        // ";   run:  %fn() > 42 " is now "%fn() > 42".
        let mut parts: Vec<&str> = comment.split_ascii_whitespace().collect();

        if parts.len() < 3 {
            panic!("Invalid syntax for test assertion, expected \"%fn() <operator> <value>\", found \"{}\"", comment);
        }

        // Last part must be a constant.
        let expected = parse_constant_data(parts[parts.len() - 1], ty).unwrap();
        parts.drain(parts.len() - 1..);

        let operator: Operator = parts[parts.len() - 1].parse().unwrap();
        parts.drain(parts.len() - 1..);

        if parts.len() != 1 {
            // Arguments not supported.
            panic!("Specifying arguments is not supported. \"%fn()\" <operator> <value>\", found \"{}\"", parts.join(" "));
        }

        let invocation = parts[0].trim();
        let invocation = invocation.trim_start_matches("%fn(");
        let invocation = invocation.trim_end_matches(")");
        let invocation = invocation.trim();

        if invocation.len() == 0 {
            return FunctionRunnerAction::TestWithResult {
                operator: operator,
                expected: expected,
            };
        }

        panic!(
            "Expected \"%fn()\" <operator> <value>\", found \"{}\"",
            parts.join(" ")
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use cranelift_reader::{parse_test, ParseOptions};

    #[test]
    fn nop() {
        let code = String::from(
            "
            test run
            function %test() -> b8 {
            ebb0:
                nop
                v1 = bconst.b8 true
                return v1
            }",
        );

        // extract function
        let test_file = parse_test(code.as_str(), ParseOptions::default()).unwrap();
        assert_eq!(1, test_file.functions.len());
        let function = test_file.functions[0].0.clone();

        // execute function
        let runner = FunctionRunnerBuilder::new(function)
            .with_default_host_isa()
            .finish();
        runner.run().unwrap() // will panic if execution fails
    }
}
