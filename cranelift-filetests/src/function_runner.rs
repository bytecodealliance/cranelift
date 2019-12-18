use core::fmt::{self, Display, Formatter};
use core::mem;
use core::str::FromStr;
use cranelift_codegen::binemit::{NullRelocSink, NullStackmapSink, NullTrapSink};
use cranelift_codegen::ir::{types, AbiParam, ArgumentPurpose, ConstantData, Function};
use cranelift_codegen::isa::TargetIsa;
use cranelift_codegen::{settings, Context};
use cranelift_native::builder as host_isa_builder;
use cranelift_reader::Parser;
use memmap::{Mmap, MmapMut};

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
    TestWithResult { operator: Operator, expected: Value },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    EQ,
    NEQ,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(match self {
            Self::EQ => "==",
            Self::NEQ => "!=",
        })
    }
}

impl FromStr for Operator {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "==" => Ok(Self::EQ),
            "!=" => Ok(Self::NEQ),
            _ => Err(format!("Unsupported operator {}", s)),
        }
    }
}

macro_rules! try_into_int {
    ($type: ty, $name: ident) => {
        struct $name {}
        impl $name {
            fn try_into(d: ConstantData) -> Result<$type, String> {
                if d.len() == (mem::size_of::<$type>()) {
                    let v = d.into_vec().into_iter().rev();
                    let mut r: $type = 0;
                    for b in v {
                        r = r << 8 | b as $type;
                    }
                    Ok(r)
                } else {
                    Err(format!(
                        "Incorrect vector size: {}, expected {}",
                        d.len(),
                        mem::size_of::<$type>()
                    ))
                }
            }
        }
    };
}

try_into_int!(u16, ConstantDataU16);
try_into_int!(u32, ConstantDataU32);
try_into_int!(u64, ConstantDataU64);
try_into_int!(u128, ConstantDataU128);

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    F32(f32),
    F64(f64),
    I8(u8),
    I16(u16),
    I32(u32),
    I64(u64),
    I128(u128),
}

impl Value {
    fn parse_as(s: &str, ty: types::Type) -> Result<Self, String> {
        if ty.is_vector() {
            return Err(format!("Expected a non-vector type, not {}", ty));
        }

        let mut p = Parser::new(s);

        let constant_data = match ty {
            types::I8 => {
                let data = p.match_uimm8("Expected a 8-bit unsigned integer").unwrap();
                Self::I8(data.into())
            }
            types::I16 => {
                let data = p.match_constant_data(ty).unwrap();
                Self::I16(ConstantDataU16::try_into(data).unwrap())
            }
            types::I32 => {
                let data = p
                    .match_uimm32("Expected a 32-bit unsigned integer")
                    .unwrap();
                Self::I32(data.into())
            }
            types::I64 => {
                let data = p
                    .match_uimm64("Expected a 64-bit unsigned integer")
                    .unwrap();
                Self::I64(data.into())
            }
            types::I128 => {
                let data = p.match_constant_data(ty).unwrap();
                let data = ConstantDataU128::try_into(data).unwrap();
                Self::I128(data.rotate_left(64))
            }
            types::F32 => {
                let data = ConstantData::default()
                    .append(p.match_ieee32("Expected a 32-bit float").unwrap());
                let data = ConstantDataU32::try_into(data).unwrap();
                Self::F32(f32::from_bits(data))
            }
            types::F64 => {
                let data = ConstantData::default()
                    .append(p.match_ieee64("Expected a 64-bit float").unwrap());
                let data = ConstantDataU64::try_into(data).unwrap();
                Self::F64(f64::from_bits(data))
            }
            b if b.is_bool() => Self::Bool(p.match_bool("Expected a boolean").unwrap()),
            _ => {
                return Err(format!(
                    "Expected a type of: float, int or bool, found {}.",
                    ty
                ))
            }
        };
        Ok(constant_data)
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

    unsafe fn invoke<T>(code_page: &Mmap) -> T {
        let callable_fn: fn() -> T = mem::transmute(code_page.as_ptr());
        callable_fn()
    }

    fn check_assertion<T>(value: T, operator: Operator, expected: T) -> Result<(), String>
    where
        T: std::fmt::Display,
        T: std::cmp::PartialEq,
        T: Copy,
    {
        let ok = match operator {
            Operator::EQ => value == expected,
            Operator::NEQ => value != expected,
        };

        if ok {
            return Ok(());
        }
        Err(format!(
            "Check failed, expected {} {} {}",
            expected, operator, value,
        ))
    }

    fn check(
        code_page: memmap::Mmap,
        ret_value_type: types::Type,
        operator: Operator,
        expected: Value,
    ) -> Result<(), String> {
        macro_rules! gen_match {
            ($x:ident, $($y: path => $z: ty, $t: path), +) => {
                match $x {
                    $($y => {
                        if let $t(v) = expected {
                            Self::check_assertion(unsafe { Self::invoke::<$z>(&code_page) }, operator, v)
                        } else {
                            panic!("Expected an enum member of Value, found {}", stringify!($t))
                        }
                    }),+ ,
                    _ => Err(format!("Unknown return type for check: {}", $x)),
                };
            };
        }

        gen_match!(ret_value_type,
            types::I8 => u8, Value::I8,
            types::I16 => u16, Value::I16,
            types::I32 => u32, Value::I32,
            types::I64 => u64, Value::I64,
            types::I128 => u128, Value::I128,
            types::B1 => bool, Value::Bool,
            types::B8 => bool, Value::Bool,
            types::B16 => bool, Value::Bool,
            types::B32 => bool, Value::Bool,
            types::B64 => bool, Value::Bool,
            types::B128 => bool, Value::Bool,
            types::F32 => f32, Value::F32,
            types::F64 => f64, Value::F64
        )
    }

    fn print(code_page: memmap::Mmap, ret_value_type: types::Type) -> Result<(), String> {
        macro_rules! gen_match {
            ($x:ident, $($y: path => $z: ty), +) => {
                match $x {
                    $($y => {
                        Err(format!("Return value is: {}", unsafe { Self::invoke::<$z>(&code_page) }))
                    }),+ ,
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

        // Check return type and merge two i64 to i128 if necessary.
        let ret_value_type = return_type(&func)?;

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

        match &self.action {
            FunctionRunnerAction::Test => {
                if !ret_value_type.is_bool() {
                    return Err(format!(
                        "Function {} return type is not boolean",
                        context.func.name
                    ));
                }
                if unsafe { Self::invoke::<bool>(&code_page) } {
                    Ok(())
                } else {
                    Err(format!("Failed: {}", context.func.name))
                }
            }
            FunctionRunnerAction::Print => Self::print(code_page, ret_value_type),
            FunctionRunnerAction::TestWithResult { operator, expected } => {
                Self::check(code_page, ret_value_type, *operator, *expected)
            }
        }
    }
}

fn return_type(func: &Function) -> Result<types::Type, String> {
    if !(func.signature.params.is_empty() && func.signature.returns.len() == 1) {
        return Err(format!(
            "function {} must have a signature like: () -> ty, found {}",
            func.name, func.signature
        ));
    }

    let normal_rets: Vec<&AbiParam> = func
        .signature
        .returns
        .iter()
        .filter(|r| r.purpose == ArgumentPurpose::Normal)
        .collect();
    if normal_rets.len() > 2 || normal_rets.len() == 0 {
        return Err(String::from("Number of return values not supported"));
    }
    if normal_rets.len() == 2 {
        // Only two i64 return arguments supported and they are evaluated as a i128.
        if !(normal_rets[0].value_type == types::I64 && normal_rets[1].value_type == types::I64) {
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
        let expected = Value::parse_as(parts[parts.len() - 1], ty).unwrap();
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
