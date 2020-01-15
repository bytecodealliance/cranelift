//! Cranelift IR interpreter.
//!
//! This module contains the logic for interpreting Cranelift instructions.

use crate::environment::Environment;
use crate::frame::Frame;
use crate::value::Value;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::immediates::Imm64;
use cranelift_codegen::ir::{
    Ebb, FuncRef, Function, Inst, InstructionData, InstructionData::*, Opcode, Opcode::*,
    Value as ValueRef, ValueList,
};
use log::debug;
use std::ops::{Add, Sub};
use thiserror::Error;

/// The valid control flow states.
pub enum ControlFlow {
    Continue,
    ContinueAt(Ebb, Vec<ValueRef>),
    Return(Vec<Value>),
}

/// The ways interpretation can fail.
#[derive(Error, Debug)]
pub enum Trap {
    #[error("unknown trap")]
    Unknown,
    #[error("invalid type for {1}: expected {0}")]
    InvalidType(String, ValueRef),
    #[error("reached an unreachable statement")]
    Unreachable,
    #[error("invalid control flow: {0}")]
    InvalidControlFlow(String),
    #[error("invalid function reference: {0}")]
    InvalidFunctionReference(FuncRef),
    #[error("invalid function name: {0}")]
    InvalidFunctionName(String),
}

/// The Cranelift interpreter; it contains immutable elements such as the function environment and
/// implements the Cranelift IR semantics.
#[derive(Default)]
pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new(env: Environment) -> Self {
        Self { env }
    }

    pub fn call_by_name(&self, func_name: &str, arguments: &[Value]) -> Result<ControlFlow, Trap> {
        let func_ref = self
            .env
            .index_of(func_name)
            .ok_or_else(|| Trap::InvalidFunctionName(func_name.to_string()))?;
        self.call_by_index(func_ref, arguments)
    }

    pub fn call_by_index(
        &self,
        func_ref: FuncRef,
        arguments: &[Value],
    ) -> Result<ControlFlow, Trap> {
        match self.env.get_by_func_ref(func_ref) {
            None => Err(Trap::InvalidFunctionReference(func_ref)),
            Some(func) => self.call(func, arguments),
        }
    }

    fn call(&self, function: &Function, arguments: &[Value]) -> Result<ControlFlow, Trap> {
        debug!("Call: {}({:?})", function.name, arguments);
        let first_ebb = function.layout.ebbs().next().expect("to have a first ebb");
        let parameters = function.dfg.ebb_params(first_ebb);
        let mut frame = Frame::new(function).with_parameters(parameters, arguments);
        let result = self.block(&mut frame, first_ebb);
        result
    }

    fn block(&self, frame: &mut Frame, ebb: Ebb) -> Result<ControlFlow, Trap> {
        debug!("Block: {}", ebb);
        for inst in frame.function.layout.ebb_insts(ebb) {
            match self.inst(frame, inst)? {
                ControlFlow::Continue => continue,
                ControlFlow::ContinueAt(ebb, old_names) => {
                    let new_names = frame.function.dfg.ebb_params(ebb);
                    frame.rename(&old_names, new_names);
                    return self.block(frame, ebb); // TODO check that TCO happens
                }
                ControlFlow::Return(rs) => return Ok(ControlFlow::Return(rs)),
            }
        }
        Err(Trap::Unreachable)
    }

    fn binary(
        &self,
        frame: &mut Frame,
        op: fn(Value, Value) -> Value,
        a: ValueRef,
        b: ValueRef,
        r: ValueRef,
    ) {
        let a = frame.get(&a);
        let b = frame.get(&b);
        let c = op(a.clone(), b.clone());
        frame.set(r, c);
    }

    // TODO refactor to only one `binary` method
    fn binary_imm(
        &self,
        frame: &mut Frame,
        op: fn(Value, Value) -> Value,
        a: ValueRef,
        b: Value,
        r: ValueRef,
    ) {
        let a = frame.get(&a);
        let c = op(a.clone(), b);
        frame.set(r, c);
    }

    fn iconst(&self, frame: &mut Frame, imm: Imm64, r: ValueRef) {
        frame.set(r, Value::Int(imm.into()));
    }

    fn bconst(&self, frame: &mut Frame, imm: bool, r: ValueRef) {
        frame.set(r, Value::Bool(imm));
    }

    // TODO add load/store
    fn inst(&self, frame: &mut Frame, inst: Inst) -> Result<ControlFlow, Trap> {
        use ControlFlow::{Continue, ContinueAt};
        debug!("Inst: {}", &frame.function.dfg.display_inst(inst, None));

        let data = &frame.function.dfg[inst];
        match data {
            Binary { opcode, args } => match opcode {
                Iadd => {
                    // TODO trap if arguments are of the wrong type; here and below
                    let res = first_result(frame.function, inst);
                    self.binary(frame, Add::add, args[0], args[1], res);
                    Ok(Continue)
                }
                _ => unimplemented!(),
            },
            BinaryImm { opcode, arg, imm } => match opcode {
                IrsubImm => {
                    let res = first_result(frame.function, inst);
                    let imm = Value::Int((*imm).into());
                    self.binary_imm(frame, Sub::sub, *arg, imm, res);
                    Ok(Continue)
                }
                _ => unimplemented!(),
            },
            Branch {
                opcode,
                args,
                destination,
            } => match opcode {
                Brnz => {
                    let mut args = value_refs(frame.function, args);
                    let first = args.remove(0);
                    match frame.get(&first) {
                        Value::Bool(true) => Ok(ContinueAt(*destination, args)),
                        Value::Bool(false) => Ok(Continue),
                        _ => Err(Trap::InvalidType("bool".to_string(), args[0])),
                    }
                }
                _ => unimplemented!(),
            },
            InstructionData::Call { args, func_ref, .. } => {
                // Find the function to call.
                let func_name = function_name_of_func_ref(*func_ref, frame.function);

                // Call function.
                let args = frame.get_all(args.as_slice(&frame.function.dfg.value_lists));
                let result = self.call_by_name(&func_name, &args)?;

                // Save results.
                if let ControlFlow::Return(returned_values) = result {
                    let ssa_values = frame.function.dfg.inst_results(inst);
                    assert_eq!(
                        ssa_values.len(),
                        returned_values.len(),
                        "expected result length ({}) to match SSA values length ({}): {}",
                        returned_values.len(),
                        ssa_values.len(),
                        frame.function.dfg.display_inst(inst, None)
                    );
                    frame.set_all(ssa_values, returned_values);
                    Ok(Continue)
                } else {
                    Err(Trap::InvalidControlFlow(format!(
                        "did not return from: {}",
                        frame.function.dfg.display_inst(inst, None)
                    )))
                }
            }
            InstructionData::Jump {
                opcode,
                destination,
                args,
            } => match opcode {
                Opcode::Fallthrough => {
                    Ok(ContinueAt(*destination, value_refs(frame.function, args)))
                }
                Opcode::Jump => Ok(ContinueAt(*destination, value_refs(frame.function, args))),
                _ => unimplemented!(),
            },
            IntCompareImm {
                opcode,
                arg,
                cond,
                imm,
            } => match opcode {
                IcmpImm => {
                    let result = if let Value::Int(arg_value) = *frame.get(arg) {
                        let imm_value: i64 = (*imm).into();
                        match cond {
                            IntCC::UnsignedLessThanOrEqual => arg_value <= imm_value,
                            IntCC::Equal => arg_value == imm_value,
                            _ => unimplemented!(),
                        }
                    } else {
                        return Err(Trap::InvalidType("int".to_string(), *arg));
                    };
                    let res = first_result(frame.function, inst);
                    frame.set(res, Value::Bool(result));
                    Ok(Continue)
                }
                _ => unimplemented!(),
            },
            MultiAry { opcode, args } => match opcode {
                Return => {
                    let rs: Vec<Value> = args
                        .as_slice(&frame.function.dfg.value_lists)
                        .iter()
                        .map(|r| frame.get(r).clone())
                        .collect();
                    Ok(ControlFlow::Return(rs))
                }
                _ => unimplemented!(),
            },
            NullAry { opcode } => match opcode {
                Nop => Ok(Continue),
                _ => unimplemented!(),
            },
            UnaryImm { opcode, imm } => match opcode {
                Iconst => {
                    let res = first_result(frame.function, inst);
                    self.iconst(frame, *imm, res);
                    Ok(Continue)
                }
                _ => unimplemented!(),
            },
            UnaryBool { opcode, imm } => match opcode {
                Bconst => {
                    let res = first_result(frame.function, inst);
                    self.bconst(frame, *imm, res);
                    Ok(Continue)
                }
                _ => unimplemented!(),
            },

            _ => unimplemented!("{:?}", data),
        }
    }
}

fn first_result(function: &Function, inst: Inst) -> ValueRef {
    function.dfg.first_result(inst)
}

fn value_refs(function: &Function, args: &ValueList) -> Vec<ValueRef> {
    args.as_slice(&function.dfg.value_lists).to_vec()
}

/// Return the (external) function name of `func_ref` in a local `function`. Note that this may
/// be truncated.
fn function_name_of_func_ref(func_ref: FuncRef, function: &Function) -> String {
    function
        .dfg
        .ext_funcs
        .get(func_ref)
        .expect("function to exist")
        .name
        .to_string()
}

#[cfg(test)]
mod tests {}
