//! Fold operations on constants at compile time.

use cranelift_codegen::{
    cursor::{Cursor, FuncCursor},
    ir::{self, InstBuilder},
};
use rustc_apfloat::{
    ieee::{Double, Single},
    Float,
};

enum ConstImm {
    Bool(bool),
    I64(i64),
    Ieee32(Single),
    Ieee64(Double),
}

impl ConstImm {
    fn unwrap_i64(self) -> i64 {
        if let ConstImm::I64(imm) = self {
            imm
        } else {
            panic!("self did not contain an `i64`.")
        }
    }

    fn evaluate_truthiness(self) -> bool {
        match self {
            ConstImm::Bool(b) => b,
            ConstImm::I64(imm) => imm != 0,
            _ => panic!(
                "Only a `ConstImm::Bool` and `ConstImm::I64` can be evaluated for \"truthiness\""
            ),
        }
    }
}

/// Fold operations on constants.
///
/// It's important to note that this will not remove unused constants. It's
/// assumed that the DCE pass will take care of them.
pub fn fold_constants(func: &mut ir::Function) {
    let mut pos = FuncCursor::new(func);

    while let Some(_ebb) = pos.next_ebb() {
        while let Some(inst) = pos.next_inst() {
            use self::ir::InstructionData::*;
            match pos.func.dfg[inst] {
                Binary { opcode, args } => {
                    fold_numerical_binary(&mut pos.func.dfg, inst, opcode, args);
                }
                Branch {
                    opcode,
                    args: _,
                    destination: _,
                } => {
                    fold_simple_branch(&mut pos.func, inst, opcode);
                }
                _ => {}
            }
        }
    }
}

fn resolve_value_to_imm(dfg: &ir::DataFlowGraph, value: ir::Value) -> Option<ConstImm> {
    let original = dfg.resolve_aliases(value);

    let inst = dfg.value_def(original).unwrap_inst();

    use self::ir::{InstructionData::*, Opcode::*};
    match dfg[inst] {
        UnaryImm {
            opcode: Iconst,
            imm,
        } => Some(ConstImm::I64(imm.into())),
        UnaryIeee32 {
            opcode: F32const,
            imm,
        } => {
            let ieee_f32 = Single::from_bits(imm.bits() as _);
            Some(ConstImm::Ieee32(ieee_f32))
        }
        UnaryIeee64 {
            opcode: F64const,
            imm,
        } => {
            let ieee_f64 = Double::from_bits(imm.bits() as _);
            Some(ConstImm::Ieee64(ieee_f64))
        }
        UnaryBool {
            opcode: Bconst,
            imm,
        } => Some(ConstImm::Bool(imm)),
        _ => None,
    }
}

fn evaluate_numerical_binary(
    dfg: &ir::DataFlowGraph,
    opcode: ir::Opcode,
    args: [ir::Value; 2],
) -> Option<ConstImm> {
    use std::num::Wrapping;

    let (imm0, imm1) = (
        resolve_value_to_imm(dfg, args[0])?,
        resolve_value_to_imm(dfg, args[1])?,
    );

    match opcode {
        ir::Opcode::Iadd => {
            let imm0 = Wrapping(imm0.unwrap_i64());
            let imm1 = Wrapping(imm1.unwrap_i64());
            Some(ConstImm::I64((imm0 + imm1).0))
        }
        ir::Opcode::Isub => {
            let imm0 = Wrapping(imm0.unwrap_i64());
            let imm1 = Wrapping(imm1.unwrap_i64());
            Some(ConstImm::I64((imm0 - imm1).0))
        }
        ir::Opcode::Imul => {
            let imm0 = Wrapping(imm0.unwrap_i64());
            let imm1 = Wrapping(imm1.unwrap_i64());
            Some(ConstImm::I64((imm0 * imm1).0))
        }
        ir::Opcode::Udiv => {
            let imm0 = Wrapping(imm0.unwrap_i64());
            let imm1 = Wrapping(imm1.unwrap_i64());
            if imm1.0 == 0 {
                panic!("Cannot divide by a zero.")
            }
            Some(ConstImm::I64((imm0 / imm1).0))
        }
        ir::Opcode::Fadd => match (imm0, imm1) {
            (ConstImm::Ieee32(imm0), ConstImm::Ieee32(imm1)) => {
                Some(ConstImm::Ieee32((imm0 + imm1).value))
            }
            (ConstImm::Ieee64(imm0), ConstImm::Ieee64(imm1)) => {
                Some(ConstImm::Ieee64((imm0 + imm1).value))
            }
            _ => unreachable!(),
        },
        ir::Opcode::Fsub => match (imm0, imm1) {
            (ConstImm::Ieee32(imm0), ConstImm::Ieee32(imm1)) => {
                Some(ConstImm::Ieee32((imm0 - imm1).value))
            }
            (ConstImm::Ieee64(imm0), ConstImm::Ieee64(imm1)) => {
                Some(ConstImm::Ieee64((imm0 - imm1).value))
            }
            _ => unreachable!(),
        },
        ir::Opcode::Fmul => match (imm0, imm1) {
            (ConstImm::Ieee32(imm0), ConstImm::Ieee32(imm1)) => {
                Some(ConstImm::Ieee32((imm0 * imm1).value))
            }
            (ConstImm::Ieee64(imm0), ConstImm::Ieee64(imm1)) => {
                Some(ConstImm::Ieee64((imm0 * imm1).value))
            }
            _ => unreachable!(),
        },
        ir::Opcode::Fdiv => match (imm0, imm1) {
            (ConstImm::Ieee32(imm0), ConstImm::Ieee32(imm1)) => {
                if imm1.is_zero() {
                    return None;
                }
                Some(ConstImm::Ieee32((imm0 / imm1).value))
            }
            (ConstImm::Ieee64(imm0), ConstImm::Ieee64(imm1)) => {
                if imm1.is_zero() {
                    return None;
                }
                Some(ConstImm::Ieee64((imm0 / imm1).value))
            }
            _ => unreachable!(),
        },
        _ => None,
    }
}

/// Fold a numerical binary function.
fn fold_numerical_binary(
    dfg: &mut ir::DataFlowGraph,
    inst: ir::Inst,
    opcode: ir::Opcode,
    args: [ir::Value; 2],
) {
    if let Some(const_imm) = evaluate_numerical_binary(dfg, opcode, args) {
        use self::ConstImm::*;
        match const_imm {
            I64(imm) => {
                let typevar = dfg.ctrl_typevar(inst);
                dfg.replace(inst).iconst(typevar, imm);
            }
            Ieee32(imm) => {
                dfg.replace(inst)
                    .f32const(ir::immediates::Ieee32::with_bits(imm.to_bits() as u32));
            }
            Ieee64(imm) => {
                dfg.replace(inst)
                    .f64const(ir::immediates::Ieee64::with_bits(imm.to_bits() as u64));
            }
            _ => unreachable!(),
        }
    }
}

fn fold_simple_branch(func: &mut ir::Function, inst: ir::Inst, opcode: ir::Opcode) {
    let (cond, ebb, args) = {
        let values = func.dfg.inst_args(inst);
        let inst_data = &func.dfg[inst];
        (
            resolve_value_to_imm(&func.dfg, values[0]).unwrap(),
            inst_data.branch_destination().unwrap(),
            values[1..].to_vec(),
        )
    };

    let truthiness = cond.evaluate_truthiness();
    let branch_if_zero = match opcode {
        ir::Opcode::Brz => true,
        ir::Opcode::Brnz => false,
        _ => unreachable!(),
    };

    if (branch_if_zero && !truthiness) || (!branch_if_zero && truthiness) {
        func.dfg.replace(inst).jump(ebb, &args);
        // remove the rest of the ebb to avoid verifier errors
        while let Some(next_inst) = func.layout.next_inst(inst) {
            func.layout.remove_inst(next_inst);
        }
    } else {
        // we can't remove the current instruction, so replace
        // it with a `nop`. DCE will get rid of it for us.
        func.dfg.replace(inst).nop();
    }
}

// /// Collapse a simple branch instructions.
// fn fold_simple_branch(func: &mut ir::Function, inst: ir::Inst) {
//     let (inst_opcode, cond, ebb, args) = {
//         let values = func.dfg.inst_args(inst);
//         let inst_data = &func.dfg[inst];
//         assert!(values.len() >= 1);
//         (
//             inst_data.opcode(),
//             func.dfg.resolve_aliases(values[0]),
//             inst_data.branch_destination().unwrap(),
//             values[1..].to_vec(),
//         )
//     };

//     if let ir::ValueDef::Result(cond_inst, _) = func.dfg.value_def(cond) {
//         match func.dfg[cond_inst] {
//             ir::InstructionData::UnaryBool {
//                 opcode: ir::Opcode::Bconst,
//                 imm,
//             } => {
//                 let branch_if_zero = match inst_opcode {
//                     ir::Opcode::Brz => true,
//                     ir::Opcode::Brnz => false,
//                     _ => panic!("Invalid state found"),
//                 };

//                 if (branch_if_zero && !imm) || (!branch_if_zero && imm) {
//                     func.dfg.replace(inst).jump(ebb, &args);
//                     // remove the rest of the ebb to avoid verifier errors
//                     while let Some(next_inst) = func.layout.next_inst(inst) {
//                         func.layout.remove_inst(next_inst);
//                     }
//                 } else {
//                     // we can't remove the current instruction, so replace
//                     // it with a `nop`. DCE will get rid of it for us.
//                     func.dfg.replace(inst).nop();
//                 }
//             }
//             _ => return,
//         }
//     }
// }
