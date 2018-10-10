//! Fold operations on constants at compile time.

use cursor::{Cursor, FuncCursor};
use ir::{self, InstBuilder};
// use timing;

/// Fold operations on constants.
///
/// It's important to note that this will not remove unused constants. It's
/// assumed that the DCE pass will take care of them.
pub fn fold_constants(func: &mut ir::Function) {
    // let _tt = timing::constant_folding();
    let mut pos = FuncCursor::new(func);

    while let Some(_ebb) = pos.next_ebb() {
        while let Some(inst) = pos.next_inst() {
            use ir::instructions::Opcode::*;
            match pos.func.dfg[inst].opcode() {
                Iadd => {
                    fold_numerical_binary(&mut pos.func.dfg, inst, |v0_imm, v1_imm| v0_imm + v1_imm)
                }
                Isub => {
                    fold_numerical_binary(&mut pos.func.dfg, inst, |v0_imm, v1_imm| v0_imm - v1_imm)
                }
                Imul => {
                    fold_numerical_binary(&mut pos.func.dfg, inst, |v0_imm, v1_imm| v0_imm * v1_imm)
                }
                Brz | Brnz => fold_simple_branch(&mut pos.func, inst),
                _ => {}
            }
        }
    }
}

/// Collapse a numerical binary function.
fn fold_numerical_binary<F>(dfg: &mut ir::DataFlowGraph, inst: ir::Inst, f: F)
where
    F: Fn(i64, i64) -> i64,
{
    let typevar = dfg.ctrl_typevar(inst);
    let (v0, v1) = {
        let values = dfg.inst_args(inst);
        assert!(values.len() == 2);
        (
            dfg.resolve_aliases(values[0]),
            dfg.resolve_aliases(values[1]),
        )
    };

    if let (ir::ValueDef::Result(v0_inst, _), ir::ValueDef::Result(v1_inst, _)) =
        (dfg.value_def(v0), dfg.value_def(v1))
    {
        let v0_imm: i64 = if let ir::InstructionData::UnaryImm {
            opcode: ir::Opcode::Iconst,
            imm,
        } = dfg[v0_inst]
        {
            imm.into()
        } else {
            return;
        };

        let v1_imm: i64 = if let ir::InstructionData::UnaryImm {
            opcode: ir::Opcode::Iconst,
            imm,
        } = dfg[v1_inst]
        {
            imm.into()
        } else {
            return;
        };

        dfg.replace(inst).iconst(typevar, f(v0_imm, v1_imm));
    }
}

/// Collapse a simple branch instructions.
fn fold_simple_branch(func: &mut ir::Function, inst: ir::Inst) {
    let (inst_opcode, cond, ebb, args) = {
        let values = func.dfg.inst_args(inst);
        let inst_data = &func.dfg[inst];
        assert!(values.len() >= 1);
        (
            inst_data.opcode(),
            func.dfg.resolve_aliases(values[0]),
            inst_data.branch_destination().unwrap(),
            values[1..].to_vec(),
        )
    };

    if let ir::ValueDef::Result(cond_inst, _) = func.dfg.value_def(cond) {
        match func.dfg[cond_inst] {
            ir::InstructionData::UnaryBool {
                opcode: ir::Opcode::Bconst,
                imm,
            } => {
                let branch_if_zero = match inst_opcode {
                    ir::Opcode::Brz => true,
                    ir::Opcode::Brnz => false,
                    _ => panic!("Invalid state found"),
                };

                if (branch_if_zero && !imm) || (!branch_if_zero && imm) {
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
            _ => return,
        }
    }
}
