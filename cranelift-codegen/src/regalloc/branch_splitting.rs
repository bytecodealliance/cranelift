//! Split the outgoing edges of conditional branches that pass parameters.
//!
//! The `minimal` and `tree` register allocators require this. One of the reason for splitting edges
//! is to be able to insert `copy` and `regmove` instructions between a conditional branch and the
//! following terminator.

use std::vec::Vec;

use crate::cursor::{Cursor, EncCursor};
use crate::dominator_tree::DominatorTree;
use crate::flowgraph::ControlFlowGraph;
use crate::ir::{Ebb, Function, Inst, InstBuilder, InstructionData, Opcode, Value};
use crate::isa::TargetIsa;
use crate::topo_order::TopoOrder;

pub fn run(
    isa: &TargetIsa,
    func: &mut Function,
    cfg: &mut ControlFlowGraph,
    domtree: &mut DominatorTree,
    topo: &mut TopoOrder,
) {
    let mut ctx = Context {
        new_blocks: false,
        cur: EncCursor::new(func, isa),
        domtree,
        topo,
        cfg,
    };
    ctx.run()
}

struct Context<'a> {
    // True if new blocks were inserted.
    new_blocks: bool,

    // Current instruction as well as reference to function and ISA.
    cur: EncCursor<'a>,

    // References to contextual data structures we need.
    domtree: &'a mut DominatorTree,
    topo: &'a mut TopoOrder,
    cfg: &'a mut ControlFlowGraph,
}

impl<'a> Context<'a> {
    fn run(&mut self) {
        // Any ebb order will do.
        self.topo.reset(self.cur.func.layout.ebbs());
        while let Some(ebb) = self.topo.next(&self.cur.func.layout, self.domtree) {
            // Branches can only be at the last or second to last position in an extended basic
            // block.
            self.cur.goto_last_inst(ebb);
            let terminator_inst = self
                .cur
                .current_inst()
                .expect("missing terminator instruction");
            match self.cur.prev_inst() {
                Some(inst) => {
                    let opcode = self.cur.func.dfg[inst].opcode();
                    if opcode.is_branch() {
                        self.visit_conditional_branch(inst, opcode);
                    } else {
                        continue;
                    }
                }
                // If there is only a terminator instruction, then there is no need to create a new
                // block.
                None => continue,
            };
            self.cur.goto_inst(terminator_inst);
            self.visit_terminator_branch(terminator_inst);
        }

        // If blocks were added the cfg and domtree are inconsistent and must be recomputed.
        if self.new_blocks {
            self.cfg.compute(&self.cur.func);
            self.domtree.compute(&self.cur.func, self.cfg);
        }
    }

    fn visit_conditional_branch(&mut self, inst: Inst, opcode: Opcode) {
        let target = match self.cur.func.dfg[inst] {
            InstructionData::Branch { destination, .. }
            | InstructionData::BranchIcmp { destination, .. }
            | InstructionData::BranchInt { destination, .. }
            | InstructionData::BranchFloat { destination, .. } => destination,
            _ => panic!("Unexpected instruction in classify_branch"),
        };

        // If there are any parameters, split the edge.
        if self.should_split_edge(inst, target) {
            // Remember the branch arguments.
            let jump_args: Vec<Value> = self
                .cur
                .func
                .dfg
                .inst_variable_args(inst)
                .iter()
                .map(|x| *x)
                .collect();

            // Create the block the branch will jump to.
            let new_ebb = self.make_empty_ebb();

            // Remove the arguments from the branch and make it jump to the new block.
            match opcode {
                Opcode::Brz => {
                    let val = self.cur.func.dfg.inst_args(inst)[0];
                    self.cur.func.dfg.replace(inst).brz(val, new_ebb, &[]);
                }
                Opcode::Brnz => {
                    let val = self.cur.func.dfg.inst_args(inst)[0];
                    self.cur.func.dfg.replace(inst).brnz(val, new_ebb, &[]);
                }
                Opcode::BrIcmp => {
                    if let InstructionData::BranchIcmp { cond, .. } = self.cur.func.dfg[inst] {
                        let x = self.cur.func.dfg.inst_args(inst)[0];
                        let y = self.cur.func.dfg.inst_args(inst)[0];
                        self.cur
                            .func
                            .dfg
                            .replace(inst)
                            .br_icmp(cond, x, y, new_ebb, &[]);
                    }
                }
                Opcode::Brif => {
                    if let InstructionData::BranchInt { cond, .. } = self.cur.func.dfg[inst] {
                        let val = self.cur.func.dfg.inst_args(inst)[0];
                        self.cur
                            .func
                            .dfg
                            .replace(inst)
                            .brif(cond, val, new_ebb, &[]);
                    }
                }
                Opcode::Brff => {
                    if let InstructionData::BranchFloat { cond, .. } = self.cur.func.dfg[inst] {
                        let val = self.cur.func.dfg.inst_args(inst)[0];
                        self.cur
                            .func
                            .dfg
                            .replace(inst)
                            .brff(cond, val, new_ebb, &[]);
                    }
                }
                _ => {
                    panic!("Unhandled side exit type");
                }
            }
            let ok = self.cur.func.update_encoding(inst, self.cur.isa).is_ok();
            debug_assert!(ok);

            // Insert a jump to the original target with the original arguments into the new block.
            self.cur.goto_first_insertion_point(new_ebb);
            self.cur.ins().jump(target, jump_args.as_slice());

            // Reset the cursor to point to the branch.
            self.cur.goto_inst(inst);
        }
    }

    fn visit_terminator_branch(&mut self, inst: Inst) {
        let inst_data = &self.cur.func.dfg[inst];
        let opcode = inst_data.opcode();
        if opcode != Opcode::Jump && opcode != Opcode::Fallthrough {
            // This opcode is ignored as it does not have any EBB parameters.
            if opcode != Opcode::IndirectJumpTableBr {
                debug_assert!(!opcode.is_branch())
            }
            return;
        }

        let target = match inst_data {
            InstructionData::Jump { destination, .. } => destination,
            _ => panic!(
                "Unexpected instruction {} in visit_terminator_branch",
                self.cur.display_inst(inst)
            ),
        };
        debug_assert!(self.cur.func.dfg[inst].opcode().is_terminator());

        // If there are any parameters, split the edge.
        if self.should_split_edge(inst, *target) {
            // Create the block the branch will jump to.
            let new_ebb = self.cur.func.dfg.make_ebb();
            self.new_blocks = true;

            // Split the current block before its terminator, and insert a new jump instruction to
            // jump to it.
            let jump = self.cur.ins().jump(new_ebb, &[]);
            self.cur.insert_ebb(new_ebb);

            // Reset the cursor to point to new terminator of the old ebb.
            self.cur.goto_inst(jump);
        }
    }

    // A new ebb must be inserted before the last ebb because the last ebb may have a
    // fallthrough_return and can't have anything after it.
    fn make_empty_ebb(&mut self) -> Ebb {
        let new_ebb = self.cur.func.dfg.make_ebb();
        let last_ebb = self.cur.layout().last_ebb().unwrap();
        self.cur.layout_mut().insert_ebb(new_ebb, last_ebb);
        self.new_blocks = true;
        new_ebb
    }

    /// Returns whether we should introduce a new branch.
    fn should_split_edge(&self, _branch: Inst, target: Ebb) -> bool {
        // We should split the edge if the target has any parameters.
        if self.cur.func.dfg.ebb_params(target).len() > 0 {
            return true;
        };

        // Or, if the target is has more than one block reaching it.
        if self.cfg.pred_iter(target).count() != 1 {
            return true;
        };

        false
    }
}
