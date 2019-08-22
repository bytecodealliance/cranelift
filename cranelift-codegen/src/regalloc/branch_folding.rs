//! Folds redundant branches created by branch_splitting.
//!
//! The branch_splitting phase attempts to split possible critical edges so
//! that the register allocator always has a unique location per conditional branch
//! in which to insert register moves.
//!
//! If the register allocator decides to not use any of those blocks, they can
//! be removed from the flowgraph.
#![cfg(feature = "basic-blocks")]

use std::vec::Vec;

use crate::dominator_tree::DominatorTree;
use crate::flowgraph::{BasicBlock, ControlFlowGraph};
use crate::ir::{Ebb, Function, Inst, Opcode, ValueList};

/// Folds an ebb if it is redundant in the CFG.
/// Returns whether folding was performed (which invalidates the domtree).
fn try_fold_redundant_ebb(
    func: &mut Function,
    cfg: &mut ControlFlowGraph,
    ebb: Ebb
) -> bool {
    // Get branch information for the middle block.
    // This block was created by regalloc, and must end with a jump or fallthrough.
    let ebb_jump: Inst = func.layout.last_inst(ebb).expect("terminator");
    let ebb_dest: Ebb = func.dfg[ebb_jump].branch_destination().expect("single dest");

    // If there's more than just a jump in the block, the block isn't redundant.
    if func.layout.prev_inst(ebb_jump).is_some() {
        return false;
    }

    // Get branch information for the parent block.
    let BasicBlock { ebb: parent, inst: parent_jump } = cfg.pred_iter(ebb).nth(0).unwrap();
    if func.dfg[parent_jump].branch_destination().is_none() {
        return false; // Parent jump was multi-target: avoid patching it.
    }

    // The ebb has no parameters, but its destination may have some.
    // If it does, the parameters now need to be passed by the parent_jump.

    // Get the arguments and parameters passed by the parent branch.
    let num_fixed = func.dfg[parent_jump]
        .opcode()
        .constraints()
        .num_fixed_value_arguments();
    let (parent_jump_args, _parent_jump_params) = func.dfg[parent_jump]
        .arguments(&func.dfg.value_lists)
        .split_at(num_fixed);

    // Get the arguments and parameters passed by the edge branch.
    let num_fixed = func.dfg[ebb_jump]
        .opcode()
        .constraints()
        .num_fixed_value_arguments();
    let (_, ebb_jump_params) = func.dfg[ebb_jump]
        .arguments(&func.dfg.value_lists)
        .split_at(num_fixed);

    // Pass the ebb_jump_params in the parent_jump_params.
    // No rewriting is needed, because the edge ebb is parameterless.
    debug_assert!(_parent_jump_params.len() == 0);
    debug_assert!(func.dfg.num_ebb_params(ebb) == 0);
    let args: Vec<_> = parent_jump_args
        .iter()
        .chain(ebb_jump_params.iter())
        .map(|x| *x)
        .collect();
    let value_list = ValueList::from_slice(&args, &mut func.dfg.value_lists);

    func.dfg[parent_jump].take_value_list(); // Drop the current list.
    func.dfg[parent_jump].put_value_list(value_list); // Put the new list.

    // TODO: Need to fix "livein" analysis to remove the removed ebbs,
    // for each parameter that was passed by the edge ebb.

    // Bypass the ebb's jump. This disconnects the Ebb from the CFG.
    func.change_branch_destination(parent_jump, ebb_dest);
    cfg.recompute_ebb(func, parent);

    // The edge Ebb is now unreachable. Update the CFG.
    debug_assert!(cfg.pred_iter(ebb).count() == 0);
    while let Some(inst) = func.layout.first_inst(ebb) {
        func.layout.remove_inst(inst);
    }

    // Remove the block...
    cfg.recompute_ebb(func, ebb); // ...from predecessor lists.
    func.layout.remove_ebb(ebb); // ...from the layout.

    true
}

pub fn run(
    func: &mut Function,
    cfg: &mut ControlFlowGraph,
    domtree: &mut DominatorTree,
    split_blocks: Vec<Ebb>,
) {
    let mut folded = false;

    for ebb in split_blocks {
        // Ensure that each block is a well-formed split edge.
        #[cfg(debug_assertions)]
        {
            // Each split edge should have exactly one predecessor.
            debug_assert!(cfg.pred_iter(ebb).count() == 1);

            // There should be a single, unconditional jump.
            let last_inst = func.layout.last_inst(ebb).expect("terminator");
            let opcode = func.dfg[last_inst].opcode();
            debug_assert!(opcode == Opcode::Jump || opcode == Opcode::Fallthrough);

            debug_assert!(func.dfg[last_inst].branch_destination().is_some());
            if let Some(prev_inst) = func.layout.prev_inst(last_inst) {
                debug_assert!(func.dfg[prev_inst].branch_destination().is_none());
            }

            // The Ebb should be parameterless.
            debug_assert!(func.dfg.num_ebb_params(ebb) == 0);
        }

        folded |= try_fold_redundant_ebb(func, cfg, ebb);
    }

    // Folding jumps invalidates the dominator tree.
    if folded {
        domtree.compute(func, cfg);
    }
}
