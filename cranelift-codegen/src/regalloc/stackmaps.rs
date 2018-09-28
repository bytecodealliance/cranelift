use crate::cursor::{Cursor, FuncCursor};
use crate::dominator_tree::DominatorTree;
use crate::ir::Function;
use crate::ir::InstBuilder;
use crate::regalloc::live_value_tracker::LiveValueTracker;
use crate::regalloc::liveness::Liveness;
use std::collections::HashSet;

// The emit_stackmaps() function analyzes each instruction to retrieve the liveness of
// the defs and operands by traversing the dominator tree in a post order fashion.
pub fn emit_stackmaps(
    func: &mut Function,
    domtree: &mut DominatorTree,
    liveness: &mut Liveness,
    tracker: &mut LiveValueTracker,
) {
    // Visit EBBs in post-order
    let mut pos = FuncCursor::new(func);
    let mut ebbs_for_stackmap = HashSet::new();

    for &ebb in domtree.cfg_postorder() {
        // call ebb_top && drop_dead_params
        tracker.ebb_top(ebb, &pos.func.dfg, liveness, &pos.func.layout, domtree);
        tracker.drop_dead_params();

        // From the top of the ebb, step through the instructions
        pos.goto_top(ebb);

        while let Some(inst) = pos.next_inst() {
            // Get opcode of instruction
            let opcode = pos.func.dfg[inst].opcode();

            // Process the instruction
            tracker.process_inst(inst, &pos.func.dfg, liveness);

            // Get rid of values that have either (1) Dead Definitions or (2) Killed by Inst
            tracker.drop_dead(inst);

            // create an empty vector to store the live values in
            let mut live_value_list = Vec::new();

            // Grab the values that are still live
            let live_info = tracker.live();

            if live_info.len() != 0 {
                for value_in_list in live_info {
                    // only store values that are reference types
                    if pos.func.dfg.ctrl_typevar(inst).is_ref() {
                        live_value_list.push(value_in_list.value);
                    }
                }
            }

            // Check if it's a branch instruction
            if opcode.is_branch() {
                // TODO: The stackmap instruction should only be added if the
                // branch_dest is above the branch in the layout

                // Find what the branch destination is
                let branch_dest = pos.func.dfg[inst].branch_destination();

                // Check if the branch destination is already in hash set
                if ebbs_for_stackmap.contains(&branch_dest) {
                    // If the destination is in the HashSet, we should replace the values
                    // that the stackmap instruction currently holds
                    pos.goto_first_insertion_point(branch_dest.unwrap());

                    // remove the current stackmap instruction with old values
                    pos.remove_inst();

                    // insert new stackmap instruction with new values
                    pos.ins().stackmap(&live_value_list);
                } else {
                    // If it isn't in the HashSet, we can insert stackmap instruction
                    // without any worries. We also insert the destination into HashSet
                    pos.goto_first_insertion_point(branch_dest.unwrap());
                    pos.ins().stackmap(&live_value_list);

                    ebbs_for_stackmap.insert(branch_dest);
                }
            }

            // Check if it's a call instruction
            if opcode.is_call() {
                // insert stackmap instruction
                pos.ins().stackmap(&live_value_list);
            }
        }
    }
}
