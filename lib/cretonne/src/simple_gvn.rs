//! A simple GVN pass.

//use entity_map::EntityMap;
use flowgraph::{ControlFlowGraph, BasicBlock};
use ir::{Cursor, Ebb, Inst, Function, Layout, Opcode, ProgramOrder};
//use packed_option::PackedOption;

//use std::cmp::Ordering;

fn trivially_unsafe_for_gvn(opcode: Opcode) -> bool {
    opcode.is_call() || opcode.is_branch() || opcode.is_terminator() ||
    opcode.is_return() || opcode.can_trap()
}

/// Perform simple GVN on `func`.
///
pub fn do_simple_gvn(func: &mut Function, cfg: &mut ControlFlowGraph) {
    // Process EBBs in a reverse post-order. This minimizes the number of split instructions we
    // need.
    let mut postorder = cfg.postorder_ebbs();
    let mut pos = Cursor::new(&mut func.layout);

    while let Some(ebb) = postorder.pop() {
        pos.goto_top(ebb);

        while let Some(inst) = pos.next_inst() {
            let opcode = func.dfg[inst].opcode();
            if trivially_unsafe_for_gvn(opcode) {
                continue;
            }

            // TODO: Implement simple redundant-load elimination.
            if opcode.can_store() {
                continue;
            }
            if opcode.can_load() {
                continue;
            }

        }
    }
}
