//! A simple GVN pass.

//use entity_map::EntityMap;
use flowgraph::{ControlFlowGraph, BasicBlock};
use ir::{Ebb, Inst, Function, Layout, ProgramOrder};
//use packed_option::PackedOption;

//use std::cmp::Ordering;

/// Perform simple GVN on `func`.
///
pub fn do_simple_gvn(func: &mut Function, cfg: &mut ControlFlowGraph) {
}
