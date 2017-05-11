//! A simple GVN pass.

use flowgraph::ControlFlowGraph;
use dominator_tree::DominatorTree;
use ir::{Cursor, DataFlowGraph, InstructionData, Function, Inst, Opcode};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

/// Test whether the given opcode is unsafe to even consider for GVN.
fn trivially_unsafe_for_gvn(opcode: Opcode) -> bool {
    opcode.is_call() || opcode.is_branch() || opcode.is_terminator() || opcode.is_return() ||
    opcode.can_trap()
}

#[derive(Debug)]
struct VisibleInst(InstructionData);

impl Hash for VisibleInst {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // TODO: Is a better way to do this? Should we just make InstructionData
        // implement Hash itself?
        use ir::instructions::InstructionData::*;
        match self.0 {
            Nullary { .. } => {}
            Unary { arg, .. } => arg.hash(state),
            Binary { args, .. } => args.hash(state),
            Ternary { args, .. } => args.hash(state),
            _ => {
                panic!("TODO: implement hashing for more variants.");
            }
        }
    }
}

impl PartialEq for VisibleInst {
    fn eq(&self, other: &VisibleInst) -> bool {
        if self.0.opcode() != other.0.opcode() {
            return false;
        }

        // TODO: Surely there is a better way to do this? Should we provide a
        // PartialEq implementation for InstructionData? Note that it isn't as
        // simple as just using the default implementation, since
        // InstructionData contains things like floating-point literals which
        // don't have Eq defined.
        use ir::instructions::InstructionData::*;
        match self.0 {
            Nullary { .. } => true,
            Unary { arg, .. } => {
                match other.0 {
                    Unary { arg: other_arg, .. } => arg == other_arg,
                    _ => panic!("we checked the opcode so this shouldn't happen"),
                }
            }
            Binary { args, .. } => {
                match other.0 {
                    Binary { args: other_args, .. } => args == other_args,
                    _ => panic!("we checked the opcode so this shouldn't happen"),
                }
            }
            Ternary { args, .. } => {
                match other.0 {
                    Ternary { args: other_args, .. } => args == other_args,
                    _ => panic!("we checked the opcode so this shouldn't happen"),
                }
            }
            _ => {
                // TODO: Implement eq for more variants.
                false
            }
        }
    }
}
impl Eq for VisibleInst {}

/// Replace the values of new_inst with the values of old_inst.
///
fn replace_values(dfg: &mut DataFlowGraph, new_inst: Inst, old_inst: Inst) {
    let mut result = 0;
    let new_results = dfg.detach_results(new_inst);
    while let Some(new_val) = new_results.get(result, &dfg.value_lists) {
        let old_val = dfg.inst_results(old_inst)[result];
        dfg.change_to_alias(new_val, old_val);
        result += 1;
    }
}

/// Perform simple GVN on `func`.
///
pub fn do_simple_gvn(func: &mut Function, cfg: &mut ControlFlowGraph) {
    let mut visible_values: HashMap<VisibleInst, Inst> = HashMap::new();

    let domtree = DominatorTree::with_function(func, &cfg);

    // Visit EBBs in a reverse post-order.
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

            let key = VisibleInst(func.dfg[inst].clone());
            let entry = visible_values.entry(key);
            use std::collections::hash_map::Entry::*;
            match entry {
                Occupied(mut entry) => {
                    if domtree.dominates(*entry.get(), inst, &pos.layout) {
                        // TODO: Delete inst, while we're here.
                        replace_values(&mut func.dfg, inst, *entry.get());
                    } else {
                        // The prior instruction doesn't dominate inst, so it
                        // won't dominate any subsequent instructions we'll
                        // visit, so just replace it.
                        *entry.get_mut() = inst;
                        continue;
                    }
                }
                Vacant(entry) => {
                    entry.insert(inst);
                }
            }
        }
    }
}
