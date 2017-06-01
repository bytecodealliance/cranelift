//! A Loop Invariant Code Motion optimization pass

use ir::{Function, Ebb, Inst, Loop, Value, Cursor, Type, InstBuilder};
use ir::InstructionData::{Jump, Branch, BranchIcmp};
use flowgraph::ControlFlowGraph;
use std::collections::HashSet;
use dominator_tree::DominatorTree;
use entity_list::{EntityList, ListPool};
use loop_analysis::LoopAnalysis;

/// Performs the LICM pass by detecting loops within the CFG and moving
/// loop-invariant instructions out of them.
/// Changes the CFG and domtree in-place during the operation.
pub fn do_licm(func: &mut Function,
               cfg: &mut ControlFlowGraph,
               domtree: &mut DominatorTree,
               loop_analysis: &mut LoopAnalysis) {
    loop_analysis.compute(func, cfg, domtree);
    for lp in loop_analysis.get_loops() {
        // For each loop that we want to optimize we determine the set of loop-invariant
        // instructions
        let invariant_inst = remove_loop_invariant_instructions(lp, func, cfg, loop_analysis);
        // Then we create the loop's pre-header and fill it with the invariant instructions
        // Then we remove the invariant instructions from the loop body
        if invariant_inst.len() > 0 {
            let pre_header = create_pre_header(loop_analysis.get_loop_header(lp).clone(),
                                               func,
                                               cfg,
                                               domtree);
            let mut pos = Cursor::new(&mut func.layout);
            pos.goto_top(pre_header);
            pos.next_inst();
            for inst in invariant_inst.iter() {
                pos.insert_inst(inst.clone());
            }
        }
    }
    // We have to recompute the domtree to account for the changes
    cfg.compute(func);
    domtree.compute(func, cfg);
}

// Insert a pre-header before the header, modifying the function layout and CFG to reflect it.
// A jump instruction to the header is placed at the end of the pre-header.
fn create_pre_header(header: Ebb,
                     func: &mut Function,
                     cfg: &mut ControlFlowGraph,
                     domtree: &DominatorTree)
                     -> Ebb {
    let pool = &mut ListPool::<Value>::new();
    let header_args_values: Vec<Value> = func.dfg
        .ebb_args(header)
        .into_iter()
        .map(|val| *val)
        .collect();
    let header_args_types: Vec<Type> = header_args_values
        .clone()
        .into_iter()
        .map(|val| func.dfg.value_type(val))
        .collect();
    let pre_header = func.dfg.make_ebb();
    let mut pre_header_args_value: EntityList<Value> = EntityList::new();
    for typ in header_args_types {
        pre_header_args_value.push(func.dfg.append_ebb_arg(pre_header, typ), pool);
    }
    for &(_, last_inst) in cfg.get_predecessors(header) {
        // We only follow normal edges (not the back edges)
        if !domtree.ebb_dominates(header.clone(), last_inst, &func.layout) {
            change_branch_jump_destination(last_inst, pre_header, func);
        }
    }
    {
        let mut pos = Cursor::new(&mut func.layout);
        pos.goto_top(header);
        // Inserts the pre-header at the right place in the layout.
        pos.insert_ebb(pre_header);
        pos.next_inst();
        func.dfg
            .ins(&mut pos)
            .jump(header, pre_header_args_value.as_slice(pool));
    }
    pre_header
}


// Change the destination of a jump or branch instruction. Panics if called with a non-jump
// or non-branch instruction.
fn change_branch_jump_destination(inst: Inst, new_ebb: Ebb, func: &mut Function) {
    let new_inst_data = match func.dfg[inst].clone() {
        Jump {
            opcode,
            destination: _,
            args,
        } => {
            Jump {
                opcode,
                destination: new_ebb,
                args,
            }
        }
        Branch {
            opcode,
            destination: _,
            args,
        } => {
            Branch {
                opcode,
                destination: new_ebb,
                args,
            }
        }
        BranchIcmp {
            opcode,
            destination: _,
            cond,
            args,
        } => {
            BranchIcmp {
                opcode,
                destination: new_ebb,
                cond,
                args,
            }
        }
        _ => {
            assert!(false);
            // For some reason the compiler wants a type placeholder here.
            func.dfg[inst].clone()
        }
    };
    // We replace the last jump to point to the new pre-header.
    func.dfg[inst] = new_inst_data
}

// Traverses a loop in reverse post-order from a header EBB and identify lopp-invariant
// instructions. Theseloop-invariant instructions are then removed from the code and returned
// (in reverse post-order) for later use.
fn remove_loop_invariant_instructions(lp: Loop,
                                      func: &mut Function,
                                      cfg: &ControlFlowGraph,
                                      loop_analysis: &LoopAnalysis)
                                      -> Vec<Inst> {
    let mut loop_values: HashSet<Value> = HashSet::new();
    let mut invariant_inst: Vec<Inst> = Vec::new();
    let mut pos = Cursor::new(&mut func.layout);
    // We traverse the loop EBB in reverse post-order.
    for ebb in loop_analysis.postorder_ebbs_loop(cfg, lp).iter().rev() {
        // Arguments of the EBB are loop values
        for val in func.dfg.ebb_args(*ebb) {
            loop_values.insert(val.clone());
        }
        pos.goto_top(*ebb);
        while let Some(inst) = pos.next_inst() {
            if func.dfg
                   .inst_args(inst)
                   .into_iter()
                   .all(|arg| !loop_values.contains(arg)) {
                // If all the instruction's argument are defined outside the loop
                // then this instruction is loop-invariant
                invariant_inst.push(inst);
                // We remove it from the loop
                pos.remove_inst();
                pos.prev_inst();
            } else {
                // If the instruction is not loop-invariant we push its results in the set of
                // loop values
                for out in func.dfg.inst_results(inst) {
                    loop_values.insert(out.clone());
                }
            }
        }
    }
    invariant_inst
}
