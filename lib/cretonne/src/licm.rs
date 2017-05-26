//! A Loop Invariant Code Motion optimization pass

use ir::{Function, Ebb, Layout, Inst, Value, Cursor, Type, InstBuilder};
use ir::InstructionData::{Jump, Branch, BranchIcmp};
use flowgraph::ControlFlowGraph;
use std::collections::HashSet;
use dominator_tree::DominatorTree;
use entity_list::{EntityList, ListPool};

/// Performs the LICM pass by detecting loops within the CFG and moving
/// loop-invariant instructions out of it.
/// Changes the CFG and domtrees so they need to be recomputed after.
pub fn do_licm(func: &mut Function, cfg: &mut ControlFlowGraph, domtree: &mut DominatorTree) {
    let connected_components: Vec<HashSet<Ebb>> = cfg.tarjan_scc();
    // Each component potentially contains a loop. This technique detects only the outer-most
    // loops.
    // TODO: detect inner-most loops by applying Tarjan's algorithm recursively.
    let loops: Vec<(Ebb, HashSet<Ebb>)> = connected_components
        .into_iter()
        .filter_map(|component| match determine_loop_header(&component,
                                                            &func.layout,
                                                            cfg,
                                                            domtree) {
                        None => None, // There is no clear loop header, we do not optimize
                        Some(header) => Some((header, component)),
                    })
        .collect();
    for (header, blocks) in loops {
        // For each loop that we want to optimize we determine the set of loop-invariant
        // instructions
        let invariant_inst = remove_loop_invariant_instructions(&header, &blocks, func, cfg);
        // Then we create the loop's pre-header and fill it with the invariant instructions
        // Then we remove the invariant instructions from the loop body
        if invariant_inst.len() > 0 {
            let pre_header = create_pre_header(header, func, cfg, domtree);
            let mut pos = Cursor::new(&mut func.layout);
            pos.goto_top(pre_header);
            pos.next_inst();
            for inst in invariant_inst.iter() {
                pos.insert_inst(inst.clone());
            }
        }
    }
    // We have to recompute the domtree to account for the changes
    domtree.compute(func, cfg);
}

// Insert a pre-header before the header, modifying the function layout and CFG to reflect it.
// A jump instruction to the header is placed at the end of the pre-header.s
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
    let (normal_edges, _) = split_normal_back_edges(header, cfg, domtree, &func.layout);
    for (_, last_inst) in normal_edges {
        change_branch_jump_destination(last_inst, pre_header, pre_header_args_value.clone(), func);
    }
    {
        let mut pos = Cursor::new(&mut func.layout);
        pos.goto_top(header);
        // Inserts the pre-header at the right place in the layout;
        pos.insert_ebb(pre_header);
        pos.next_inst();
        func.dfg
            .ins(&mut pos)
            .jump(header, pre_header_args_value.as_slice(pool));
    }
    cfg.compute(func);
    pre_header
}


// Change the destination of a jump or branch instruction. Panics if called with a non-jump
// or non-branch instruction.
fn change_branch_jump_destination(inst: Inst,
                                  new_ebb: Ebb,
                                  new_ebb_args_value: EntityList<Value>,
                                  func: &mut Function) {
    let new_inst_data = match func.dfg[inst].clone() {
        Jump {
            opcode,
            destination: _,
            args: _,
        } => {
            Jump {
                opcode,
                destination: new_ebb,
                args: new_ebb_args_value.clone(),
            }
        }
        Branch {
            opcode,
            destination: _,
            args: _,
        } => {
            Branch {
                opcode,
                destination: new_ebb,
                args: new_ebb_args_value.clone(),
            }
        }
        BranchIcmp {
            opcode,
            destination: _,
            cond,
            args: _,
        } => {
            BranchIcmp {
                opcode,
                destination: new_ebb,
                cond,
                args: new_ebb_args_value.clone(),
            }
        }
        _ => {
            assert!(false);
            // For some reason the compiler wants a type placeholder here.
            func.dfg[inst].clone()
        }
    };
    // We replace the last jump to point to the new pre-header
    func.dfg[inst] = new_inst_data
}

fn remove_loop_invariant_instructions(header: &Ebb,
                                      blocks: &HashSet<Ebb>,
                                      func: &mut Function,
                                      cfg: &ControlFlowGraph)
                                      -> Vec<Inst> {
    let mut loop_values: HashSet<Value> = HashSet::new();
    let mut invariant_inst: Vec<Inst> = Vec::new();
    let mut pos = Cursor::new(&mut func.layout);
    // We traverse the loop EBB in reverse post-order.
    for ebb in cfg.postorder_ebbs_block(header, blocks).iter().rev() {
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

// Check for the presence of a loop in a strongly connected component and returns an eventual loop
// header.
fn determine_loop_header(component: &HashSet<Ebb>,
                         layout: &Layout,
                         cfg: &ControlFlowGraph,
                         domtree: &DominatorTree)
                         -> Option<Ebb> {
    let mut result = None;
    if component.len() == 1 {
        // We just have to check if this EBB has a back edge to itself
        for ebb in component {
            // This loop only runs once (one element in the set)
            let predecessors = cfg.get_predecessors(ebb.clone());
            let mut has_backedge = false;
            for &(pred_ebb, _) in predecessors {
                if pred_ebb == *ebb {
                    has_backedge = true;
                    break;
                }
            }
            if has_backedge {
                result = Some(ebb.clone())
            }
        }
        result
    } else {
        // We search for the max dominator EBB and then check if it actually dominates the others
        for ebb_b in component {
            match result {
                None => result = Some(ebb_b.clone()),
                Some(ebb_a) => {
                    if domtree.ebb_ebb_dominates(ebb_b.clone(), ebb_a, layout) {
                        result = Some(ebb_b.clone())
                    }
                }
            }
        }
        match result {
            None => None,
            Some(result_ebb) => {
                for ebb in component {
                    if !domtree.ebb_ebb_dominates(result_ebb, ebb.clone(), layout) {
                        // There is no loop header that dominates all the other EBBs in the
                        // component, we give up LICM on this component.
                        return None;
                    }
                }
                result
            }
        }
    }
}

// Given a loop header, returns its predecessors but making the distinction between
// back edges and normal edges.
fn split_normal_back_edges(loop_header: Ebb,
                           cfg: &ControlFlowGraph,
                           domtree: &DominatorTree,
                           layout: &Layout)
                           -> (Vec<(Ebb, Inst)>, Vec<(Ebb, Inst)>) {
    let predecessors = cfg.get_predecessors(loop_header);
    let mut normal_edges = Vec::new();
    let mut back_edges = Vec::new();
    for &(ebb_pred, last_inst) in predecessors {
        if domtree.ebb_dominates(loop_header.clone(), last_inst, layout) {
            back_edges.push((ebb_pred, last_inst))
        } else {
            normal_edges.push((ebb_pred, last_inst))
        }
    }
    (normal_edges, back_edges)
}
