//! A Loop Invariant Code Motion optimization pass

use ir::{Function, Ebb, Inst, Value, Cursor, CursorBase, Type, InstBuilder, Layout, ValueDef};
use flowgraph::ControlFlowGraph;
use entity_list::{ListPool, EntityList};
use dominator_tree::DominatorTree;
use loop_analysis::{LoopAnalysis, Loop};

enum DefPosition {
    OutsideLoop(),
    ParentLoop(Loop),
    InsideCurrentLoop(),
}

/// Performs the LICM pass by detecting loops within the CFG and moving
/// loop-invariant instructions out of them.
/// Changes the CFG and domtree in-place during the operation.
pub fn do_licm(func: &mut Function,
               cfg: &mut ControlFlowGraph,
               domtree: &mut DominatorTree,
               loop_analysis: &mut LoopAnalysis) {
    loop_analysis.compute(func, cfg, domtree);
    let mut inst_pool = ListPool::new();
    let mut insts_rpo = EntityList::new();
    // Because Rust doesn't allow us to modify data structures that we are iterating upon, we
    // first have to do a pass to list all the instruction susceptible to be hoisted.
    for ebb in domtree.cfg_postorder().iter().rev() {
        for inst in func.layout.ebb_insts(*ebb) {
            match loop_analysis.base_loop_inst(inst, &func.layout) {
                None => (),
                Some(_) => {
                    if func.dfg.has_results(inst) {
                        insts_rpo.push(inst, &mut inst_pool);
                    }
                }
            }
        }
    }
    // We iterate over all the instructions in reverse postorder. If the instruction is part of a
    // loop, then we look at its arguments: if they are not defined inside the same loop, then the
    // instruction is loop-invariant and we hoist it. We don't have to test for child loop because
    // of the reverse postorder and the immediate hoisting guarantees that later instructions that
    // use its result value can also be hoisted.
    for &inst in insts_rpo.as_slice(&inst_pool) {
        // First which loop is the instruction in
        let inst_loop = match loop_analysis.base_loop_inst(inst, &func.layout) {
            None => continue,
            Some(lp) => lp,
        };
        // First we find where is the closest definition of one of the instruction's arguments
        let closest_def_loop = closest_def_position(func, inst, inst_loop, loop_analysis);
        // If all the instruction's argument are defined outside the loop then this instruction is
        // loop-invariant
        let destination_loop = match closest_def_loop {
            // If all the arguments are defined outside any loops we move the instruction just
            // before the outermost loop containing inst_loop.
            DefPosition::OutsideLoop() => loop_analysis.outermost_loop(inst_loop, None),
            // In this case we move just before the loop which would be a direct child of lp
            DefPosition::ParentLoop(lp) => loop_analysis.outermost_loop(inst_loop, Some(lp)),
            // If an argument is defined inside the current loop, we abort because the instruction
            // is not loop-invariant.
            DefPosition::InsideCurrentLoop() => continue,
        };
        match has_pre_header(&func.layout,
                             cfg,
                             domtree,
                             loop_analysis.loop_header(destination_loop)) {
            None => {
                let loop_header = loop_analysis.loop_header(destination_loop);
                let (pre_header, jump_inst) = create_pre_header(loop_header, func, cfg, domtree);
                loop_analysis.recompute_loop_preheader(pre_header, loop_header);
                domtree.recompute_loop_preheader(loop_header, pre_header, jump_inst);
                {
                    let mut cur = Cursor::new(&mut func.layout);
                    cur.goto_inst(inst);
                    cur.remove_inst();
                    cur.goto_inst(jump_inst);
                    cur.insert_inst(inst);
                }
            }
            // If there is a natural pre-header we insert new instructions just before the
            // related jumping instruction (which is not necessarily at the end).
            Some((_, last_jump_inst)) => {
                let mut cur = Cursor::new(&mut func.layout);
                cur.goto_inst(inst);
                cur.remove_inst();
                cur.goto_inst(last_jump_inst);
                cur.insert_inst(inst);
            }
        };
    }
}

// Given a instruction and the inner-most loop it is part of, determine from the positions of its
// arguments the closest definition of one of the instruction's argument. This closest position
// can be:
// - outside any loop;
// - inside the `inst_loop`, meaning that the instruction is not loop-invariant;
// - inside a parent loop which is not `inst_loop`.
fn closest_def_position(func: &Function,
                        inst: Inst,
                        inst_loop: Loop,
                        loop_analysis: &LoopAnalysis)
                        -> DefPosition {
    func.dfg
        .inst_args(inst)
        .into_iter()
        .fold(DefPosition::OutsideLoop(), |acc, arg| {
            let new_def_position = match func.dfg.value_def(*arg) {
                ValueDef::Arg(ebb_def, _) => {
                    match loop_analysis.base_loop_ebb(ebb_def) {
                        None => DefPosition::OutsideLoop(),
                        Some(loop_def) => {
                            if inst_loop != loop_def {
                                DefPosition::ParentLoop(loop_def)
                            } else {
                                DefPosition::InsideCurrentLoop()
                            }
                        }
                    }
                }
                ValueDef::Res(inst_def, _) => {
                    match loop_analysis.base_loop_inst(inst_def, &func.layout) {
                        None => DefPosition::OutsideLoop(),
                        Some(loop_def) => {
                            if inst_loop != loop_def {
                                DefPosition::ParentLoop(loop_def)
                            } else {
                                DefPosition::InsideCurrentLoop()
                            }
                        }
                    }
                }
            };
            // We now find some sort of minimum between `acc` and `new_def_position`.
            match acc {
                DefPosition::InsideCurrentLoop() => DefPosition::InsideCurrentLoop(),
                DefPosition::OutsideLoop() => new_def_position,
                DefPosition::ParentLoop(old_lp) => {
                    match new_def_position {
                        DefPosition::InsideCurrentLoop() => DefPosition::InsideCurrentLoop(),
                        DefPosition::OutsideLoop() => acc,
                        DefPosition::ParentLoop(new_lp) => {
                            if loop_analysis.is_child_loop(new_lp, old_lp) {
                                DefPosition::ParentLoop(new_lp)
                            } else {
                                DefPosition::ParentLoop(old_lp)
                            }
                        }
                    }
                }
            }
        })
}

// Insert a pre-header before the header, modifying the function layout and CFG to reflect it.
// A jump instruction to the header is placed at the end of the pre-header.
fn create_pre_header(header: Ebb,
                     func: &mut Function,
                     cfg: &mut ControlFlowGraph,
                     domtree: &DominatorTree)
                     -> (Ebb, Inst) {
    // TODO: find a way not to allocate all these Vec
    let header_args_values: Vec<Value> = func.dfg.ebb_args(header).into_iter().cloned().collect();
    let header_args_types: Vec<Type> = header_args_values
        .clone()
        .into_iter()
        .map(|val| func.dfg.value_type(val))
        .collect();
    let pre_header = func.dfg.make_ebb();
    let mut pre_header_args_value: Vec<Value> = Vec::new();
    for typ in header_args_types {
        pre_header_args_value.push(func.dfg.append_ebb_arg(pre_header, typ));
    }
    for (pred_ebb, last_inst) in
        cfg.get_predecessors(header)
            .iter()
            .cloned()
            .collect::<Vec<(Ebb, Inst)>>() {
        // We only follow normal edges (not the back edges)
        if !domtree.dominates(header, last_inst, &func.layout) {
            change_branch_jump_destination(last_inst, pre_header, func);
            cfg.recompute_ebb(func, pred_ebb);
        }
    }
    let jump_inst = {
        let mut pos = Cursor::new(&mut func.layout);
        pos.goto_top(header);
        // Inserts the pre-header at the right place in the layout.
        pos.insert_ebb(pre_header);
        pos.next_inst();
        func.dfg
            .ins(&mut pos)
            .jump(header, pre_header_args_value.as_slice())
    };
    cfg.recompute_ebb(func, pre_header);
    (pre_header, jump_inst)
}

// Detects if a loop header has a natural pre-header.
//
// A loop header has a pre-header if there is only one predecessor that the header doesn't
// dominate.
// Returns the pre-header Ebb and the instruction jumping to  the header.
fn has_pre_header(layout: &Layout,
                  cfg: &ControlFlowGraph,
                  domtree: &DominatorTree,
                  header: Ebb)
                  -> Option<(Ebb, Inst)> {
    let mut result = None;
    let mut found = false;
    for &(pred_ebb, last_inst) in cfg.get_predecessors(header) {
        // We only count normal edges (not the back edges)
        if !domtree.dominates(header, last_inst, layout) {
            if found {
                // We have already found one, there are more than one
                return None;
            } else {
                result = Some((pred_ebb, last_inst));
                found = true;
            }
        }
    }
    result
}


// Change the destination of a jump or branch instruction. Does nothing if called with a non-jump
// or non-branch instruction.
fn change_branch_jump_destination(inst: Inst, new_ebb: Ebb, func: &mut Function) {
    match func.dfg[inst].branch_destination_mut() {
        None => (),
        Some(instruction_dest) => *instruction_dest = new_ebb,
    }
}
