//! A loop analysis represented as mappings of loops to their header Ebb
//! and parent in the loop tree.

use dominator_tree::DominatorTree;
use entity_map::{EntityMap, PrimaryEntityData, Keys};
use flowgraph::ControlFlowGraph;
use ir::{Function, Ebb, Layout, Cursor, InstBuilder};
use ir::entities::Inst;
use packed_option::PackedOption;

/// A opaque reference to a code loop.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loop(u32);
entity_impl!(Loop, "loop");

/// Loop tree information for a single function.
///
/// Loops are referenced by the Loop object, and for each loop you can access its header EBB,
/// its eventual parent in the loop tree and all the EBB belonging to the loop.
pub struct LoopAnalysis {
    loops: EntityMap<Loop, LoopData>,
    ebb_loop_map: EntityMap<Ebb, EbbLoopData>,
}

struct LoopData {
    header: Ebb,
    parent: PackedOption<Loop>,
}

impl PrimaryEntityData for LoopData {}

impl LoopData {
    /// Creates a `LoopData` object with the loop header and its eventual parent in the loop tree.
    fn new(header: Ebb, parent: Option<Loop>) -> LoopData {
        LoopData {
            header: header,
            parent: parent.into(),
        }
    }
}

/// If an `Ebb` is part of a loop, then we record two things: the id of the loop it's part of
/// and the last instruction in the `Ebb` pertaining to the loop. If the `Ebb` is part of multiple
/// loops, then we make sure by splitting the `Ebb` that it is part of at most two loops, one being
/// the direct child of the other, and the parent loop should contain all the `Ebb` up to its
/// terminator instruction.
#[derive(Clone,Debug)]
enum EbbLoopData {
    NoLoop(),
    Loop { loop_id: Loop, last_inst: Inst },
}

impl PrimaryEntityData for EbbLoopData {}

impl Default for EbbLoopData {
    fn default() -> EbbLoopData {
        EbbLoopData::NoLoop()
    }
}

/// Methods for querying the loop analysis.
impl LoopAnalysis {
    /// Allocate a new blank loop analysis struct. Use `compute` to compute the loop analysis for
    /// a function.
    pub fn new() -> LoopAnalysis {
        LoopAnalysis {
            loops: EntityMap::new(),
            ebb_loop_map: EntityMap::new(),
        }
    }

    /// Returns all the loops contained in a function.
    pub fn loops(&self) -> Keys<Loop> {
        self.loops.keys()
    }

    /// Returns the header EBB of a particular loop.
    ///
    /// The characteristic property of a loop header block is that it dominates some of its
    /// predecessors.
    pub fn loop_header(&self, lp: Loop) -> Ebb {
        self.loops[lp].header
    }

    /// Return the eventual parent of a loop in the loop tree.
    pub fn loop_parent(&self, lp: Loop) -> Option<Loop> {
        self.loops[lp].parent.expand()
    }

    /// Determine if an `Ebb` belongs to a loop by running a finger along the loop tree.
    ///
    /// Returns `true` if `ebb` is in loop `lp`.
    pub fn is_in_loop(&self, ebb: Ebb, lp: Loop) -> bool {
        match self.ebb_loop_map[ebb] {
            EbbLoopData::NoLoop() => false,
            EbbLoopData::Loop { loop_id, .. } => self.is_child_loop(loop_id, lp),
        }
    }

    /// Determine which region of an `Ebb` belongs to a particular loop.
    ///
    /// If `ebb` belongs to `lp`, it returns `None` if the whole `ebb` belongs to `lp` or
    /// `Some(inst)` where `inst` is the last instruction of `ebb` to belong to `lp`.
    pub fn last_loop_instruction(&self, ebb: Ebb, lp: Loop) -> Result<Option<Inst>, ()> {
        match self.ebb_loop_map[ebb] {
            EbbLoopData::NoLoop() => Err(()),
            EbbLoopData::Loop { loop_id, last_inst } => {
                if lp == loop_id {
                    Ok(Some(last_inst))
                } else if self.is_child_loop(loop_id, lp) {
                    Ok(None)
                } else {
                    Err(())
                }
            }
        }
    }

    /// Determines if a loop is contained in another loop.
    ///
    /// `is_child_loop(child,parent)` returns `true` if and only if `child` is a child loop of
    /// `parent` (or `child == parent`).
    pub fn is_child_loop(&self, child: Loop, parent: Loop) -> bool {
        let mut finger = Some(child);
        while let Some(finger_loop) = finger {
            if finger_loop == parent {
                return true;
            }
            finger = self.loop_parent(finger_loop);
        }
        false
    }
}

impl LoopAnalysis {
    /// Detects the loops in a function. Needs the control flow graph and the dominator tree.
    pub fn compute(&mut self,
                   func: &mut Function,
                   cfg: &mut ControlFlowGraph,
                   domtree: &mut DominatorTree) {
        self.loops.clear();
        self.ebb_loop_map.clear();
        self.ebb_loop_map.resize(func.dfg.num_ebbs());
        self.find_loop_headers(cfg, domtree, &func.layout);
        self.discover_loop_blocks(cfg, domtree, func);
    }

    // Traverses the CFG in reverse postorder and create a loop object for every EBB having a
    // back edge.
    fn find_loop_headers(&mut self,
                         cfg: &ControlFlowGraph,
                         domtree: &DominatorTree,
                         layout: &Layout) {
        // We traverse the CFG in reverse postorder
        for &ebb in domtree.cfg_postorder().iter().rev() {
            for &(_, pred_inst) in cfg.get_predecessors(ebb) {
                // If the ebb dominates one of its predecessors it is a back edge
                if domtree.dominates(ebb, pred_inst, layout) {
                    // This ebb is a loop header, so we create its associated loop
                    self.loops.push(LoopData::new(ebb, None));
                    break;
                    // We break because we only need one back edge to identify a loop header.
                }
            }
        }
    }

    // Intended to be called after `find_loop_headers`. For each detected loop header,
    // discovers all the ebb belonging to the loop and its inner loops. After a call to this
    // function, the loop tree is fully constructed.
    fn discover_loop_blocks(&mut self,
                            cfg: &mut ControlFlowGraph,
                            domtree: &mut DominatorTree,
                            func: &mut Function) {
        let mut stack: Vec<(Ebb, Inst)> = Vec::new();
        // We handle each loop header in reverse order, corresponding to a pesudo postorder
        // traversal of the graph.
        for lp in self.loops().rev() {
            for &(pred, pred_inst) in cfg.get_predecessors(self.loops[lp].header) {
                // We follow the back edges only
                if domtree.dominates(self.loops[lp].header, pred_inst, &func.layout) {
                    stack.push((pred, pred_inst));
                }
            }
            // Then we proceed to discover loop blocks by doing a "reverse" DFS
            while let Some((ebb, loop_edge_inst)) = stack.pop() {
                let continue_dfs: Option<Ebb>;
                match self.ebb_loop_map[ebb] {
                    EbbLoopData::NoLoop() => {
                        // The ebb hasn't been visited yet, we tag it as part of the loop
                        self.ebb_loop_map[ebb] = EbbLoopData::Loop {
                            loop_id: lp,
                            last_inst: loop_edge_inst,
                        };
                        // We stop the DFS if the block is the header of the loop
                        if ebb != self.loops[lp].header {
                            continue_dfs = Some(ebb);
                        } else {
                            continue_dfs = None;
                        }
                    }
                    EbbLoopData::Loop {
                        loop_id,
                        last_inst: _,
                    } => {
                        // So here the ebb we're visiting is already tagged as being part of a loop
                        // But since we're considering the loop headers in postorder, that loop
                        // can only be an inner loop of the loop we're considering.
                        // If this is the first time we reach this inner loop, we declare our loop
                        // the parent loop and continue the DFS at the header block of the inner
                        // loop.
                        // If we have already visited this inner loop, we can stop the DFS here.
                        let mut current_loop = loop_id;
                        let mut current_loop_parent_option = self.loops[current_loop].parent;
                        while let Some(current_loop_parent) = current_loop_parent_option.expand() {
                            if current_loop_parent == lp {
                                // We have encounterd lp so we stop (already visited)
                                break;
                            } else {
                                current_loop = current_loop_parent;
                                // We lookup the parent loop
                                current_loop_parent_option = self.loops[current_loop].parent;
                            }
                        }
                        // Now current_loop_parent is either:
                        // - None and current_loop is an new inner loop of lp
                        // - Some(...) and the initial node loop_id was a known inner loop of lp,
                        //   in this case we have already visited it and we stop the DFS
                        match current_loop_parent_option.expand() {
                            Some(_) => continue_dfs = None,
                            None => {
                                let inner_loop = current_loop;
                                let inner_loop_header = self.loops[inner_loop].header;
                                if inner_loop == lp {
                                    // If the inner loop is lp it means that we have reached a
                                    // a block that has been previously tagged as part of lp so
                                    // it has been visited by the DFS so we stop
                                    continue_dfs = None
                                } else {
                                    // Else we proceed to mark the inner loop as child of lp
                                    // And continue the DFS with the inner loop's header
                                    self.loops[inner_loop].parent = lp.into();
                                    continue_dfs = Some(inner_loop_header);
                                    // Here we perform a modification that's not related to
                                    // the loop discovery algorithm but rather to the way we
                                    // store the loop information. Indeed, for each Ebb we record
                                    // the loop its part of and the last inst in this loop
                                    // But here the Ebb is part of at least two loops, the child
                                    // loop and the parent loop . So two cases arise:
                                    // - either the entire ebb is part of the parent loop and this
                                    // is fine;
                                    // - either only part of the ebb is part of the parent loop and
                                    // in that case we can't store the information of where does
                                    // the parent loop stops.
                                    // In the second case, we proceed to split the Ebb just before
                                    // the parent's loop branch instruction (which is not a
                                    // terminator instruction) so that now the original ebb is
                                    // entirely in the parent loop, and the one we created by
                                    // splitting is part of only one loop, the parent loop.
                                    if func.layout
                                           .last_inst(ebb)
                                           .map_or(false, |ebb_last_inst| {
                                        ebb_last_inst != loop_edge_inst
                                    }) {
                                        // This handles the second case
                                        self.split_ebb_containing_two_loops(ebb,
                                                                            loop_edge_inst,
                                                                            lp,
                                                                            func,
                                                                            domtree,
                                                                            cfg);
                                    }
                                }
                            }
                        }
                    }
                }
                // Now we have handled the popped Ebb and need to continue the DFS by adding the
                // predecessors of that Ebb
                if let Some(continue_dfs) = continue_dfs {
                    for &(pre, pre_inst) in cfg.get_predecessors(continue_dfs) {
                        stack.push((pre, pre_inst))
                    }
                }
            }
        }
    }

    // We are in the case where `ebb` belongs partially to two different loops, the child and
    // the parent. `lp` here is the parent loop and we create a new Ebb so that `ebb` belongs
    // in its entirety to the parent loop.
    // We also update the cfd and domtree to reflect that.
    fn split_ebb_containing_two_loops(&mut self,
                                      ebb: Ebb,
                                      split_inst: Inst,
                                      lp: Loop,
                                      func: &mut Function,
                                      domtree: &mut DominatorTree,
                                      cfg: &mut ControlFlowGraph) {
        let new_ebb = func.dfg.make_ebb();
        let real_split_inst = {
            let mut cur = Cursor::new(&mut func.layout);
            cur.goto_inst(split_inst);
            cur.next_inst()
                .expect("you cannot split at the last instruction")
        };
        func.layout.split_ebb(new_ebb, real_split_inst);
        let middle_jump_inst = {
            let cur = &mut Cursor::new(&mut func.layout);
            cur.goto_bottom(ebb);
            func.dfg.ins(cur).jump(new_ebb, &[])
        };
        *self.ebb_loop_map.ensure(new_ebb) = EbbLoopData::Loop {
            loop_id: lp,
            last_inst: split_inst,
        };
        cfg.recompute_ebb(func, ebb);
        cfg.recompute_ebb(func, new_ebb);
        domtree.recompute_split_ebb(ebb, new_ebb, middle_jump_inst);
    }
}

#[cfg(test)]
mod test {

    use ir::{Function, InstBuilder, Cursor, CursorBase, types};
    use loop_analysis::{Loop, LoopAnalysis};
    use flowgraph::ControlFlowGraph;
    use dominator_tree::DominatorTree;

    #[test]
    fn nested_loops_detection() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();
        let ebb3 = func.dfg.make_ebb();
        let cond = func.dfg.append_ebb_arg(ebb0, types::I32);

        {
            let dfg = &mut func.dfg;
            let cur = &mut Cursor::new(&mut func.layout);

            cur.insert_ebb(ebb0);
            dfg.ins(cur).jump(ebb1, &[]);

            cur.insert_ebb(ebb1);
            dfg.ins(cur).jump(ebb2, &[]);

            cur.insert_ebb(ebb2);
            dfg.ins(cur).brnz(cond, ebb1, &[]);
            dfg.ins(cur).jump(ebb3, &[]);

            cur.insert_ebb(ebb3);
            dfg.ins(cur).brnz(cond, ebb0, &[]);

        }

        let mut loop_analysis = LoopAnalysis::new();
        let mut cfg = ControlFlowGraph::new();
        let mut domtree = DominatorTree::new();
        cfg.compute(&func);
        domtree.compute(&func, &cfg);
        loop_analysis.compute(&mut func, &mut cfg, &mut domtree);

        let loops = loop_analysis.loops().collect::<Vec<Loop>>();
        assert_eq!(loops.len(), 2);
        assert_eq!(loop_analysis.loop_header(loops[0]), ebb0);
        assert_eq!(loop_analysis.loop_header(loops[1]), ebb1);
        assert_eq!(loop_analysis.loop_parent(loops[1]), Some(loops[0]));
        assert_eq!(loop_analysis.loop_parent(loops[0]), None);
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[1]), false);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb3, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[1]), false);
    }

    #[test]
    fn complex_loop_detection() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();
        let ebb3 = func.dfg.make_ebb();
        let ebb4 = func.dfg.make_ebb();
        let ebb5 = func.dfg.make_ebb();
        let cond = func.dfg.append_ebb_arg(ebb0, types::I32);

        {
            let dfg = &mut func.dfg;
            let cur = &mut Cursor::new(&mut func.layout);

            cur.insert_ebb(ebb0);
            dfg.ins(cur).brnz(cond, ebb1, &[]);
            dfg.ins(cur).jump(ebb3, &[]);

            cur.insert_ebb(ebb1);
            dfg.ins(cur).jump(ebb2, &[]);

            cur.insert_ebb(ebb2);
            dfg.ins(cur).brnz(cond, ebb1, &[]);
            dfg.ins(cur).jump(ebb5, &[]);

            cur.insert_ebb(ebb3);
            dfg.ins(cur).jump(ebb4, &[]);

            cur.insert_ebb(ebb4);
            dfg.ins(cur).brnz(cond, ebb3, &[]);
            dfg.ins(cur).jump(ebb5, &[]);

            cur.insert_ebb(ebb5);
            dfg.ins(cur).brnz(cond, ebb0, &[]);

        }

        let mut loop_analysis = LoopAnalysis::new();
        let mut cfg = ControlFlowGraph::new();
        let mut domtree = DominatorTree::new();
        cfg.compute(&func);
        domtree.compute(&func, &cfg);
        loop_analysis.compute(&mut func, &mut cfg, &mut domtree);

        let loops = loop_analysis.loops().collect::<Vec<Loop>>();
        assert_eq!(loops.len(), 3);
        assert_eq!(loop_analysis.loop_header(loops[0]), ebb0);
        assert_eq!(loop_analysis.loop_header(loops[1]), ebb1);
        assert_eq!(loop_analysis.loop_header(loops[2]), ebb3);
        assert_eq!(loop_analysis.loop_parent(loops[1]), Some(loops[0]));
        assert_eq!(loop_analysis.loop_parent(loops[2]), Some(loops[0]));
        assert_eq!(loop_analysis.loop_parent(loops[0]), None);
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb3, loops[2]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb4, loops[2]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb5, loops[0]), true);
    }

    #[test]
    fn ebb_splitting() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();
        let cond = func.dfg.append_ebb_arg(ebb0, types::I32);

        let (inst0, inst1) = {
            let dfg = &mut func.dfg;
            let cur = &mut Cursor::new(&mut func.layout);

            cur.insert_ebb(ebb0);
            dfg.ins(cur).jump(ebb1, &[]);

            cur.insert_ebb(ebb1);
            let inst0 = dfg.ins(cur).brnz(cond, ebb1, &[]);
            let inst1 = dfg.ins(cur).brnz(cond, ebb0, &[]);
            dfg.ins(cur).jump(ebb2, &[]);

            cur.insert_ebb(ebb2);
            dfg.ins(cur).return_(&[]);
            (inst0, inst1)
        };

        let mut loop_analysis = LoopAnalysis::new();
        let mut cfg = ControlFlowGraph::new();
        let mut domtree = DominatorTree::new();
        cfg.compute(&func);
        domtree.compute(&func, &cfg);
        loop_analysis.compute(&mut func, &mut cfg, &mut domtree);

        let loops = loop_analysis.loops().collect::<Vec<Loop>>();
        assert_eq!(loops.len(), 2);
        assert_eq!(loop_analysis.loop_header(loops[0]), ebb0);
        assert_eq!(loop_analysis.loop_header(loops[1]), ebb1);
        assert_eq!(loop_analysis.loop_parent(loops[1]), Some(loops[0]));
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[0]), false);
        assert_eq!(loop_analysis.is_in_loop(Ebb::with_number(3).unwrap(), loops[0]),
                   true);
        assert_eq!(loop_analysis
                       .last_loop_instruction(ebb1, loops[1])
                       .unwrap()
                       .unwrap(),
                   inst0);
        assert_eq!(loop_analysis
                       .last_loop_instruction(Ebb::with_number(3).unwrap(), loops[0])
                       .unwrap()
                       .unwrap(),
                   inst1)
    }
    #[test]
    fn ebb_splitting_complex() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();
        let ebb3 = func.dfg.make_ebb();
        let cond = func.dfg.append_ebb_arg(ebb0, types::I32);

        let (inst0, inst1) = {
            let dfg = &mut func.dfg;
            let cur = &mut Cursor::new(&mut func.layout);

            cur.insert_ebb(ebb0);
            dfg.ins(cur).jump(ebb1, &[]);

            cur.insert_ebb(ebb1);
            dfg.ins(cur).brnz(cond, ebb2, &[]);
            let inst0 = dfg.ins(cur).jump(ebb1, &[]);

            cur.insert_ebb(ebb2);
            dfg.ins(cur).brnz(cond, ebb3, &[]);
            let inst1 = dfg.ins(cur).jump(ebb0, &[]);

            cur.insert_ebb(ebb3);
            dfg.ins(cur).return_(&[]);
            (inst0, inst1)
        };

        let mut loop_analysis = LoopAnalysis::new();
        let mut cfg = ControlFlowGraph::new();
        let mut domtree = DominatorTree::new();
        cfg.compute(&func);
        domtree.compute(&func, &cfg);
        loop_analysis.compute(&mut func, &mut cfg, &mut domtree);
        let loops = loop_analysis.loops().collect::<Vec<Loop>>();
        assert_eq!(loops.len(), 2);
        assert_eq!(loop_analysis.loop_header(loops[0]), ebb0);
        assert_eq!(loop_analysis.loop_header(loops[1]), ebb1);
        assert_eq!(loop_analysis.loop_parent(loops[1]), Some(loops[0]));
        assert_eq!(loop_analysis.is_in_loop(ebb0, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[1]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb1, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[0]), true);
        assert_eq!(loop_analysis.is_in_loop(ebb2, loops[1]), false);
        assert_eq!(loop_analysis.is_in_loop(Ebb::with_number(4).unwrap(), loops[0]),
                   true);
        assert_eq!(loop_analysis
                       .last_loop_instruction(ebb1, loops[1])
                       .unwrap()
                       .unwrap(),
                   inst0);
        assert_eq!(loop_analysis
                       .last_loop_instruction(ebb2, loops[0])
                       .unwrap()
                       .unwrap(),
                   inst1)
    }
}
