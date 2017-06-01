//! TA loop analysis respresented as mappings of loops to their header and parent in the loop tree.


use ir::{Function, Ebb, Layout, Loop};
use flowgraph::ControlFlowGraph;
use dominator_tree::DominatorTree;
use entity_map::{EntityMap, PrimaryEntityData};
use packed_option::PackedOption;
use std::collections::HashSet;

/// Loop tree information for a single function.
///
/// Loops are referenced by the Loop object, and for each loop you can access its header EBB,
/// its eventual parent in the loop tree and all the EBB belonging to the loop.
pub struct LoopAnalysis {
    loops: EntityMap<Loop, LoopData>,
    ebbs: EntityMap<Ebb, PackedOption<Loop>>,
}

struct LoopData {
    header: Ebb,
    parent: PackedOption<Loop>,
}

impl PrimaryEntityData for LoopData {}

/// Methods for querying the loop analysis.
impl LoopAnalysis {
    /// Allocate a new blank loop analysis struct. Use `compute` to compute the loop analysis for
    /// a function.
    pub fn new() -> LoopAnalysis {
        LoopAnalysis {
            loops: EntityMap::new(),
            ebbs: EntityMap::new(),
        }
    }

    /// Returns all the loops contained in a function.
    pub fn get_loops(&self) -> Vec<Loop> {
        self.loops.keys().collect()
    }

    /// Returns the header EBB of a particular loop.
    ///
    /// The characteristic property of a loop header block is that it dominates some of its
    /// predecessors.
    pub fn get_loop_header(&self, lp: Loop) -> Ebb {
        self.loops[lp].header
    }

    /// Return the eventual parents of a loop in the loop tree.
    pub fn get_loop_parent(&self, lp: Loop) -> Option<Loop> {
        self.loops[lp].parent.expand()
    }

    /// Determine if an Ebb belongs to a loop by running a finger along the loop tree.
    ///
    /// Returns `true` if `ebb` is in loop `lp`.
    pub fn is_in_loop(&self, ebb: Ebb, lp: Loop) -> bool {
        let mut ebb_loop = self.ebbs[ebb].expand();
        while ebb_loop != None {
            if ebb_loop.unwrap() == lp {
                return true;
            } else {
                ebb_loop = self.get_loop_parent(ebb_loop.unwrap());
            }
        }
        false
    }

    /// Return ebbs from a loop in post-order, starting from an entry point in the block.
    pub fn postorder_ebbs_loop(&self, cfg: &ControlFlowGraph, lp: Loop) -> Vec<Ebb> {
        if self.loops.get(lp).is_none() {
            return Vec::new();
        }
        let mut grey = HashSet::new();
        let mut black = HashSet::new();
        let mut stack = vec![self.loops[lp].header.clone()];
        let mut postorder = Vec::new();

        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            if !grey.contains(&node) {
                // This is a white node. Mark it as gray.
                grey.insert(node);
                stack.push(node);
                // Get any children we've never seen before.
                for child in cfg.get_successors(node) {
                    if self.is_in_loop(child.clone(), lp) && !grey.contains(child) {
                        stack.push(child.clone());
                    }
                }
            } else if !black.contains(&node) {
                postorder.push(node.clone());
                black.insert(node.clone());
            }
        }
        postorder
    }
}

impl LoopAnalysis {
    /// Detects the loops in a function. Needs the control flow graph and the dominator tree.
    pub fn compute(&mut self, func: &Function, cfg: &ControlFlowGraph, domtree: &DominatorTree) {
        self.loops.clear();
        self.ebbs.clear();
        self.ebbs.resize(func.dfg.num_ebbs());
        self.find_loop_headers(cfg, domtree, &func.layout);
        self.discover_loop_blocks(cfg, domtree, &func.layout)
    }

    // Traverses the CFG in reverse postorder and create a loop object for every EBB having a
    // back edge.
    fn find_loop_headers(&mut self,
                         cfg: &ControlFlowGraph,
                         domtree: &DominatorTree,
                         layout: &Layout) {
        // We traverse the CFg in reverse postorder
        for ebb in cfg.postorder_ebbs().iter().rev() {
            for &(_, pred_inst) in cfg.get_predecessors(*ebb) {
                // If the ebb dominates one of its predecessors it is a back edge
                if domtree.ebb_dominates(ebb.clone(), pred_inst, layout) {
                    // This ebb is a loop header, so we create its associated loop
                    let lp = self.loops
                        .push(LoopData {
                                  header: *ebb,
                                  parent: PackedOption::from(None),
                              });
                    self.ebbs[*ebb] = PackedOption::from(lp);
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
                            cfg: &ControlFlowGraph,
                            domtree: &DominatorTree,
                            layout: &Layout) {
        let mut stack: Vec<Ebb> = Vec::new();
        // We handle each loop header in reverse order, corresponding to a pesudo postorder
        // traversal of the graph.
        for lp in self.get_loops().iter().rev() {
            for &(pred, pred_inst) in cfg.get_predecessors(self.loops[*lp].header) {
                // We follow the back edges
                if domtree.ebb_dominates(self.loops[*lp].header, pred_inst, layout) {
                    stack.push(pred);
                }
            }
            while !stack.is_empty() {
                let node = stack.pop().unwrap();
                let continue_dfs: Option<Ebb>;
                if self.ebbs[node].is_none() {
                    // The node hasn't been visited yet, we tag it as part of the loop
                    self.ebbs[node] = PackedOption::from(*lp);
                    continue_dfs = Some(node);
                } else {
                    // The node is part of a loop, which can be lp or an inner loop
                    let mut node_loop = self.ebbs[node].unwrap();
                    let mut node_loop_parent = self.loops[node_loop].parent;
                    while node_loop_parent.is_some() {
                        if node_loop_parent.unwrap() == *lp {
                            // We have encounterd lp so we stop (already visited)
                            break;
                        } else {
                            // We lookup the parent loop
                            node_loop = node_loop_parent.unwrap();
                            node_loop_parent = self.loops[node_loop].parent;
                        }
                    }
                    // Now node_loop_parent is either:
                    // - None and node_loop is an new inner loop of lp
                    // - Some(...) and the initial node_loop was a known inner loop of lp
                    match node_loop_parent.expand() {
                        Some(_) => continue_dfs = None,
                        None => {
                            if node_loop != *lp {
                                self.loops[node_loop].parent = PackedOption::from(*lp);
                                continue_dfs = Some(self.loops[node_loop].header)
                            } else {
                                // If lp is a one-block loop then we make sure we stop
                                continue_dfs = None
                            }
                        }
                    }
                }
                // Now we have handled the popped node and need to continue the DFS by adding the
                // predecessors of that node
                if continue_dfs != None {
                    for &(pred, _) in cfg.get_predecessors(continue_dfs.unwrap()) {
                        stack.push(pred)
                    }
                }
            }

        }
    }
}
