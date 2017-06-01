/// The loop analysis detects loops in the CFG that have an identified header block.
///
/// The characteristic property of a loop header block is that it dominates all the other blocks
/// of the loop.

use ir::{Function, Ebb, Layout, Loop};
use flowgraph::{ControlFlowGraph, EbbSet};
use dominator_tree::DominatorTree;
use entity_map::{EntityMap, PrimaryEntityData};
use packed_option::PackedOption;
use std::collections::HashSet;

pub struct LoopAnalysis {
    loops: EntityMap<Loop, LoopData>,
    ebbs: EntityMap<Ebb, PackedOption<Loop>>,
}

struct LoopData {
    header: Ebb,
    parent: PackedOption<Loop>,
}

impl PrimaryEntityData for LoopData {}

impl LoopAnalysis {
    pub fn compute(&mut self, func: &Function, cfg: &ControlFlowGraph, domtree: &DominatorTree) {
        self.loops.clear();
        self.ebbs.clear();
        self.ebbs.resize(func.dfg.num_ebbs());
        let connected_components: Vec<EbbSet> = cfg.tarjan_scc();
        // Each component potentially contains a loop. This technique detects only the outer-most
        // loops.
        // TODO: detect inner-most loops by applying Tarjan's algorithm recursively.
        for (header, component) in
            connected_components
                .into_iter()
                .filter_map(|component| {
                    match determine_loop_header(&component, &func.layout, cfg, domtree) {
                        None => None, // There is no clear loop header, we do not optimize
                        Some(header) => Some((header, component)),
                    }
                }) {
            let new_loop = &self.loops
                                .push(LoopData {
                                          header: header,
                                          parent: PackedOption::default(),
                                      });
            for ebb in component.values() {
                self.ebbs[*ebb] = PackedOption::from(*new_loop);
            }
        }
    }

    pub fn new() -> LoopAnalysis {
        LoopAnalysis {
            loops: EntityMap::new(),
            ebbs: EntityMap::new(),
        }
    }

    pub fn get_loops(&self) -> Vec<Loop> {
        self.loops.keys().collect()
    }

    pub fn get_loop_header(&self, lp: Loop) -> Ebb {
        self.loops[lp].header
    }

    pub fn get_loop_parent(&self, lp: Loop) -> Option<Loop> {
        self.loops[lp].parent.expand()
    }

    /// Determine if an Ebb belongs to a loop by running a finger along the loop tree.
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

// Check for the presence of a loop in a strongly connected component and returns an eventual
// loop header.
fn determine_loop_header(component: &EbbSet,
                         layout: &Layout,
                         cfg: &ControlFlowGraph,
                         domtree: &DominatorTree)
                         -> Option<Ebb> {
    let mut result = None;
    if component.len() == 1 {
        // We just have to check if this EBB has a back edge to itself.
        for ebb in component {
            // This loop only runs once (one element in the set).
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
        // We search for the max dominator EBB and then check if it actually dominates
        // the others.
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
