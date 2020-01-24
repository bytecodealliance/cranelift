//! A control flow graph represented as mappings of extended basic blocks to their predecessors
//! and successors.
//!
//! Successors are represented as extended basic blocks while predecessors are represented by basic
//! blocks. Basic blocks are denoted by tuples of block and branch/jump instructions. Each
//! predecessor tuple corresponds to the end of a basic block.
//!
//! ```c
//!     Block0:
//!         ...          ; beginning of basic block
//!
//!         ...
//!
//!         brz vx, Block1 ; end of basic block
//!
//!         ...          ; beginning of basic block
//!
//!         ...
//!
//!         jmp Block2     ; end of basic block
//! ```
//!
//! Here `Block1` and `Block2` would each have a single predecessor denoted as `(Block0, brz)`
//! and `(Block0, jmp Block2)` respectively.

use crate::bforest;
use crate::entity::SecondaryMap;
use crate::ir::instructions::BranchInfo;
use crate::ir::{Block, Function, Inst};
use crate::timing;
use core::mem;

/// A basic block denoted by its enclosing Block and last instruction.
#[derive(Debug, PartialEq, Eq)]
pub struct BlockPredecessor {
    /// Enclosing Block key.
    pub ebb: Block,
    /// Last instruction in the basic block.
    pub inst: Inst,
}

impl BlockPredecessor {
    /// Convenient method to construct new BlockPredecessor.
    pub fn new(ebb: Block, inst: Inst) -> Self {
        Self { ebb, inst }
    }
}

/// A container for the successors and predecessors of some Block.
#[derive(Clone, Default)]
struct CFGNode {
    /// Instructions that can branch or jump to this block.
    ///
    /// This maps branch instruction -> predecessor block which is redundant since the block containing
    /// the branch instruction is available from the `layout.inst_ebb()` method. We store the
    /// redundant information because:
    ///
    /// 1. Many `pred_iter()` consumers want the block anyway, so it is handily available.
    /// 2. The `invalidate_ebb_successors()` may be called *after* branches have been removed from
    ///    their block, but we still need to remove them form the old block predecessor map.
    ///
    /// The redundant block stored here is always consistent with the CFG successor lists, even after
    /// the IR has been edited.
    pub predecessors: bforest::Map<Inst, Block>,

    /// Set of blocks that are the targets of branches and jumps in this block.
    /// The set is ordered by block number, indicated by the `()` comparator type.
    pub successors: bforest::Set<Block>,
}

/// The Control Flow Graph maintains a mapping of ebbs to their predecessors
/// and successors where predecessors are basic blocks and successors are
/// extended basic blocks.
pub struct ControlFlowGraph {
    data: SecondaryMap<Block, CFGNode>,
    pred_forest: bforest::MapForest<Inst, Block>,
    succ_forest: bforest::SetForest<Block>,
    valid: bool,
}

impl ControlFlowGraph {
    /// Allocate a new blank control flow graph.
    pub fn new() -> Self {
        Self {
            data: SecondaryMap::new(),
            valid: false,
            pred_forest: bforest::MapForest::new(),
            succ_forest: bforest::SetForest::new(),
        }
    }

    /// Clear all data structures in this control flow graph.
    pub fn clear(&mut self) {
        self.data.clear();
        self.pred_forest.clear();
        self.succ_forest.clear();
        self.valid = false;
    }

    /// Allocate and compute the control flow graph for `func`.
    pub fn with_function(func: &Function) -> Self {
        let mut cfg = Self::new();
        cfg.compute(func);
        cfg
    }

    /// Compute the control flow graph of `func`.
    ///
    /// This will clear and overwrite any information already stored in this data structure.
    pub fn compute(&mut self, func: &Function) {
        let _tt = timing::flowgraph();
        self.clear();
        self.data.resize(func.dfg.num_ebbs());

        for ebb in &func.layout {
            self.compute_ebb(func, ebb);
        }

        self.valid = true;
    }

    fn compute_ebb(&mut self, func: &Function, ebb: Block) {
        for inst in func.layout.ebb_insts(ebb) {
            match func.dfg.analyze_branch(inst) {
                BranchInfo::SingleDest(dest, _) => {
                    self.add_edge(ebb, inst, dest);
                }
                BranchInfo::Table(jt, dest) => {
                    if let Some(dest) = dest {
                        self.add_edge(ebb, inst, dest);
                    }
                    for dest in func.jump_tables[jt].iter() {
                        self.add_edge(ebb, inst, *dest);
                    }
                }
                BranchInfo::NotABranch => {}
            }
        }
    }

    fn invalidate_ebb_successors(&mut self, ebb: Block) {
        // Temporarily take ownership because we need mutable access to self.data inside the loop.
        // Unfortunately borrowck cannot see that our mut accesses to predecessors don't alias
        // our iteration over successors.
        let mut successors = mem::replace(&mut self.data[ebb].successors, Default::default());
        for succ in successors.iter(&self.succ_forest) {
            self.data[succ]
                .predecessors
                .retain(&mut self.pred_forest, |_, &mut e| e != ebb);
        }
        successors.clear(&mut self.succ_forest);
    }

    /// Recompute the control flow graph of `ebb`.
    ///
    /// This is for use after modifying instructions within a specific block. It recomputes all edges
    /// from `ebb` while leaving edges to `ebb` intact. Its functionality a subset of that of the
    /// more expensive `compute`, and should be used when we know we don't need to recompute the CFG
    /// from scratch, but rather that our changes have been restricted to specific blocks.
    pub fn recompute_ebb(&mut self, func: &Function, ebb: Block) {
        debug_assert!(self.is_valid());
        self.invalidate_ebb_successors(ebb);
        self.compute_ebb(func, ebb);
    }

    fn add_edge(&mut self, from: Block, from_inst: Inst, to: Block) {
        self.data[from]
            .successors
            .insert(to, &mut self.succ_forest, &());
        self.data[to]
            .predecessors
            .insert(from_inst, from, &mut self.pred_forest, &());
    }

    /// Get an iterator over the CFG predecessors to `ebb`.
    pub fn pred_iter(&self, ebb: Block) -> PredIter {
        PredIter(self.data[ebb].predecessors.iter(&self.pred_forest))
    }

    /// Get an iterator over the CFG successors to `ebb`.
    pub fn succ_iter(&self, ebb: Block) -> SuccIter {
        debug_assert!(self.is_valid());
        self.data[ebb].successors.iter(&self.succ_forest)
    }

    /// Check if the CFG is in a valid state.
    ///
    /// Note that this doesn't perform any kind of validity checks. It simply checks if the
    /// `compute()` method has been called since the last `clear()`. It does not check that the
    /// CFG is consistent with the function.
    pub fn is_valid(&self) -> bool {
        self.valid
    }
}

/// An iterator over block predecessors. The iterator type is `BlockPredecessor`.
///
/// Each predecessor is an instruction that branches to the block.
pub struct PredIter<'a>(bforest::MapIter<'a, Inst, Block>);

impl<'a> Iterator for PredIter<'a> {
    type Item = BlockPredecessor;

    fn next(&mut self) -> Option<BlockPredecessor> {
        self.0.next().map(|(i, e)| BlockPredecessor::new(e, i))
    }
}

/// An iterator over block successors. The iterator type is `Block`.
pub type SuccIter<'a> = bforest::SetIter<'a, Block>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cursor::{Cursor, FuncCursor};
    use crate::ir::{types, Function, InstBuilder};
    use alloc::vec::Vec;

    #[test]
    fn empty() {
        let func = Function::new();
        ControlFlowGraph::with_function(&func);
    }

    #[test]
    fn no_predecessors() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();
        func.layout.append_ebb(ebb0);
        func.layout.append_ebb(ebb1);
        func.layout.append_ebb(ebb2);

        let cfg = ControlFlowGraph::with_function(&func);

        let mut fun_ebbs = func.layout.ebbs();
        for ebb in func.layout.ebbs() {
            assert_eq!(ebb, fun_ebbs.next().unwrap());
            assert_eq!(cfg.pred_iter(ebb).count(), 0);
            assert_eq!(cfg.succ_iter(ebb).count(), 0);
        }
    }

    #[test]
    fn branches_and_jumps() {
        let mut func = Function::new();
        let ebb0 = func.dfg.make_ebb();
        let cond = func.dfg.append_ebb_param(ebb0, types::I32);
        let ebb1 = func.dfg.make_ebb();
        let ebb2 = func.dfg.make_ebb();

        let br_ebb0_ebb2;
        let br_ebb1_ebb1;
        let jmp_ebb0_ebb1;
        let jmp_ebb1_ebb2;

        {
            let mut cur = FuncCursor::new(&mut func);

            cur.insert_ebb(ebb0);
            br_ebb0_ebb2 = cur.ins().brnz(cond, ebb2, &[]);
            jmp_ebb0_ebb1 = cur.ins().jump(ebb1, &[]);

            cur.insert_ebb(ebb1);
            br_ebb1_ebb1 = cur.ins().brnz(cond, ebb1, &[]);
            jmp_ebb1_ebb2 = cur.ins().jump(ebb2, &[]);

            cur.insert_ebb(ebb2);
        }

        let mut cfg = ControlFlowGraph::with_function(&func);

        {
            let ebb0_predecessors = cfg.pred_iter(ebb0).collect::<Vec<_>>();
            let ebb1_predecessors = cfg.pred_iter(ebb1).collect::<Vec<_>>();
            let ebb2_predecessors = cfg.pred_iter(ebb2).collect::<Vec<_>>();

            let ebb0_successors = cfg.succ_iter(ebb0).collect::<Vec<_>>();
            let ebb1_successors = cfg.succ_iter(ebb1).collect::<Vec<_>>();
            let ebb2_successors = cfg.succ_iter(ebb2).collect::<Vec<_>>();

            assert_eq!(ebb0_predecessors.len(), 0);
            assert_eq!(ebb1_predecessors.len(), 2);
            assert_eq!(ebb2_predecessors.len(), 2);

            assert_eq!(
                ebb1_predecessors.contains(&BlockPredecessor::new(ebb0, jmp_ebb0_ebb1)),
                true
            );
            assert_eq!(
                ebb1_predecessors.contains(&BlockPredecessor::new(ebb1, br_ebb1_ebb1)),
                true
            );
            assert_eq!(
                ebb2_predecessors.contains(&BlockPredecessor::new(ebb0, br_ebb0_ebb2)),
                true
            );
            assert_eq!(
                ebb2_predecessors.contains(&BlockPredecessor::new(ebb1, jmp_ebb1_ebb2)),
                true
            );

            assert_eq!(ebb0_successors, [ebb1, ebb2]);
            assert_eq!(ebb1_successors, [ebb1, ebb2]);
            assert_eq!(ebb2_successors, []);
        }

        // Change some instructions and recompute ebb0
        func.dfg.replace(br_ebb0_ebb2).brnz(cond, ebb1, &[]);
        func.dfg.replace(jmp_ebb0_ebb1).return_(&[]);
        cfg.recompute_ebb(&mut func, ebb0);
        let br_ebb0_ebb1 = br_ebb0_ebb2;

        {
            let ebb0_predecessors = cfg.pred_iter(ebb0).collect::<Vec<_>>();
            let ebb1_predecessors = cfg.pred_iter(ebb1).collect::<Vec<_>>();
            let ebb2_predecessors = cfg.pred_iter(ebb2).collect::<Vec<_>>();

            let ebb0_successors = cfg.succ_iter(ebb0);
            let ebb1_successors = cfg.succ_iter(ebb1);
            let ebb2_successors = cfg.succ_iter(ebb2);

            assert_eq!(ebb0_predecessors.len(), 0);
            assert_eq!(ebb1_predecessors.len(), 2);
            assert_eq!(ebb2_predecessors.len(), 1);

            assert_eq!(
                ebb1_predecessors.contains(&BlockPredecessor::new(ebb0, br_ebb0_ebb1)),
                true
            );
            assert_eq!(
                ebb1_predecessors.contains(&BlockPredecessor::new(ebb1, br_ebb1_ebb1)),
                true
            );
            assert_eq!(
                ebb2_predecessors.contains(&BlockPredecessor::new(ebb0, br_ebb0_ebb2)),
                false
            );
            assert_eq!(
                ebb2_predecessors.contains(&BlockPredecessor::new(ebb1, jmp_ebb1_ebb2)),
                true
            );

            assert_eq!(ebb0_successors.collect::<Vec<_>>(), [ebb1]);
            assert_eq!(ebb1_successors.collect::<Vec<_>>(), [ebb1, ebb2]);
            assert_eq!(ebb2_successors.collect::<Vec<_>>(), []);
        }
    }
}
