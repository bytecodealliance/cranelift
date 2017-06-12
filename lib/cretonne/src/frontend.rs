//! A frontend for building Cretonne IL from other languages.

use ir::{Ebb, Function, Type};
use context::Context;
use flowgraph::ControlFlowGraph;
use ssa::SSABuilder;
use entity_map::{EntityMap, EntityRef};
use std::hash::Hash;

/// Data structure containing the information necessary to build a Cretonne IL function.
pub struct ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    func: Function,
    cfg: ControlFlowGraph,
    ssa: SSABuilder<Variable>,
    block_mapping: EntityMap<ExtendedBlock, Ebb>,
    types: EntityMap<Variable, Type>,
}

/// This module allows you to create a function in Cretonne IL in a straightforward way, hiding
/// all the complexity of its internal representation.
///
/// The module is parametrized by two types which are the representations of variables and extended
/// basic blocks in your origin language. Every instruction of the Cretonne IL has an associated
/// method to create an instance of it with immediate arguments and variable arguments. You are
/// responsible to split your instruction flow into extended blocks whose properties are:
///
/// - branch and jump instructions can only point at the top of extended blocks;
/// - the last instruction of each block is a terminator instruction which has no natural sucessor,
///   and those instructions can only appear at the end of extended blocks.
///
/// The module performs the translation of your origin variables into the Cretonne IL SSA values,
/// and builds the internal function data structure and the CFG from the data provided in the
/// instructions.
impl<Variable, ExtendedBlock> ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    /// After the call to this function, new instructions will be inserted into the designed
    /// block, in the order they are declared.
    ///
    /// When inserting the terminator instruction (which
    /// don't have a falltrough to its immediate successor), the block will be declared filled
    /// and it will not be possible to append instructions to it.
    pub fn switch_to_block(&mut self, block: ExtendedBlock) {
        unimplemented!();
    }

    /// Function to call with `block` as soon as the last branch instruction to `block` has been
    /// created. Declares that all the predecessors of this block are known.
    ///
    /// Forgetting to call this method on any block will prevent you from retrieving
    /// the Cretonne IL.
    pub fn seal_block(&mut self, block: ExtendedBlock) {
        unimplemented!();
    }

    /// In order to use a variable as argument or result of any instruction, you need to declare
    /// its type here.
    pub fn declare_var(&mut self, var: Variable, ty: Type) {
        unimplemented!();
    }

    /// Inserts an add instruction at the end of the current extended block.
    /// Example of an instruction method.
    pub fn iadd(&mut self, res: Variable, arg1: Variable, arg2: Variable) {
        unimplemented!();
    }

    /// Consumes the `ILBuilder` and create a compilation context containing the fully formed
    /// Cretonne IL function. To call at the end of the translation process.
    pub fn to_context(self) -> Context {
        unimplemented!();
    }
}
