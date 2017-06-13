//! A frontend for building Cretonne IL from other languages.

use ir::{Ebb, Type, Value, Cursor, InstBuilder};
use context::Context;
use ssa::SSABuilder;
use entity_map::{EntityMap, EntityRef};
use std::hash::Hash;
use ssa::Block;
use std::usize;

/// Data structure containing the information necessary to build a Cretonne IL function.
pub struct ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    context: Context,
    ssa: SSABuilder<Variable>,
    /// Mapping from the user's `ExtendedBlock` to internal `Ebb` and its filled flag.
    block_mapping: EntityMap<ExtendedBlock, (Ebb, bool)>,
    types: EntityMap<Variable, Type>,
    position: Position,
}

struct Position {
    filled: bool,
    basic_block: Block,
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
    /// Creates a ILBuilder structure.
    pub fn new() -> ILBuilder<Variable, ExtendedBlock> {
        ILBuilder {
            context: Context::new(),
            ssa: SSABuilder::new(),
            block_mapping: EntityMap::new(),
            types: EntityMap::new(),
            position: Position {
                filled: true,
                basic_block: Block::new(usize::MAX),
            },
        }
    }

    /// Consumes the `ILBuilder` and create a compilation context containing the fully formed
    /// Cretonne IL function. To call at the end of the translation process.
    pub fn to_context(self) -> Context {
        self.context
    }

    /// After the call to this function, new instructions will be inserted into the designed
    /// block, in the order they are declared.
    ///
    /// When inserting the terminator instruction (which doesn't have a falltrough to its immediate
    /// successor), the block will be declared filled and it will not be possible to append
    /// instructions to it.
    pub fn switch_to_block(&mut self, block: ExtendedBlock) {
        // First we check that the previous block has been filled.
        assert!(self.position.filled);
        let ebb = match self.block_mapping.get(block) {
            Some(&(ebb, filled)) => {
                // We can't switch to a block already filled
                assert!(!filled);
                ebb
            }
            None => {
                // If the corresponding ebb hasn't been created we create it.
                self.create_ebb(block)
            }
        };
        let basic_block = self.ssa.declare_ebb_header_block(ebb);
        // Then we change the cursor position.
        self.position = Position {
            filled: false,
            basic_block: basic_block,
        };
    }

    /// Function to call with `block` as soon as the last branch instruction to `block` has been
    /// created. Declares that all the predecessors of this block are known.
    ///
    /// Forgetting to call this method on any block will prevent you from retrieving
    /// the Cretonne IL.
    pub fn seal_block(&mut self, block: ExtendedBlock) {
        let (ebb, _) = self.block_mapping[block];
        self.ssa
            .seal_ebb_header_block(ebb, &mut self.context.func.dfg);
    }

    /// In order to use a variable as argument or result of any instruction, you need to declare
    /// its type here.
    pub fn declare_var(&mut self, var: Variable, ty: Type) {
        *self.types.ensure(var) = ty;
    }

    /// Inserts a load immediate instruction at the end of the current extended block.
    pub fn iconst(&mut self, var: Variable, imm: i64) {
        assert!(!self.position.filled);
        let ty = match self.types.get(var) {
            Some(&ty) => ty,
            None => panic!("this variable is used but its type has not been declared"),
        };
        let val = {
            let cur = &mut Cursor::new(&mut self.context.func.layout);
            self.context.func.dfg.ins(cur).iconst(ty, imm)
        };
        self.def_var(var, val);
    }

    /// Inserts an add instruction at the end of the current extended block.
    pub fn iadd(&mut self, res: Variable, arg1: Variable, arg2: Variable) {
        assert!(!self.position.filled);
        let arg1val = self.use_var(arg1);
        let arg2val = self.use_var(arg2);
        let resval = {
            let cur = &mut Cursor::new(&mut self.context.func.layout);
            self.context.func.dfg.ins(cur).iadd(arg1val, arg2val)
        };
        self.def_var(res, resval);
    }

    /// Inserts an jump instruction at the end of the current extended block.
    pub fn jump(&mut self, dest: ExtendedBlock) {
        assert!(!self.position.filled);
        let ebb = match self.block_mapping.get(dest) {
            Some(&(ebb, _)) => ebb,
            None => {
                // If the corresponding ebb hasn't been created we create it.
                self.create_ebb(dest)
            }
        };
        {
            let cur = &mut Cursor::new(&mut self.context.func.layout);
            self.context.func.dfg.ins(cur).jump(ebb, &[]);
        }
        self.position.filled = true;
    }

    /// Inserts an branch if zero instruction at the end of the current extended block.
    pub fn brz(&mut self, var: Variable, dest: ExtendedBlock) {
        assert!(!self.position.filled);
        let val = self.use_var(var);
        let ebb = match self.block_mapping.get(dest) {
            Some(&(ebb, _)) => ebb,
            None => {
                // If the corresponding ebb hasn't been created we create it.
                self.create_ebb(dest)
            }
        };
        let jump_inst = {
            let cur = &mut Cursor::new(&mut self.context.func.layout);
            self.context.func.dfg.ins(cur).brz(val, ebb, &[])
        };
        // This branch means that we have to create a new basic block
        let old_basic_block = self.position.basic_block;
        let new_basic_block = self.ssa.declare_ebb_body_block(ebb, old_basic_block);
        self.position.basic_block = new_basic_block;
        // And declare the predecessor
        self.ssa
            .declare_ebb_predecessor(ebb, old_basic_block, jump_inst);
    }
}

// Helper functions
impl<Variable, ExtendedBlock> ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    fn create_ebb(&mut self, block: ExtendedBlock) -> Ebb {
        let ebb = self.context.func.dfg.make_ebb();
        self.context.func.layout.append_ebb(ebb);
        self.block_mapping[block] = (ebb, false);
        ebb
    }

    fn use_var(&mut self, var: Variable) -> Value {
        let ty = match self.types.get(var) {
            Some(&ty) => ty,
            None => panic!("this variable is used but its type has not been declared"),
        };
        self.ssa
            .use_var(&mut self.context.func.dfg,
                     var,
                     ty,
                     self.position.basic_block)
    }

    fn def_var(&mut self, var: Variable, val: Value) {
        self.ssa.def_var(var, val, self.position.basic_block);
    }
}
