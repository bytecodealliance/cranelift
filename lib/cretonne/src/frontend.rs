//! A frontend for building Cretonne IL from other languages.

use ir::{Ebb, Type, Value, Cursor, InstBuilder, Function, Inst, FunctionName, Signature};
use ssa::SSABuilder;
use entity_map::{EntityMap, EntityRef};
use std::hash::Hash;
use ssa::Block;

/// Data structure containing the information necessary to build a Cretonne IL function.
pub struct ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    func: Function,
    ssa: SSABuilder<Variable>,
    /// Mapping from the user's `ExtendedBlock` to internal `Ebb` and its filled flag.
    block_mapping: EntityMap<ExtendedBlock, ExtendedBlockData>,
    types: EntityMap<Variable, Type>,
    position: Position<ExtendedBlock>,
}

#[derive(Clone, Default)]
struct ExtendedBlockData {
    ebb: Ebb,
    filled: bool,
}

impl Default for Ebb {
    fn default() -> Ebb {
        Ebb::new(0)
    }
}

struct Position<ExtendedBlock> {
    extended_block: ExtendedBlock,
    ebb: Ebb,
    filled: bool,
    basic_block: Block,
}

/// This module allows you to create a function in Cretonne IL in a straightforward way, hiding
/// all the complexity of its internal representation.
///
/// The module is parametrized by two types which are the representations of variables and extended
/// basic blocks in your origin language. Every instruction of the Cretonne IL has an associated
/// method to create an instance of it. You are
/// responsible to split your instruction flow into extended blocks whose properties are:
///
/// - branch and jump instructions can only point at the top of extended blocks;
/// - the last instruction of each block is a terminator instruction which has no natural sucessor,
///   and those instructions can only appear at the end of extended blocks.
///
/// The parameters of Cretonne IL instructions are Cretonne IL values, which can only be created
/// as results of other Cretonne IL instructions. To be able to create variables redefined multiple
/// times in your program, use the `def_var` and `use_var` command, that will maintain the
/// correspondance between your variables and Cretonne IL SSA values.
impl<Variable, ExtendedBlock> ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    /// Creates a ILBuilder structure that will contain the function you're building.
    pub fn new(name: FunctionName, sig: Signature) -> ILBuilder<Variable, ExtendedBlock> {
        ILBuilder {
            func: Function::with_name_signature(name, sig),
            ssa: SSABuilder::new(),
            block_mapping: EntityMap::new(),
            types: EntityMap::new(),
            position: Position {
                extended_block: ExtendedBlock::default(),
                ebb: Ebb::new(0),
                filled: true,
                basic_block: Block::new(0),
            },
        }
    }

    /// Consumes the `ILBuilder` and create a compilation context containing the fully formed
    /// Cretonne IL function. To call at the end of the translation process.
    pub fn to_function(self) -> Function {
        self.func
    }

    /// After the call to this function, new instructions will be inserted into the designed
    /// block, in the order they are declared.
    ///
    /// When inserting the terminator instruction (which doesn't have a falltrough to its immediate
    /// successor), the block will be declared filled and it will not be possible to append
    /// instructions to it.
    pub fn switch_to_block(&mut self, block: ExtendedBlock) {
        // First we check that the previous block has been filled.
        if !self.position.filled {
            panic!("you have to fill your block before switching")
        };
        let ebb = self.get_or_create_ebb(block);
        if self.block_mapping[block].filled {
            panic!("you cannot switch to a block which is already filled")
        };
        // We cannot switch to a filled block
        let basic_block = self.ssa.header_block(ebb);
        // Then we change the cursor position.
        self.position = Position {
            extended_block: block,
            ebb: ebb,
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
        match self.block_mapping.get(block) {
            Some(data) => {
                self.ssa.seal_ebb_header_block(data.ebb, &mut self.func.dfg);
            }
            None => panic!("you cannot seal a block you have not yet referenced"),
        }
    }

    /// In order to use a variable in a `use_var`, you need to declare its type with this method.
    pub fn declare_var(&mut self, var: Variable, ty: Type) {
        *self.types.ensure(var) = ty;
    }

    /// Returns the Cretonne IL value corresponding to the utilization at the current program
    /// position of a previously defined user variable.
    pub fn use_var(&mut self, var: Variable) -> Value {
        let ty = match self.types.get(var) {
            Some(&ty) => ty,
            None => panic!("this variable is used but its type has not been declared"),
        };
        self.ssa
            .use_var(&mut self.func.dfg, var, ty, self.position.basic_block)
    }

    /// Register a new definition of a user variable.
    pub fn def_var(&mut self, var: Variable, val: Value) {
        self.ssa.def_var(var, val, self.position.basic_block);
    }
}

impl<Variable, ExtendedBlock> ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    /// Inserts an branch if zero instruction at the end of the current extended block.
    pub fn brz(&mut self, val: Value, dest: ExtendedBlock) {
        self.check_not_filled();
        let dest_ebb = self.get_or_create_ebb(dest);
        let jump_inst = {
            let cur = &mut Cursor::new(&mut self.func.layout);
            cur.goto_bottom(self.position.ebb);
            self.func.dfg.ins(cur).brz(val, dest_ebb, &[])
        };
        // This branch means that we have to create a new basic block
        self.declare_successor(dest_ebb, jump_inst);
        self.move_to_next_basic_block();
    }

    /// Inserts an branch if not zero instruction at the end of the current extended block.
    pub fn brnz(&mut self, val: Value, dest: ExtendedBlock) {
        self.check_not_filled();
        let dest_ebb = self.get_or_create_ebb(dest);
        let jump_inst = {
            let cur = &mut Cursor::new(&mut self.func.layout);
            cur.goto_bottom(self.position.ebb);
            self.func.dfg.ins(cur).brnz(val, dest_ebb, &[])
        };
        // This branch means that we have to create a new basic block
        self.declare_successor(dest_ebb, jump_inst);
        self.move_to_next_basic_block();
    }

    /// Inserts an add instruction at the end of the current extended block.
    pub fn iadd(&mut self, arg1: Value, arg2: Value) -> Value {
        self.check_not_filled();
        let cur = &mut Cursor::new(&mut self.func.layout);
        cur.goto_bottom(self.position.ebb);
        self.func.dfg.ins(cur).iadd(arg1, arg2)
    }

    /// Inserts a load immediate instruction at the end of the current extended block.
    pub fn iconst(&mut self, ty: Type, imm: i64) -> Value {
        self.check_not_filled();
        let cur = &mut Cursor::new(&mut self.func.layout);
        cur.goto_bottom(self.position.ebb);
        self.func.dfg.ins(cur).iconst(ty, imm)
    }

    /// Inserts an sub instruction at the end of the current extended block.
    pub fn isub(&mut self, arg1: Value, arg2: Value) -> Value {
        self.check_not_filled();
        let cur = &mut Cursor::new(&mut self.func.layout);
        cur.goto_bottom(self.position.ebb);
        self.func.dfg.ins(cur).isub(arg1, arg2)
    }

    /// Inserts an jump instruction at the end of the current extended block.
    pub fn jump(&mut self, dest: ExtendedBlock) {
        self.check_not_filled();
        let dest_ebb = self.get_or_create_ebb(dest);
        let jump_inst = {
            let cur = &mut Cursor::new(&mut self.func.layout);
            cur.goto_bottom(self.position.ebb);
            self.func.dfg.ins(cur).jump(dest_ebb, &[])
        };
        self.declare_successor(dest_ebb, jump_inst);
        self.fill_current_block();
    }

    /// Inserts a return instruction at the end of the current extended block.
    pub fn return_(&mut self, args: &[Value]) {
        self.check_not_filled();
        {
            let cur = &mut Cursor::new(&mut self.func.layout);
            cur.goto_bottom(self.position.ebb);
            self.func.dfg.ins(cur).return_(args);
        }
        self.fill_current_block();
    }
}

// Helper functions
impl<Variable, ExtendedBlock> ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    fn create_ebb(&mut self, block: ExtendedBlock) -> Ebb {
        let ebb = self.func.dfg.make_ebb();
        self.ssa.declare_ebb_header_block(ebb);
        self.func.layout.append_ebb(ebb);
        *self.block_mapping.ensure(block) = ExtendedBlockData {
            ebb: ebb,
            filled: false,
        };
        ebb
    }

    fn get_or_create_ebb(&mut self, block: ExtendedBlock) -> Ebb {
        match self.block_mapping.get(block) {
            Some(data) => return data.ebb,
            None => {}
        }
        // If the corresponding ebb hasn't been created we create it.
        self.create_ebb(block)
    }

    fn move_to_next_basic_block(&mut self) {
        self.position.basic_block =
            self.ssa
                .declare_ebb_body_block(self.position.ebb, self.position.basic_block);
    }

    fn fill_current_block(&mut self) {
        self.position.filled = true;
        self.block_mapping[self.position.extended_block].filled = true;
    }

    fn declare_successor(&mut self, dest_ebb: Ebb, jump_inst: Inst) {
        self.ssa
            .declare_ebb_predecessor(dest_ebb, self.position.basic_block, jump_inst);
    }

    fn check_not_filled(&self) {
        assert!(!self.position.filled);
    }
}

#[cfg(test)]
mod tests {
    use entity_map::EntityRef;
    use ir::{FunctionName, Signature, ArgumentType};
    use ir::types::*;
    use frontend::ILBuilder;

    use std::u32;

    /// An opaque reference to variable.
    #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
    pub struct Variable(u32);
    impl EntityRef for Variable {
        fn new(index: usize) -> Self {
            assert!(index < (u32::MAX as usize));
            Variable(index as u32)
        }

        fn index(self) -> usize {
            self.0 as usize
        }
    }
    impl Default for Variable {
        fn default() -> Variable {
            Variable(u32::MAX)
        }
    }
    /// An opaque reference to extended blocks.
    #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
    pub struct ExtendedBlock(u32);
    impl EntityRef for ExtendedBlock {
        fn new(index: usize) -> Self {
            assert!(index < (u32::MAX as usize));
            ExtendedBlock(index as u32)
        }

        fn index(self) -> usize {
            self.0 as usize
        }
    }
    impl Default for ExtendedBlock {
        fn default() -> ExtendedBlock {
            ExtendedBlock(u32::MAX)
        }
    }

    #[test]
    fn sample_function() {
        let mut sig = Signature::new();
        sig.return_types.push(ArgumentType::new(I32));
        sig.argument_types.push(ArgumentType::new(I32));
        let mut builder = ILBuilder::<Variable, ExtendedBlock>::new(FunctionName::new("sample_function",),
                                                                    sig);
        // Here is the pseudo-program we want to translate:
        // block0:
        //    x = 1;
        //    y = 2;
        //    z = x + y;
        //    jump block1
        // block1:
        //    z = z + y;
        //    brnz y, block2;
        //    z = z - x;
        //    return y
        // block2:
        //    y = y - x
        //    jump block1
        let block0 = ExtendedBlock(0);
        let block1 = ExtendedBlock(1);
        let block2 = ExtendedBlock(2);

        let x = Variable(0);
        let y = Variable(1);
        let z = Variable(2);
        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.declare_var(z, I32);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.iconst(I32, 1);
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.iconst(I32, 2);
            builder.def_var(y, tmp);
        }
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        builder.jump(block1);

        builder.switch_to_block(block1);
        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(z);
            let tmp = builder.iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.brnz(arg, block2);
        }
        {
            let arg1 = builder.use_var(z);
            let arg2 = builder.use_var(x);
            let tmp = builder.isub(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.return_(&[arg]);
        }

        builder.switch_to_block(block2);
        builder.seal_block(block2);

        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(x);
            let tmp = builder.isub(arg1, arg2);
            builder.def_var(y, tmp);
        }
        builder.jump(block1);
        builder.seal_block(block1);

        let func = builder.to_function();
        println!("{}", func.display(None));

    }

}
