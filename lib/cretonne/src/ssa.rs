//! A SSA-building API that handles incomplete CFGs.
//!
//! The algorithm is based upon Braun M., Buchwald S., Hack S., Leißa R., Mallon C.,
//! Zwinkau A. (2013) Simple and Efficient Construction of Static Single Assignment Form.
//! In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
//! Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg

use ir::{Ebb, Function, Value, ValueListPool, ValueList};
use entity_map::{EntityMap, EntityRef, PrimaryEntityData};
use entity_list::EntityList;
use entity_list::ListPool;
use sparse_map::{SparseMap, SparseMapValue};
use packed_option::ReservedValue;
use std::u32;
use std::collections::HashMap;

type BlockList = EntityList<Block>;
type BlockListPool = ListPool<Block>;

/// Structure containing the data relevant the construction of SSA for a given function.
///
/// The parameter struct `Variable` corresponds to the way variables are represented in the
/// non-SSA language you're translating from.
///
/// The SSA building relies on information about the variables used and defined, as well as
/// their position relative to basic blocks which are stricter than extended basic blocks since
/// they don't allow branching in the middle of them.
///
/// This SSA building module allows you to def and use variables on the fly while you are
/// constructing the CFG, no need for a separate SSA pass after the CFG is completed.
///
/// A basic block is said _filled_ if all the instruction that it contains have been translated,
/// and it is said _sealed_ if all of its predecessors have been declared. Only filled predecessors
/// can be declared.
pub struct SSABuilder<Variable>
    where Variable: EntityRef
{
    // Records for every variable its type and, for every revelant block, the last definition of
    // the variable in the block.
    variables: EntityMap<Variable, VariableData>,
    // Records the position of the basic blocks and the list of values used but not defined in the
    // block.
    blocks: EntityMap<Block, BlockData>,
    // Records the basic blocks at the beginning of the `Ebb`s.
    // The value is `(Ebb,Block)` because in a `SparseMap`, the key has to be computable from the
    // value.
    ebb_headers: SparseMap<Ebb, (Ebb, Block)>,
    // The two next fields are just regions of memory used to store lists.
    value_pool: ValueListPool,
    block_pool: BlockListPool,
}

// Describes the current position of a basic block in the control flow graph.
enum BlockPosition {
    // A block at the top of an `Ebb`. Contains the list of known predecessors of this block
    // and a boolean that indicates if the block is sealed or not.
    // A block is sealed if all of its predecessors have been declared.
    EbbHeader(BlockList, bool),
    // A block inside an `Ebb` with an unique other block as its predecessor.
    // The block is implicitely sealed at creation.
    EbbBody(Block),
}

struct BlockData {
    // Position of the block inside the control flow graph
    block_position: BlockPosition,
    // Ebb to which this block belongs.
    ebb: Ebb,
    // List of values used in this block or one of its sucessors but that don't have an
    // unique definition before.
    undef_values: ValueList,
}
impl PrimaryEntityData for BlockData {}

struct VariableData {
    // Records the current definitions of a variable, for each block.
    // The value is `(Block, Value)` because in a `SparseMap`, the key has to be computable from the
    // value.
    current_defs: HashMap<Block, Value>,
}
impl PrimaryEntityData for VariableData {}

/// A opaque reference to a basic block.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Block(u32);
impl EntityRef for Block {
    fn new(index: usize) -> Self {
        assert!(index < (u32::MAX as usize));
        Block(index as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

impl ReservedValue for Block {
    fn reserved_value() -> Block {
        Block(u32::MAX)
    }
}

impl SparseMapValue<Block> for (Block, Value) {
    fn key(&self) -> Block {
        match self {
            &(block, _) => block,
        }
    }
}

impl SparseMapValue<Ebb> for (Ebb, Block) {
    fn key(&self) -> Ebb {
        match self {
            &(ebb, _) => ebb,
        }
    }
}

impl<Variable> SSABuilder<Variable>
    where Variable: EntityRef
{
    /// Allocate a new blank SSA builder struct. Use the API function to interact with the struct.
    pub fn new() -> SSABuilder<Variable> {
        SSABuilder {
            variables: EntityMap::new(),
            blocks: EntityMap::new(),
            value_pool: ListPool::new(),
            block_pool: ListPool::new(),
            ebb_headers: SparseMap::new(),
        }
    }
}


/// The following methods are the API of the SSA builder. Here is how it should be used when
/// translating to Cretonne IL:
///
/// - for each new symbol encountered, create a corresponding variable with `make_var`;
///
/// - for each sequence of contiguous instructions (with no branches), create a corresponding
///   basic block with `declare_ebb_body_block` or `declare_ebb_header_block` depending on the
///   position of the basic block;
///
/// - while traversing a basic block and translating instruction, use `def_var` and `use_var`
///   to record definitions and uses of variables, these methods will give you the corresponding
///   SSA values;
///
/// - when all the instructions in a basic block have translated, the block is said _filled_ and
///   only then you can add it as a predecessor to other blocks with `declare_ebb_predecessor`;
///
/// - when you have constructed all the predecessor to a basic block at the beginning of an `Ebb`,
///   call `seal_ebb_header_block` on it with the `Function` that you are building.
impl<Variable> SSABuilder<Variable>
    where Variable: EntityRef
{
    /// Declares a new definition of a variable in a given basic block.
    /// The SSA value is passed as an argument because it should be created with
    /// `ir::DataFlowGraph::append_result`.
    pub fn def_var(&mut self, var: Variable, val: Value, block: Block) {
        self.variables[var].current_defs.insert(block, val);
    }

    /// Declares a use of a variable in a given basic block.
    /// Returns the SSA value corresponding to the current SSA definition of this variable.
    pub fn use_var(&mut self, var: Variable, block: Block) -> Value {
        unimplemented!()
    }

    /// Declares a new basic block belonging to the body of a certain `Ebb` and having `pred`
    /// as a predecessor. `pred` is the only predecessor of the block and the block is sealed
    /// at creation.
    ///
    /// To declare a `Ebb` header block, see `declare_ebb_header_block`.
    pub fn declare_ebb_body_block(&mut self, ebb: Ebb, pred: Block) -> Block {
        self.blocks
            .push(BlockData {
                      block_position: BlockPosition::EbbBody(pred),
                      ebb: ebb,
                      undef_values: EntityList::new(),
                  })
    }

    /// Declares a new basic block at the beginning of an `Ebb`. No predecessors are declared
    /// here and the block is not sealed.
    /// Predecessors have to be added with `declare_ebb_predecessor`.
    pub fn declare_ebb_header_block(&mut self, ebb: Ebb) -> Block {
        self.blocks
            .push(BlockData {
                      block_position: BlockPosition::EbbHeader(EntityList::new(), false),
                      ebb: ebb,
                      undef_values: EntityList::new(),
                  })
    }

    /// Declares a new predecessor for an `Ebb` header block. Note that the predecessor is a
    /// `Block` and not an `Ebb`. This `Block` must be filled before added as predecessor.
    pub fn declare_ebb_predecessor(&mut self, ebb: Ebb, pred: Block) {
        unimplemented!()
    }

    /// Completes the global value numbering for an `Ebb`, all of its predecessors having been
    /// already sealed.
    ///
    /// This method modifies the function's `Layout` by adding arguments to the `Ebb`s to
    /// take into account the Phi function placed by the SSA algorithm.
    pub fn seal_ebb_header_block(&mut self, ebb: Ebb, func: &mut Function) {
        unimplemented!()
    }
}

#[cfg(test)]
mod test {
    use entity_map::EntityRef;
    use ir::{Function, InstBuilder, Cursor, Type};
    use ir::types::*;
    use ssa::{SSABuilder, Block};
    use std::u32;

    /// A opaque reference to a basic block.
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

    //#[test]
    fn simple_block() {
        let mut func = Function::new();
        let mut ssa: SSABuilder<Variable> = SSABuilder::new();
        let ebb0 = func.dfg.make_ebb();

        let cur = &mut Cursor::new(&mut func.layout);
        cur.insert_ebb(ebb0);
        let block = ssa.declare_ebb_header_block(ebb0);
        // Here is the pseudo-program we want to translate:
        // x = 1;
        // y = 2;
        // z = x + y;
        // z = x + z;
        let x_var = Variable(0);
        let x_ssa = func.dfg.ins(cur).iconst(I32, 1);
        ssa.def_var(x_var, x_ssa, block);
        let y_var = Variable(1);
        let y_ssa = func.dfg.ins(cur).iconst(I32, 2);
        ssa.def_var(y_var, y_ssa, block);
        let z_var = Variable(2);
        let z1_ssa = func.dfg
            .ins(cur)
            .iadd(ssa.use_var(x_var, block), ssa.use_var(y_var, block));
        ssa.def_var(z_var, z1_ssa, block);
        let z2_ssa = func.dfg
            .ins(cur)
            .iadd(ssa.use_var(x_var, block), ssa.use_var(z_var, block));
        assert_eq!(ssa.use_var(z_var, block), z2_ssa);
        assert_eq!(ssa.use_var(x_var, block), x_ssa);
        assert_eq!(ssa.use_var(y_var, block), y_ssa);
    }
}
