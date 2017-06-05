//! A SSA-building API that handles incomplete CFGs.

/// The algorithm is based upon Braun M., Buchwald S., Hack S., Leißa R., Mallon C.,
/// Zwinkau A. (2013) Simple and Efficient Construction of Static Single Assignment Form.
/// In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
/// Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg

use ir::{Ebb, Function, Value, Type, ValueListPool, ValueList};
use entity_map::{EntityMap, EntityRef};
use flowgraph::ControlFlowGraph;
use entity_list::ListPool;
use sparse_map::{SparseMap, SparseMapValue};
use packed_option::{PackedOption, ReservedValue};
use std::u32;

/// Structure containing the data relevant the construction of SSA for a given function.
pub struct SSABuilder {
    // Records for every variable its type and, for every revelant block, the last definition of
    // the variable in the block.
    variables: EntityMap<Variable, VariableData>,
    // Records the position of the basic blocks and the list of values used but not defined in the
    // block.
    blocks: EntityMap<Block, BlockData>,
    // Records the basic blocks at the beginning of the `Ebb`s.
    ebb_headers: SparseMap<Ebb, (Ebb, Block)>,
    // Region of memory used to store the lists of values.
    value_pool: ValueListPool,
}

struct BlockData {
    block_predecessor: PackedOption<Block>,
    ebb: Ebb,
    undef_values: ValueList,
}

struct VariableData {
    current_defs: SparseMap<Block, (Block, Value)>,
    ty: Type,
}

/// A opaque reference to a non-SSA variable.
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

impl SSABuilder {
    /// Allocate a new blank SSA builder struct. Use the API function to interact with the struct.
    pub fn new() -> SSABuilder {
        SSABuilder {
            variables: EntityMap::new(),
            blocks: EntityMap::new(),
            value_pool: ListPool::new(),
            ebb_headers: SparseMap::new(),
        }
    }
}


/// The following methods are the API of the SSA builder. Here is how it should be used when
/// translating to Cretonne IL:
/// * for each new symbol encountered, create a corresponding variable with `make_var`;
/// * for each sequence of contiguous instructions (with no branches), create a corresponding
///   basic block with `declare_block`;
/// * while traversing a basic block, use `def_var` and `use_var` to record definitions and
///   uses of variables, these methods will give you the corresponding SSA values;
/// * when you have constructed all the successors to a basic block at the beginning of an `Ebb`,
///   call `seal_block` on it with the `Function` and `ControlFlowGraph` that you are building.
impl SSABuilder {
    /// Declares a new variable of given type to the SSA builder.
    pub fn make_var(ty: Type) -> Variable {
        unimplemented!()
    }

    /// Declares a new definition of a variable in a given basic block.
    /// Returns the SSA value corresponding to this definition.
    pub fn def_var(var: Variable, block: Block) -> Value {
        unimplemented!()
    }

    /// Declares a use of a variable in a given basic block.
    /// Returns the SSA value corresponding to the current SSA definition of this variable.
    pub fn use_var(var: Variable, block: Block) -> Value {
        unimplemented!()
    }

    /// Declares a new basic block belonging to a certain `Ebb`. If it is the first basic block
    /// in the `Ebb`, pass `None` for the second argument, otherwise give the predecessor.
    pub fn declare_block(ebb: Ebb, pred: Option<Block>) -> Block {
        unimplemented!()
    }

    /// Completes the global value numbering for an `Ebb`, all of its predecessors having been
    /// already sealed.
    ///
    /// This method modifies the function's `Layout` by adding arguments to the `Ebbs` to
    /// take into account the Phi function placed by the SSA algorithm.
    pub fn seal_block(ebb: Ebb, func: &mut Function, cfg: &ControlFlowGraph) {
        unimplemented!()
    }
}
