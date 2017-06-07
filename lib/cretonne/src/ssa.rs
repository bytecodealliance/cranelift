//! A SSA-building API that handles incomplete CFGs.
//!
//! The algorithm is based upon Braun M., Buchwald S., Hack S., Leißa R., Mallon C.,
//! Zwinkau A. (2013) Simple and Efficient Construction of Static Single Assignment Form.
//! In: Jhala R., De Bosschere K. (eds) Compiler Construction. CC 2013.
//! Lecture Notes in Computer Science, vol 7791. Springer, Berlin, Heidelberg

use ir::{Ebb, Value, Inst, Type, DataFlowGraph};
use std::hash::Hash;
use entity_map::{EntityMap, EntityRef, PrimaryEntityData};
use sparse_map::{SparseMap, SparseMapValue};
use packed_option::ReservedValue;
use std::u32;
use std::collections::HashMap;

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
    variables: SparseMap<Variable, VariableData<Variable>>,
    // Records the position of the basic blocks and the list of values used but not defined in the
    // block.
    blocks: EntityMap<Block, BlockData<Variable>>,
    // Records the basic blocks at the beginning of the `Ebb`s.
    // The value is `(Ebb,Block)` because in a `SparseMap`, the key has to be computable from the
    // value.
    ebb_headers: SparseMap<Ebb, (Ebb, Block)>,
}

// Describes the current position of a basic block in the control flow graph.
enum BlockData<Variable> {
    // A block at the top of an `Ebb`. Contains the list of known predecessors of this block
    // and a boolean that indicates if the block is sealed or not.
    // A block is sealed if all of its predecessors have been declared.
    // The fields are `(predecessors, sealed, ebb, undef_values)`.
    EbbHeader(HashMap<Block, Inst>, bool, Ebb, Vec<(Variable, Value)>),
    // A block inside an `Ebb` with an unique other block as its predecessor.
    // The block is implicitely sealed at creation.
    // The fields are `(predecessor, ebb)`.
    EbbBody(Block, Ebb),
}
impl<Variable> PrimaryEntityData for BlockData<Variable> {}

impl<Variable> BlockData<Variable> {
    pub fn add_predecessor(&mut self, pred: Block, inst: Inst) {
        match self {
            &mut BlockData::EbbBody(_, _) => assert!(false),
            &mut BlockData::EbbHeader(ref mut predecessors, _, _, _) => {
                predecessors.insert(pred, inst);
                ()
            }
        }
    }
}

struct VariableData<Variable> {
    // Records the current definitions of a variable, for each block.
    // The value is `(Block, Value)` because in a `SparseMap`, the key has to be computable from the
    // value.
    current_defs: HashMap<Block, Value>,
    variable: Variable,
}
impl<Variable> PrimaryEntityData for VariableData<Variable> {}

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

impl<Variable> SparseMapValue<Variable> for VariableData<Variable>
    where Variable: EntityRef
{
    fn key(&self) -> Variable {
        self.variable
    }
}

impl<Variable> SSABuilder<Variable>
    where Variable: EntityRef
{
    /// Allocate a new blank SSA builder struct. Use the API function to interact with the struct.
    pub fn new() -> SSABuilder<Variable> {
        SSABuilder {
            variables: SparseMap::new(),
            blocks: EntityMap::new(),
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
    where Variable: EntityRef + Hash
{
    /// Declares a new definition of a variable in a given basic block.
    /// The SSA value is passed as an argument because it should be created with
    /// `ir::DataFlowGraph::append_result`.
    pub fn def_var(&mut self, var: Variable, val: Value, block: Block) {
        if match self.variables.get_mut(var) {
               Some(data) => {
                   data.current_defs.insert(block, val);
                   false
               }
               None => true,
           } {
            let mut defs = HashMap::new();
            defs.insert(block, val);
            self.variables
                .insert(VariableData {
                            current_defs: defs,
                            variable: var,
                        });
        }
    }

    /// Declares a use of a variable in a given basic block.
    /// Returns the SSA value corresponding to the current SSA definition of this variable.
    pub fn use_var(&mut self,
                   dfg: &mut DataFlowGraph,
                   var: Variable,
                   ty: Type,
                   block: Block)
                   -> Value {
        // First we lookup for the current definition of the variable in this block
        match self.variables.get(var) {
            None => (),
            Some(var_data) => {
                match var_data.current_defs.get(&block) {
                    None => (),
                    Some(val) => {
                        return *val;
                    }
                }
            }
        };
        // At this point if we haven't returned it means that we have to search in the
        // predecessors.
        let (new_block, possible_val) = match self.blocks[block] {
            BlockData::EbbHeader(_, sealed, ebb, ref mut undef_variables) => {
                // The block has multiple predecessors so we append an Ebb argument that
                // will serve as a value.
                // TODO: BUGFIX if the block is sealed then we have to append the branch argument
                // to its predecessors
                let val = dfg.append_ebb_arg(ebb, ty);
                undef_variables.push((var, val));
                (block, Some((val, sealed, ebb)))
            }
            BlockData::EbbBody(pred, _) => (pred, None),
        };
        match possible_val {
            // The block has a single predecessor, we look into it.
            None => self.use_var(dfg, var, ty, new_block),
            // The block has multiple predecessors, we register the ebb argument as the current
            // definition for the variable.
            Some((val, sealed, ebb)) => {
                self.def_var(var, val, block);
                if sealed {
                    self.seal_ebb_header_block(ebb, dfg)
                }
                val
            }
        }

    }

    /// Declares a new basic block belonging to the body of a certain `Ebb` and having `pred`
    /// as a predecessor. `pred` is the only predecessor of the block and the block is sealed
    /// at creation.
    ///
    /// To declare a `Ebb` header block, see `declare_ebb_header_block`.
    pub fn declare_ebb_body_block(&mut self, ebb: Ebb, pred: Block) -> Block {
        self.blocks.push(BlockData::EbbBody(pred, ebb))
    }

    /// Declares a new basic block at the beginning of an `Ebb`. No predecessors are declared
    /// here and the block is not sealed.
    /// Predecessors have to be added with `declare_ebb_predecessor`.
    pub fn declare_ebb_header_block(&mut self, ebb: Ebb) -> Block {
        let block = self.blocks
            .push(BlockData::EbbHeader(HashMap::new(), false, ebb, Vec::new()));
        self.ebb_headers.insert((ebb, block));
        block
    }

    /// Declares a new predecessor for an `Ebb` header block and record the branch instruction
    /// of the predecessor that leads to it. Note that the predecessor is a `Block` and not an
    /// `Ebb`. This `Block` must be filled before added as predecessor.
    pub fn declare_ebb_predecessor(&mut self, ebb: Ebb, pred: Block, inst: Inst) {
        let header_block = match self.ebb_headers.get(ebb) {
            None => panic!("you are declaring a predecessor for an ebb not declared"),
            Some(&(_, header)) => header,

        };
        self.blocks[header_block].add_predecessor(pred, inst)
    }

    /// Completes the global value numbering for an `Ebb`, all of its predecessors having been
    /// already sealed.
    ///
    /// This method modifies the function's `Layout` by adding arguments to the `Ebb`s to
    /// take into account the Phi function placed by the SSA algorithm.
    pub fn seal_ebb_header_block(&mut self, ebb: Ebb, dfg: &mut DataFlowGraph) {
        let block = match self.ebb_headers.get(ebb) {
            None => panic!("this ebb has no block header defined"),
            Some(&(_, block)) => block,
        };
        // TODO: find a way to not allocate vectors
        let (predecessors, undef_vars): (Vec<(Block, Inst)>, Vec<(Variable, Value)>) =
            match self.blocks[block] {
                BlockData::EbbBody(_, _) => panic!("this should not happen"),
                BlockData::EbbHeader(ref predecessors, _, _, ref undef_vars) => {
                    (predecessors.iter().map(|(&x, &y)| (x, y)).collect(), undef_vars.clone())
                }
            };
        for &(pred, last_inst) in predecessors.iter() {
            // For each predecessor we query what is the local SSA value corresponding to var
            // and we put it as an argument of the branch instruction.
            for &(var, val) in undef_vars.iter() {
                let ty = dfg.value_type(val);
                let pred_val = self.use_var(dfg, var, ty, pred);
                dfg.append_branch_argument(last_inst, ebb, pred_val);
            }
        }
        // Then we clear the undef_vars.
        match self.blocks[block] {
            BlockData::EbbBody(_, _) => panic!("this should not happen"),
            BlockData::EbbHeader(_, _, _, ref mut undef_vars) => undef_vars.clear(),
        };

    }
}

#[cfg(test)]
mod tests {
    use entity_map::EntityRef;
    use ir::{Function, InstBuilder, Cursor, Inst};
    use ir::types::*;
    use ssa::SSABuilder;
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

    #[test]
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
        assert_eq!(ssa.use_var(&mut func.dfg, x_var, I32, block), x_ssa);
        assert_eq!(ssa.use_var(&mut func.dfg, y_var, I32, block), y_ssa);
        let z_var = Variable(2);
        let x_use1 = ssa.use_var(&mut func.dfg, x_var, I32, block);
        let y_use1 = ssa.use_var(&mut func.dfg, y_var, I32, block);
        let z1_ssa = func.dfg.ins(cur).iadd(x_use1, y_use1);
        ssa.def_var(z_var, z1_ssa, block);
        assert_eq!(ssa.use_var(&mut func.dfg, z_var, I32, block), z1_ssa);
        let x_use2 = ssa.use_var(&mut func.dfg, x_var, I32, block);
        let z_use1 = ssa.use_var(&mut func.dfg, z_var, I32, block);
        let z2_ssa = func.dfg.ins(cur).iadd(x_use2, z_use1);
        ssa.def_var(z_var, z2_ssa, block);
        assert_eq!(ssa.use_var(&mut func.dfg, z_var, I32, block), z2_ssa);
    }

    #[test]
    fn sequence_of_blocks() {
        let mut func = Function::new();
        let mut ssa: SSABuilder<Variable> = SSABuilder::new();
        let ebb0 = func.dfg.make_ebb();
        let ebb1 = func.dfg.make_ebb();

        let cur = &mut Cursor::new(&mut func.layout);
        cur.insert_ebb(ebb0);
        cur.insert_ebb(ebb1);
        cur.goto_bottom(ebb0);
        let block0 = ssa.declare_ebb_header_block(ebb0);
        // Here is the pseudo-program we want to translate:
        // ebb0:
        //    x = 1;
        //    y = 2;
        //    z = x + y;
        //    jump ebb1;
        //    z = x + z;
        // ebb1:
        //    y = x + y;
        let x_var = Variable(0);
        let x_ssa = func.dfg.ins(cur).iconst(I32, 1);
        ssa.def_var(x_var, x_ssa, block0);
        let y_var = Variable(1);
        let y_ssa = func.dfg.ins(cur).iconst(I32, 2);
        ssa.def_var(y_var, y_ssa, block0);
        assert_eq!(ssa.use_var(&mut func.dfg, x_var, I32, block0), x_ssa);
        assert_eq!(ssa.use_var(&mut func.dfg, y_var, I32, block0), y_ssa);
        let z_var = Variable(2);
        let x_use1 = ssa.use_var(&mut func.dfg, x_var, I32, block0);
        let y_use1 = ssa.use_var(&mut func.dfg, y_var, I32, block0);
        let z1_ssa = func.dfg.ins(cur).iadd(x_use1, y_use1);
        ssa.def_var(z_var, z1_ssa, block0);
        assert_eq!(ssa.use_var(&mut func.dfg, z_var, I32, block0), z1_ssa);
        let y_use2 = ssa.use_var(&mut func.dfg, y_var, I32, block0);
        let jump_inst: Inst = func.dfg.ins(cur).brnz(y_use2, ebb1, &[]);
        let block1 = ssa.declare_ebb_body_block(ebb0, block0);
        let x_use2 = ssa.use_var(&mut func.dfg, x_var, I32, block1);
        assert_eq!(x_use2, x_ssa);
        let z_use1 = ssa.use_var(&mut func.dfg, z_var, I32, block1);
        assert_eq!(z_use1, z1_ssa);
        let z2_ssa = func.dfg.ins(cur).iadd(x_use2, z_use1);
        ssa.def_var(z_var, z2_ssa, block1);
        assert_eq!(ssa.use_var(&mut func.dfg, z_var, I32, block1), z2_ssa);
        ssa.seal_ebb_header_block(ebb0, &mut func.dfg);
        let block2 = ssa.declare_ebb_header_block(ebb1);
        ssa.declare_ebb_predecessor(ebb1, block0, jump_inst);
        let x_use3 = ssa.use_var(&mut func.dfg, x_var, I32, block2);
        assert_eq!(func.dfg.ebb_args(ebb1)[0], x_use3);
        let y_use3 = ssa.use_var(&mut func.dfg, y_var, I32, block2);
        assert_eq!(func.dfg.ebb_args(ebb1)[1], y_use3);
        cur.goto_bottom(ebb1);
        let y2_ssa = func.dfg.ins(cur).iadd(x_use3, y_use3);
        ssa.def_var(y_var, y2_ssa, block2);
        ssa.seal_ebb_header_block(ebb1, &mut func.dfg);
        match func.dfg[jump_inst].branch_arguments(ebb1) {
            None => assert!(false),
            Some(jump_args) => {
                assert_eq!(jump_args.as_slice(&func.dfg.value_lists)[1], x_ssa);
                assert_eq!(jump_args.as_slice(&func.dfg.value_lists)[2], y_ssa)
            }
        };
    }
}
