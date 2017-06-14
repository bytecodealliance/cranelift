//! A frontend for building Cretonne IL from other languages.

use ir::{Ebb, Type, Value, Cursor, InstBuilder, Function, Inst, FunctionName, Signature, SigRef,
         FuncRef, JumpTable, StackSlot, MemFlags, JumpTableData, StackSlotData};
use ssa::SSABuilder;
use entity_map::{EntityMap, EntityRef};
use std::hash::Hash;
use ssa::Block;
use ir::immediates::{Imm64, Uimm8, Ieee32, Ieee64, Offset32, Uoffset32};
use ir::condcodes::{IntCC, FloatCC};
use isa::RegUnit;

/// Data structure containing the information necessary to build a Cretonne IL function.
///
/// # Example
///
/// Here is a pseudo-program we want to transform in Cretonne IL:
///
/// ```cton
/// function(x) {
/// x, y, z : I32
/// block0:
///    y = 2;
///    z = x + y;
///    jump block1
/// block1:
///    z = z + y;
///    brnz y, block2;
///    z = z - x;
///    return y
/// block2:
///    y = y - x
///    jump block1
/// }
/// ```
///
/// Here is how you build the corresponding Cretonne IL function using `ILBuilder`:
///
/// ```rust
/// use cretonne::entity_map::EntityRef;
/// use cretonne::ir::{FunctionName, Signature, ArgumentType};
/// use cretonne::ir::types::*;
/// use cretonne::frontend::ILBuilder;
/// use cretonne::verifier::verify_function;
///
/// use std::u32;
///
/// // An opaque reference to variable.
/// #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
/// pub struct Variable(u32);
/// impl EntityRef for Variable {
///     fn new(index: usize) -> Self {
///         assert!(index < (u32::MAX as usize));
///         Variable(index as u32)
///     }
///
///     fn index(self) -> usize {
///         self.0 as usize
///     }
/// }
/// impl Default for Variable {
///     fn default() -> Variable {
///         Variable(u32::MAX)
///     }
/// }
/// // An opaque reference to extended blocks.
/// #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
/// pub struct ExtendedBlock(u32);
/// impl EntityRef for ExtendedBlock {
///     fn new(index: usize) -> Self {
///         assert!(index < (u32::MAX as usize));
///         ExtendedBlock(index as u32)
///     }
///
///    fn index(self) -> usize {
///        self.0 as usize
///     }
/// }
/// impl Default for ExtendedBlock {
///     fn default() -> ExtendedBlock {
///         ExtendedBlock(u32::MAX)
///     }
/// }
///
/// let mut sig = Signature::new();
/// sig.return_types.push(ArgumentType::new(I32));
/// sig.argument_types.push(ArgumentType::new(I32));
/// let mut builder =
///     ILBuilder::<Variable, ExtendedBlock>::new(FunctionName::new("sample_function",),
///                                                             sig);
///
/// let block0 = ExtendedBlock(0);
/// let block1 = ExtendedBlock(1);
/// let block2 = ExtendedBlock(2);
///
/// let x = Variable(0);
/// let y = Variable(1);
/// let z = Variable(2);
/// builder.declare_var(x, I32);
/// builder.declare_var(y, I32);
/// builder.declare_var(z, I32);
///
/// builder.switch_to_block(block0);
/// builder.seal_block(block0);
/// {
///     let tmp = builder.arg_value(0);
///     builder.def_var(x, tmp);
/// }
/// {
///     let tmp = builder.iconst(I32, 2);
///     builder.def_var(y, tmp);
/// }
/// {
///     let arg1 = builder.use_var(x);
///     let arg2 = builder.use_var(y);
///     let tmp = builder.iadd(arg1, arg2);
///     builder.def_var(z, tmp);
/// }
/// builder.jump(block1);
///
/// builder.switch_to_block(block1);
/// {
///     let arg1 = builder.use_var(y);
///     let arg2 = builder.use_var(z);
///     let tmp = builder.iadd(arg1, arg2);
///     builder.def_var(z, tmp);
/// }
/// {
///     let arg = builder.use_var(y);
///     builder.brnz(arg, block2);
/// }
/// {
///     let arg1 = builder.use_var(z);
///     let arg2 = builder.use_var(x);
///     let tmp = builder.isub(arg1, arg2);
///     builder.def_var(z, tmp);
/// }
/// {
///     let arg = builder.use_var(y);
///     builder.return_(&[arg]);
/// }
///
/// builder.switch_to_block(block2);
/// builder.seal_block(block2);
///
/// {
///     let arg1 = builder.use_var(y);
///     let arg2 = builder.use_var(x);
///     let tmp = builder.isub(arg1, arg2);
///     builder.def_var(y, tmp);
/// }
/// builder.jump(block1);
/// builder.seal_block(block1);
///
/// let func = builder.to_function();
/// let res = verify_function(&func, None);
/// println!("{}", func.display(None));
/// match res {
///     Ok(_) => {}
///     Err(err) => panic!("{}", err),
/// }
/// ```
pub struct ILBuilder<Variable, ExtendedBlock>
    where Variable: EntityRef + Hash + Default,
          ExtendedBlock: EntityRef + Default
{
    func: Function,
    ssa: SSABuilder<Variable>,
    /// Mapping from the user's `ExtendedBlock` to internal `Ebb` and its filled flag.
    block_mapping: EntityMap<ExtendedBlock, ExtendedBlockData>,
    types: EntityMap<Variable, TypeData>,
    position: Position<ExtendedBlock>,
    pristine: bool,
    function_args_values: Vec<Value>,
}

#[derive(Clone, Default)]
struct ExtendedBlockData {
    ebb: Ebb,
    filled: bool,
    sealed: bool,
    created: bool,
}

#[derive(Clone, Default)]
struct TypeData {
    typ: Type,
    created: bool,
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
///
/// Before exporting the function with `to_function`, you need to fill (with instructions, the last
/// of which is a terminator) and seal (with `seal_block`) all the blocks you have referenced.
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
            pristine: true,
            function_args_values: Vec::new(),
        }
    }

    /// Consumes the `ILBuilder` and create a compilation context containing the fully formed
    /// Cretonne IL function. To call at the end of the translation process.
    pub fn to_function(self) -> Function {
        // First we check that all the blocks have been sealed and filled
        for extd_block in self.block_mapping.keys() {
            let ref block_data = self.block_mapping[extd_block];
            if block_data.created != bool::default() {
                if !block_data.filled {
                    panic!("a block is not filled");
                }
                if !block_data.sealed {
                    panic!("a block is not sealed");
                }
            }
        }
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
        if self.pristine {
            self.fill_function_args_values(ebb);
        }
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
        if self.block_mapping.ensure(block).created != bool::default() {
            self.ssa
                .seal_ebb_header_block(self.block_mapping[block].ebb, &mut self.func.dfg);
            self.block_mapping[block].sealed = true;
        } else {
            panic!("you cannot seal a block you have not yet referenced")
        }
    }

    /// In order to use a variable in a `use_var`, you need to declare its type with this method.
    pub fn declare_var(&mut self, var: Variable, ty: Type) {
        *self.types.ensure(var) = TypeData {
            typ: ty,
            created: !bool::default(),
        };
    }

    /// Returns the Cretonne IL value corresponding to the utilization at the current program
    /// position of a previously defined user variable.
    pub fn use_var(&mut self, var: Variable) -> Value {
        let ty = match self.types.get(var) {
            Some(ty) => {
                if ty.created != bool::default() {
                    ty.typ
                } else {
                    panic!("this variable is used but its type has not been declared")
                }
            }
            None => panic!("this variable is used but its type has not been declared"),
        };
        self.ssa
            .use_var(&mut self.func.dfg, var, ty, self.position.basic_block)
    }

    /// Register a new definition of a user variable. Panics if the type of the value is not the
    /// same as the type registered for the variable.
    pub fn def_var(&mut self, var: Variable, val: Value) {
        if self.func.dfg.value_type(val) != self.types[var].typ {
            panic!("the type of the value is not the type registered for the variable")
        };
        self.ssa.def_var(var, val, self.position.basic_block);
    }

    /// Returns the value corresponding to the `i`-th argument of the function as defined by
    /// the function signature. Panics if `i` is out of bounds or if called before the first call
    /// to `switch_to_block`.
    pub fn arg_value(&self, i: usize) -> Value {
        if self.pristine {
            panic!("you have to call switch_to_block first.")
        }
        self.function_args_values[i]
    }

    /// Creates a jump table in the function, to be used by `br_table` instructions.
    pub fn create_jump_table(&mut self) -> JumpTable {
        self.func.jump_tables.push(JumpTableData::new())
    }

    /// Inserts an entry in a previously declared jump table.
    pub fn insert_jump_table_entry(&mut self, jt: JumpTable, index: usize, block: ExtendedBlock) {
        let ebb = self.get_or_create_ebb(block);
        self.func.jump_tables[jt].set_entry(index, ebb);
    }

    /// Creates a stack slot in the function, to be used by `stack_load`, `stack_store` and
    /// `stack_addr` instructions.
    pub fn create_stack_slot(&mut self, size: u32) -> StackSlot {
        self.func.stack_slots.push(StackSlotData::new(size))
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
            created: !bool::default(),
            ebb: ebb,
            filled: false,
            sealed: false,
        };
        ebb
    }

    fn get_or_create_ebb(&mut self, block: ExtendedBlock) -> Ebb {
        match self.block_mapping.get(block) {
            Some(data) => {
                if data.created != bool::default() {
                    return data.ebb;
                }
            }
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

    fn check_return_args(&self, args: &[Value]) {
        if args.len() != self.func.signature.return_types.len() {
            panic!("the number of returned values doesn't match the function signature")
        };
        for (i, arg) in args.iter().enumerate() {
            let valty = self.func.dfg.value_type(*arg);
            if self.func.signature.return_types[i].value_type != valty {
                panic!("the types of the values returned don't match the function signature");
            }
        }
    }

    fn fill_function_args_values(&mut self, ebb: Ebb) {
        assert!(self.pristine);
        for argtyp in self.func.signature.argument_types.iter() {
            self.function_args_values
                .push(self.func.dfg.append_ebb_arg(ebb, argtyp.value_type));
        }
        self.pristine = false;
    }
}

// Include code generated by `lib/cretonne/meta/gen_instr.py`.
//
// This file defined all the methods corresponding to the instructions of Cretonne IL.
include!(concat!(env!("OUT_DIR"), "/frontend.rs"));
