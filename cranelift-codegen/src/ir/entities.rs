//! Cranelift IR entity references.
//!
//! Instructions in Cranelift IR need to reference other entities in the function. This can be other
//! parts of the function like extended basic blocks or stack slots, or it can be external entities
//! that are declared in the function preamble in the text format.
//!
//! These entity references in instruction operands are not implemented as Rust references both
//! because Rust's ownership and mutability rules make it difficult, and because 64-bit pointers
//! take up a lot of space, and we want a compact in-memory representation. Instead, entity
//! references are structs wrapping a `u32` index into a table in the `Function` main data
//! structure. There is a separate index type for each entity type, so we don't lose type safety.
//!
//! The `entities` module defines public types for the entity references along with constants
//! representing an invalid reference. We prefer to use `Option<EntityRef>` whenever possible, but
//! unfortunately that type is twice as large as the 32-bit index type on its own. Thus, compact
//! data structures use the `PackedOption<EntityRef>` representation, while function arguments and
//! return values prefer the more Rust-like `Option<EntityRef>` variant.
//!
//! The entity references all implement the `Display` trait in a way that matches the textual IR
//! format.

use crate::entity::entity_impl;
use core::fmt;
use core::u32;
#[cfg(feature = "enable-serde")]
use serde::{Deserialize, Serialize};

/// An opaque reference to an [extended basic
/// block](https://en.wikipedia.org/wiki/Extended_basic_block) in a
/// [`Function`](super::function::Function).
///
/// You can get an `Ebb` using `cranelift_frontend::FunctionBuilder::create_ebb`.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Ebb(u32);
entity_impl!(Ebb, "ebb");

impl Ebb {
    /// Create a new EBB reference from its number. This corresponds to the `ebbNN` representation.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Ebb(n))
        } else {
            None
        }
    }
}

/// An opaque reference to an SSA value.
///
/// You can get a constant `Value` from the following
/// [`InstBuilder`](super::InstBuilder) instructions:
///
/// - [`iconst`](super::InstBuilder::iconst) for integer constants
/// - [`f32const`](super::InstBuilder::f32const) for 32-bit float constants
/// - [`f64const`](super::InstBuilder::f64const) for 64-bit float constants
/// - [`bconst`](super::InstBuilder::bconst) for boolean constants
/// - [`vconst`](super::InstBuilder::vconst) for vector constants
/// - [`null`](super::InstBuilder::null) for null pointer constants
///
/// Any `InstBuilder` instruction that has an output will also return a `Value`.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "v");

impl Value {
    /// Create a value from its number representation.
    /// This is the number in the `vNN` notation.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX / 2 {
            Some(Value(n))
        } else {
            None
        }
    }
}

/// An opaque reference to an instruction in a function.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Inst(u32);
entity_impl!(Inst, "inst");

/// An opaque reference to a stack slot.
///
/// Stack slots represent an address on the
/// [call stack](https://en.wikipedia.org/wiki/Call_stack).
///
/// `StackSlot`s can be created with
/// `cranelift_frontend::FunctionBuilder::create_stackslot`.
///
/// `StackSlot`s are most often used with
/// [`stack_addr`](super::InstBuilder::stack_addr) and
/// [`stack_load`](super::InstBuilder::stack_load).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "enable-serde", derive(Serialize, Deserialize))]
pub struct StackSlot(u32);
entity_impl!(StackSlot, "ss");

impl StackSlot {
    /// Create a new stack slot reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(StackSlot(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a global value.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct GlobalValue(u32);
entity_impl!(GlobalValue, "gv");

impl GlobalValue {
    /// Create a new global value reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(GlobalValue(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a constant.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Constant(u32);
entity_impl!(Constant, "const");

impl Constant {
    /// Create a const reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Constant(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a [jump table](https://en.wikipedia.org/wiki/Branch_table).
///
/// `JumpTable`s are used for indirect branching.
/// See [`br_table`](super::InstBuilder::br_table) for details.
///
/// `JumpTable`s can be created with
/// [`create_jump_table`](cranelift_frontend::FunctionBuilder::create_jump_table).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "enable-serde", derive(Serialize, Deserialize))]
pub struct JumpTable(u32);
entity_impl!(JumpTable, "jt");

impl JumpTable {
    /// Create a new jump table reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(JumpTable(n))
        } else {
            None
        }
    }
}

/// An opaque reference to another [`Function`](super::Function).
///
/// `FuncRef`s are used for [direct](super::InstBuilder::call) function calls
/// and by [`func_addr`](super::InstBuilder::func_addr) for use in
/// [indirect](super::InstBuilder::call_indirect) function calls.
///
/// `FuncRef`s can be created with
///
/// - [`import_function`](cranelift_frontend::FunctionBuilder::import_function)
/// for external functions
/// - [`declare_func_in_func`](cranelift_module::Module::declare_func_in_func)
/// for functions declared elsewhere in the same native
/// [`Module`](cranelift_module::Module)
/// - [`make_direct_func`](cranelift_wasm::FuncEnvironment::make_direct_func)
/// for functions declared in the same WebAssembly
/// [`FuncEnvironment`](cranelift_wasm::FuncEnvironment)
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FuncRef(u32);
entity_impl!(FuncRef, "fn");

impl FuncRef {
    /// Create a new external function reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(FuncRef(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a function [`Signature`](super::Signature).
///
/// `SigRef`s are used to declare a function with
/// [`import_function`](cranelift_frontend::FunctionBuilder::import_function)
/// as well as to make an [indirect function call](super::InstBuilder::call_indirect).
///
/// `SigRef`s can be created with
/// [`import_signature`](cranelift_frontend::FunctionBuilder::import_signature).
///
/// You can retrieve the [`Signature`](super::Signature) that was used to create a `SigRef` with
/// [`signature`](cranelift_frontend::FunctionBuilder::signature).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SigRef(u32);
entity_impl!(SigRef, "sig");

impl SigRef {
    /// Create a new function signature reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(SigRef(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a [heap](https://en.wikipedia.org/wiki/Memory_management#DYNAMIC).
///
/// Heaps are used to access dynamically allocated memory through
/// [`heap_addr`](super::InstBuilder::heap_addr).
///
/// To create a heap, use [`create_heap`](cranelift_frontend::FunctionBuilder::create_heap).
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Heap(u32);
entity_impl!(Heap, "heap");

impl Heap {
    /// Create a new heap reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Heap(n))
        } else {
            None
        }
    }
}

/// An opaque reference to a table.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Table(u32);
entity_impl!(Table, "table");

impl Table {
    /// Create a new table reference from its number.
    ///
    /// This method is for use by the parser.
    pub fn with_number(n: u32) -> Option<Self> {
        if n < u32::MAX {
            Some(Table(n))
        } else {
            None
        }
    }
}

/// An opaque reference to any of the entities defined in this module that can appear in CLIF IR.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum AnyEntity {
    /// The whole function.
    Function,
    /// An extended basic block.
    Ebb(Ebb),
    /// An instruction.
    Inst(Inst),
    /// An SSA value.
    Value(Value),
    /// A stack slot.
    StackSlot(StackSlot),
    /// A Global value.
    GlobalValue(GlobalValue),
    /// A jump table.
    JumpTable(JumpTable),
    /// An external function.
    FuncRef(FuncRef),
    /// A function call signature.
    SigRef(SigRef),
    /// A heap.
    Heap(Heap),
    /// A table.
    Table(Table),
}

impl fmt::Display for AnyEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AnyEntity::Function => write!(f, "function"),
            AnyEntity::Ebb(r) => r.fmt(f),
            AnyEntity::Inst(r) => r.fmt(f),
            AnyEntity::Value(r) => r.fmt(f),
            AnyEntity::StackSlot(r) => r.fmt(f),
            AnyEntity::GlobalValue(r) => r.fmt(f),
            AnyEntity::JumpTable(r) => r.fmt(f),
            AnyEntity::FuncRef(r) => r.fmt(f),
            AnyEntity::SigRef(r) => r.fmt(f),
            AnyEntity::Heap(r) => r.fmt(f),
            AnyEntity::Table(r) => r.fmt(f),
        }
    }
}

impl fmt::Debug for AnyEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &dyn fmt::Display).fmt(f)
    }
}

impl From<Ebb> for AnyEntity {
    fn from(r: Ebb) -> Self {
        AnyEntity::Ebb(r)
    }
}

impl From<Inst> for AnyEntity {
    fn from(r: Inst) -> Self {
        AnyEntity::Inst(r)
    }
}

impl From<Value> for AnyEntity {
    fn from(r: Value) -> Self {
        AnyEntity::Value(r)
    }
}

impl From<StackSlot> for AnyEntity {
    fn from(r: StackSlot) -> Self {
        AnyEntity::StackSlot(r)
    }
}

impl From<GlobalValue> for AnyEntity {
    fn from(r: GlobalValue) -> Self {
        AnyEntity::GlobalValue(r)
    }
}

impl From<JumpTable> for AnyEntity {
    fn from(r: JumpTable) -> Self {
        AnyEntity::JumpTable(r)
    }
}

impl From<FuncRef> for AnyEntity {
    fn from(r: FuncRef) -> Self {
        AnyEntity::FuncRef(r)
    }
}

impl From<SigRef> for AnyEntity {
    fn from(r: SigRef) -> Self {
        AnyEntity::SigRef(r)
    }
}

impl From<Heap> for AnyEntity {
    fn from(r: Heap) -> Self {
        AnyEntity::Heap(r)
    }
}

impl From<Table> for AnyEntity {
    fn from(r: Table) -> Self {
        AnyEntity::Table(r)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::u32;
    use std::string::ToString;

    #[test]
    fn value_with_number() {
        assert_eq!(Value::with_number(0).unwrap().to_string(), "v0");
        assert_eq!(Value::with_number(1).unwrap().to_string(), "v1");

        assert_eq!(Value::with_number(u32::MAX / 2), None);
        assert!(Value::with_number(u32::MAX / 2 - 1).is_some());
    }

    #[test]
    fn memory() {
        use crate::packed_option::PackedOption;
        use core::mem;
        // This is the whole point of `PackedOption`.
        assert_eq!(
            mem::size_of::<Value>(),
            mem::size_of::<PackedOption<Value>>()
        );
    }

    #[test]
    fn constant_with_number() {
        assert_eq!(Constant::with_number(0).unwrap().to_string(), "const0");
        assert_eq!(Constant::with_number(1).unwrap().to_string(), "const1");
    }
}
