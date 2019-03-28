//! External data structures needed for address transform.

use crate::DefinedFuncIndex;
use cranelift_codegen::ir;
use cranelift_entity::PrimaryMap;
use std::vec::Vec;

/// Single wasm source location to generated address mapping.
#[derive(Debug)]
pub struct InstructionAddressMap {
    /// Original source location.
    pub srcloc: ir::SourceLoc,

    /// Generated instructions offset.
    pub code_offset: usize,

    /// Generated instructions length.
    pub code_len: usize,
}

/// Function and its instructions addresses mappings.
#[derive(Debug)]
pub struct FunctionAddressMap {
    /// Instructions maps.
    /// The array is sorted by the InstructionAddressMap::code_offset field.
    pub instructions: Vec<InstructionAddressMap>,

    /// Function start source location (normally declaration).
    pub start_srcloc: ir::SourceLoc,

    /// Function end source location.
    pub end_srcloc: ir::SourceLoc,

    /// Generated function body offset if applicable, otherwise 0.
    pub body_offset: usize,

    /// Generated function body length.
    pub body_len: usize,
}

/// Module functions addresses mappings.
pub type ModuleAddressMap = PrimaryMap<DefinedFuncIndex, FunctionAddressMap>;
