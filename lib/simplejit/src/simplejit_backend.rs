//! Defines `SimpleJITBackend`.

use cretonne::{self, ir, binemit};
use cton_module::{Backend, Linkage, DataContext};
use std::mem;

struct RelocRecord {
    kind: binemit::Reloc,
    offset: binemit::CodeOffset,
    name: ir::ExternalName,
    addend: binemit::Addend,
}

pub struct SimpleJITCompiledFunction {
    code: Vec<u8>,
    relocs: Vec<RelocRecord>,
}

pub struct SimpleJITCompiledData {
    storage: Vec<u8>,
    size: usize,
    relocs: Vec<RelocRecord>,
}

/// A `SimpleJITBackend` implements `Backend` and emits code and data into memory where it can be
/// directly called and accessed.
pub struct SimpleJITBackend {}

impl SimpleJITBackend {
    /// Create a new `SimpleJITBackend`.
    pub fn new() -> Self {
        Self {}
    }

    /// Return a pointer to the function referred to by `func`, which can be transmuted into a Rust
    /// function type and called.
    pub fn get_raw_funcaddr(&self, func: &SimpleJITCompiledFunction) -> *const u8 {
        func.code.as_ptr()
    }

    /// Return a function pointer for `func`.
    ///
    /// TODO: Check that `func`'s signature matches `FuncTy` and make this safe.
    pub unsafe fn get_funcaddr<FuncTy>(&self, func: &SimpleJITCompiledFunction) -> *const FuncTy
    where
        FuncTy: Sized,
    {
        mem::transmute::<_, *const FuncTy>(func.code.as_ptr())
    }

    /// Return a data pointer for `data`, which can be transmuted into a Rust data type and loaded
    /// through.
    pub fn get_raw_dataaddr(&self, data: &SimpleJITCompiledData) -> *const u8 {
        data.storage.as_ptr()
    }

    /// Return a data pointer for `data`, which can be transmuted into a Rust data type and stored
    /// or loaded through.
    pub fn get_raw_dataaddr_mut(&self, data: &mut SimpleJITCompiledData) -> *mut u8 {
        data.storage.as_mut_ptr()
    }

    /// Return a data pointer for `data`, which can be transmuted into a Rust data type and loaded
    /// through. `data` must refer to a non-`Import` data object.
    ///
    /// TODO: Check that `data`'s length is sufficient and make this safe.
    pub fn get_dataaddr<'data>(&self, data: &'data SimpleJITCompiledData) -> &'data [u8] {
        &data.storage[0..data.size]
    }

    /// Return a data pointer for `data`, which can be transmuted into a Rust data type and stored
    /// or loaded through. `data` must refer to a non-`Import` data object.
    pub fn get_dataaddr_mut<'data>(
        &self,
        data: &'data mut SimpleJITCompiledData,
    ) -> &'data mut [u8] {
        &mut data.storage[0..data.size]
    }
}

impl Backend for SimpleJITBackend {
    type CompiledFunction = SimpleJITCompiledFunction;
    type CompiledData = SimpleJITCompiledData;

    fn define_function(
        &mut self,
        name: &str,
        ctx: &cretonne::Context,
    ) -> SimpleJITCompiledFunction {
        unimplemented!();
    }

    fn define_data(&mut self, name: &str, data: &DataContext) -> SimpleJITCompiledData {
        unimplemented!();
    }

    fn write_data_funcaddr(
        &mut self,
        data: &mut SimpleJITCompiledData,
        offset: usize,
        what: ir::FuncRef,
    ) {
        unimplemented!();
    }

    fn write_data_dataaddr(
        &mut self,
        data: &mut SimpleJITCompiledData,
        offset: usize,
        what: ir::GlobalVar,
        usize: binemit::Addend,
    ) {
        unimplemented!();
    }

    fn finalize_function(&mut self, func: &SimpleJITCompiledFunction) {
        unimplemented!();
    }

    fn finalize_data(&mut self, data: &SimpleJITCompiledData) {
        unimplemented!();
    }
}
