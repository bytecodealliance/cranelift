//! Defines `FaerieBackend`.

use cretonne::{self, ir, binemit};
use cretonne::isa::TargetIsa;
use cton_module::{Backend, Linkage, FuncId, DataId, DataContext};
use failure::Error;
use faerie;
use std::fs::File;
use target;

pub struct FaerieCompiledFunction {}

pub struct FaerieCompiledData {}

/// A `FaerieBackend` implements `Backend` and emits ".o" files using the `faerie` library.
pub struct FaerieBackend {
    artifact: faerie::Artifact,
}

impl FaerieBackend {
    /// Create a new `FaerieBackend`.
    pub fn new(target: faerie::Target, name: String) -> Self {
        Self { artifact: faerie::Artifact::new(target, name) }
    }

    /// Create a new `FaerieBackend` using the given Cretonne target.
    pub fn with_isa(isa: &TargetIsa, name: String) -> Result<Self, Error> {
        Ok(Self {
            artifact: faerie::Artifact::new(target::translate(isa)?, name),
        })
    }

    /// Call `emit` on the faerie `Artifact`, producing bytes in memory.
    pub fn emit<O: faerie::Object>(&self) -> Result<Vec<u8>, Error> {
        self.artifact.emit::<O>()
    }

    /// Call `write` on the faerie `Artifact`, writing to a file.
    pub fn write<O: faerie::Object>(&self, sink: File) -> Result<(), Error> {
        self.artifact.write::<O>(sink)
    }
}

impl Backend for FaerieBackend {
    type CompiledFunction = FaerieCompiledFunction;
    type CompiledData = FaerieCompiledData;

    fn define_function(&mut self, name: &str, ctx: &cretonne::Context) -> FaerieCompiledFunction {
        unimplemented!()
    }

    fn define_data(&mut self, name: &str, data: &DataContext) -> FaerieCompiledData {
        unimplemented!()
    }

    fn write_data_funcaddr(
        &mut self,
        data: &mut FaerieCompiledData,
        offset: usize,
        what: ir::FuncRef,
    ) {
        unimplemented!()
    }
    fn write_data_dataaddr(
        &mut self,
        data: &mut FaerieCompiledData,
        offset: usize,
        what: ir::GlobalVar,
        usize: binemit::Addend,
    ) {
        unimplemented!()
    }

    fn finalize_function(&mut self, func: &FaerieCompiledFunction) {
        unimplemented!()
    }
    fn finalize_data(&mut self, data: &FaerieCompiledData) {
        unimplemented!()
    }
}
