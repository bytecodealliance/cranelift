//! Defines `DataContext`.

use cretonne::entity::PrimaryMap;
use cretonne::binemit::{CodeOffset, Addend};
use cretonne::ir;

/// This specifies how data is to be initialized.
#[derive(PartialEq, Eq, Debug)]
pub enum Init {
    /// This indicates that no initialization has been specified yet.
    Uninitialized,
    /// Initialize the data with all zeros.
    Zeros {
        /// The size of the data.
        size: usize,
    },
    /// Initialize the data with the specified contents.
    Bytes {
        /// The contents, which also implies the size of the data.
        contents: Box<[u8]>,
    },
}

impl Init {
    /// Return the size of the data to be initialized.
    pub fn size(&self) -> usize {
        match *self {
            Init::Uninitialized => panic!("data size not initialized yet"),
            Init::Zeros { size } => size,
            Init::Bytes { ref contents } => contents.len(),
        }
    }
}

/// A flag specifying whether data is readonly or may be written to.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Writability {
    /// Data is readonly, meaning writes to it will trap.
    Readonly,
    /// Data is writable.
    Writable,
}

/// A description of a data object.
pub struct DataDescription {
    /// Whether the data readonly or writable.
    pub writable: Writability,
    /// How the data should be initialized.
    pub init: Init,
    /// External function declarations.
    pub function_decls: PrimaryMap<ir::FuncRef, ir::ExternalName>,
    /// External data object declarations.
    pub data_decls: PrimaryMap<ir::GlobalVar, ir::ExternalName>,
    /// Function addresses to write at specified offsets.
    pub function_relocs: Vec<(CodeOffset, ir::FuncRef)>,
    /// Data addresses to write at specified offsets.
    pub data_relocs: Vec<(CodeOffset, ir::GlobalVar, Addend)>,
}

/// This is to data objects what cretonne::Context is to functions.
pub struct DataContext {
    description: DataDescription,
}

impl DataContext {
    /// Allocate a new context.
    pub fn new() -> Self {
        Self {
            description: DataDescription {
                writable: Writability::Readonly,
                init: Init::Uninitialized,
                function_decls: PrimaryMap::new(),
                data_decls: PrimaryMap::new(),
                function_relocs: Vec::new(),
                data_relocs: Vec::new(),
            },
        }
    }

    /// Clear all data structures in this context.
    pub fn clear(&mut self) {
        self.description.writable = Writability::Readonly;
        self.description.init = Init::Uninitialized;
        self.description.function_decls.clear();
        self.description.data_decls.clear();
        self.description.function_relocs.clear();
        self.description.data_relocs.clear();
    }

    /// Define a zero-initialized object with the given size.
    pub fn define_zeroinit(&mut self, size: usize, writable: Writability) {
        self.description.writable = writable;
        self.description.init = Init::Zeros { size };
    }

    /// Define a zero-initialized object with the given size.
    ///
    /// TODO: Can we avoid a Box here?
    pub fn define(&mut self, contents: Box<[u8]>, writable: Writability) {
        self.description.writable = writable;
        self.description.init = Init::Bytes { contents };
    }

    /// Declare an external function import.
    pub fn import_function(&mut self, name: ir::ExternalName) -> ir::FuncRef {
        self.description.function_decls.push(name)
    }

    /// Declares a global variable import.
    pub fn import_global_var(&mut self, name: ir::ExternalName) -> ir::GlobalVar {
        self.description.data_decls.push(name)
    }

    /// Write the address of `func` into the data at offset `offset`.
    pub fn write_function_addr(&mut self, offset: CodeOffset, func: ir::FuncRef) {
        self.description.function_relocs.push((offset, func))
    }

    /// Write the address of `data` into the data at offset `offset`.
    pub fn write_data_addr(&mut self, offset: CodeOffset, data: ir::GlobalVar, addend: Addend) {
        self.description.data_relocs.push((offset, data, addend))
    }

    /// Reference the initializer data.
    pub fn description(&self) -> &DataDescription {
        debug_assert!(
            self.description.init != Init::Uninitialized,
            "data must be initialized first"
        );
        &self.description
    }
}
