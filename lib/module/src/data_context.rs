//! Defines `DataContext`.

use cretonne::entity::PrimaryMap;
use cretonne::ir;

/// TODO: Use this.
pub enum _Init<'a> {
    Zeros { size: usize },
    Bytes { contents: &'a [u8] },
}

/// This is to data objects what cretonne::Context is to functions.
///
/// TODO: Is this needed, or should we just do this through `Module`?
pub struct DataContext {
    function_addresses: PrimaryMap<ir::FuncRef, ir::ExternalName>,
    data_addresses: PrimaryMap<ir::GlobalVar, ir::ExternalName>,
}

impl DataContext {
    /// Declare an external function import.
    pub fn import_function(&mut self, name: ir::ExternalName) -> ir::FuncRef {
        self.function_addresses.push(name)
    }

    /// Declares a global variable import.
    pub fn import_global_var(&mut self, name: ir::ExternalName) -> ir::GlobalVar {
        self.data_addresses.push(name)
    }
}
