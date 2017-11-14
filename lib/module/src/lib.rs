//! Support for working with multiple functions and data objects at
//! once.

#![deny(missing_docs)]

extern crate cretonne;
extern crate ordermap;

mod module;

pub use module::{DataSymbol, Compilation, CompiledFunction, DisplayCompiledFunction, Module};
