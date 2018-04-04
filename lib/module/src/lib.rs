//! Top-level lib.rs for `cton_module`.

#![deny(missing_docs, trivial_numeric_casts, unused_extern_crates)]

extern crate cretonne;
#[macro_use]
extern crate cton_entity;

mod backend;
mod data_context;
mod module;

pub use backend::Backend;
pub use data_context::DataContext;
pub use module::{DataId, FuncId, Linkage, Module, ModuleNamespace};
