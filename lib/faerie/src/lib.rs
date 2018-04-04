//! Top-level lib.rs for `cton_faerie`.
//!
//! Users of this module should not have to depend on faerie directly.

#![deny(missing_docs, trivial_numeric_casts, unused_extern_crates)]

extern crate cretonne;
extern crate cton_module;
extern crate faerie;
#[macro_use]
extern crate failure;
extern crate goblin;

mod backend;
mod container;
mod target;

pub use backend::FaerieBackend;
pub use container::Format;
