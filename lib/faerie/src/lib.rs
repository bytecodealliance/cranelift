//! Top-level lib.rs for `cton_faerie`.

#![deny(missing_docs,
        trivial_numeric_casts,
        unused_extern_crates)]

extern crate cretonne;
extern crate cton_module;
extern crate faerie;
#[macro_use]
extern crate failure;

mod faerie_backend;
mod target;

pub use faerie_backend::FaerieBackend;
