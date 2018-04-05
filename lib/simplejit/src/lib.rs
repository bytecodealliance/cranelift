//! Top-level lib.rs for `cton_simplejit`.

#![deny(missing_docs, trivial_numeric_casts, unused_extern_crates)]

extern crate cretonne;
extern crate cton_module;
extern crate cton_native;
extern crate errno;
extern crate region;
extern crate libc;

mod backend;
mod memory;

pub use backend::SimpleJITBackend;
