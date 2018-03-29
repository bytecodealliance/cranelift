//! Top-level lib.rs for `cton_simplejit`.

#![deny(missing_docs,
        trivial_numeric_casts,
        unused_extern_crates)]

extern crate cretonne;
extern crate cton_module;
//extern crate cton_native;
//extern crate region;

mod simplejit_backend;

pub use simplejit_backend::SimpleJITBackend;
