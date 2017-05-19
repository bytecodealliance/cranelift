//! Cretonne file reader library.
//!
//! The cton_reader library supports reading .cton files. This functionality is needed for testing
//! Cretonne, but is not essential for a JIT compiler.

#![deny(missing_docs)]

extern crate cretonne;

pub mod function;
pub mod json;
