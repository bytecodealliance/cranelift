#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(not(feature = "std"), feature(alloc))]
//! Cretonne file reader library.
//!
//! The cton_reader library supports reading .cton files. This functionality is needed for testing
//! Cretonne, but is not essential for a JIT compiler.

#![deny(missing_docs)]

extern crate cretonne;

#[cfg(not(feature = "std"))]
#[macro_use] extern crate alloc;

#[cfg(not(feature = "std"))]
extern crate hashmap_core;

#[cfg(not(feature = "std"))]
mod std {
    pub use core::{fmt, result, u16, u32, str, mem};
    pub use alloc::{vec, string, boxed};
    pub mod collections {
        pub use hashmap_core::HashMap;
    }
}

pub use error::{Location, Result, Error};
pub use parser::{parse_functions, parse_test};
pub use testcommand::{TestCommand, TestOption};
pub use testfile::{TestFile, Details, Comment};
pub use isaspec::{IsaSpec, parse_options};
pub use sourcemap::SourceMap;

mod error;
mod lexer;
mod parser;
mod testcommand;
mod isaspec;
mod testfile;
mod sourcemap;
