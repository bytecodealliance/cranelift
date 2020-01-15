//! Cranelift annotation parser.
//!
//! This module parses CLIF annotation like `; test: %fn0(42) == true`; this allows executing and
//! testing function calls in CLIF files.
pub mod lexer;
pub mod parser;
