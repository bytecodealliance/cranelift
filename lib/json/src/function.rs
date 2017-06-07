//! JSON serialization of a function.

use cretonne::ir::Function;
use cretonne::isa::TargetIsa;
use std::fmt::{self, Display, Formatter};
use json::json_function;

/// Wrapper type capable of displaying a `Function` with correct ISA annotations in JSON format.
pub struct JSONDisplayFunction<'a> {
    /// The function to display.
    pub func: &'a Function,
    /// The optional isa to display it with.
    pub isa: Option<&'a TargetIsa>,
}

impl<'a> Display for JSONDisplayFunction<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        json_function(fmt, self.func, self.isa)
    }
}
