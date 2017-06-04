//! Function names.
//!
//! The name of a function doesn't have any meaning to Cretonne which compiles functions
//! independently.

use std::fmt::{self, Write};

/// The name of a function can be any UTF-8 string.
///
/// Function names are mostly a testing and debugging tool.
/// In particular, `.cton` files use function names to identify functions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FunctionName(String);

impl FunctionName {
    /// Creates a new function name from a string.
    ///
    /// Caller should validate that the string contains only
    /// ASCII alphanumerical characters and `_`.
    pub fn from_string(s: &str) -> FunctionName {
        FunctionName(s.into())
    }
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('%')?;
        f.write_str(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionName;

    #[test]
    fn formatting_string() {
        assert_eq!(FunctionName::from_string("").to_string(), "%");
        assert_eq!(FunctionName::from_string("x").to_string(), "%x");
        assert_eq!(FunctionName::from_string(" ").to_string(), "% ");
    }
}
