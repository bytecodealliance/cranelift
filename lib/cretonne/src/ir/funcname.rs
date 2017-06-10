//! Function names.
//!
//! The name of a function doesn't have any meaning to Cretonne which compiles functions
//! independently.

use std::fmt::{self, Write};
use std::ascii::AsciiExt;

/// The name of a function can be any sequence of bytes.
///
/// Function names are mostly a testing and debugging tool.
/// In particular, `.cton` files use function names to identify functions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FunctionName(Vec<u8>);

impl FunctionName {
    /// Creates a new function name from a sequence of bytes.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use cretonne::ir::FunctionName;
    /// // Create `FunctionName` from a string.
    /// let name = FunctionName::new("hello");
    /// assert_eq!(name.to_string(), "%hello");
    ///
    /// // Create `FunctionName` from a sequence of bytes.
    /// let bytes: &[u8] = &[10, 9, 8];
    /// let name = FunctionName::new(bytes);
    /// assert_eq!(name.to_string(), "#0a0908");
    /// ```
    pub fn new<T>(v: T) -> FunctionName
        where T: Into<Vec<u8>>
    {
        FunctionName(v.into())
    }
}

/// Tries to interpret bytes as ASCII alphanumerical characters and `_`.
fn try_as_name(bytes: &[u8]) -> Option<String> {
    let mut name = String::with_capacity(bytes.len());
    for c in bytes.iter().map(|&b| b as char) {
        if c.is_ascii() && c.is_alphanumeric() || c == '_' {
            name.push(c);
        } else {
            return None;
        }
    }
    Some(name)
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = try_as_name(&self.0) {
            write!(f, "%{}", name)
        } else {
            f.write_char('#')?;
            for byte in &self.0 {
                write!(f, "{:02x}", byte)?;
            }
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FunctionName;

    #[test]
    fn displaying() {
        assert_eq!(FunctionName::new("").to_string(), "%");
        assert_eq!(FunctionName::new("x").to_string(), "%x");
        assert_eq!(FunctionName::new("x_1").to_string(), "%x_1");
        assert_eq!(FunctionName::new(" ").to_string(), "#20");
        assert_eq!(FunctionName::new("кретон").to_string(),
                   "#d0bad180d0b5d182d0bed0bd");
        assert_eq!(FunctionName::new("印花棉布").to_string(),
                   "#e58db0e88ab1e6a389e5b883");
        assert_eq!(FunctionName::new(vec![0, 1, 2, 3, 4, 5]).to_string(),
                   "#000102030405");
    }
}
