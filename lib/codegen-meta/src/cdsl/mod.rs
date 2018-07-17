//! Cranelift DSL classes.
//!
//! This module defines the classes that are used to define Cranelift
//! instructions and other entitties.

pub mod types;

/// Convert the string `s` to CamelCase.
fn _camel_case(s: &str) -> String {
    let mut output_chars = String::with_capacity(s.len());

    let mut capitalize = true;
    for curr_char in s.chars() {
        if curr_char == '_' {
            capitalize = true;
        } else {
            if capitalize {
                output_chars.extend(curr_char.to_uppercase());
            } else {
                output_chars.push(curr_char);
            }
            capitalize = false;
        }
    }

    output_chars
}

/// Check if `x` is a power of two.
fn _is_power_of_two(x: u8) -> bool {
    x > 0 && x & (x - 1) == 0
}

/// Compute the next power of two that is greater than `x`.
fn _next_power_of_two(x: u8) -> u8 {
    let mut s = 1;
    let mut res = x;
    while res & (res + 1) != 0 {
        res |= res >> s;
        s *= 2;
    }

    res + 1
}

#[cfg(test)]
mod tests {
    use super::_camel_case as camel_case;
    use super::_is_power_of_two as is_power_of_two;
    use super::_next_power_of_two as next_power_of_two;

    #[test]
    fn camel_case_works() {
        assert_eq!(camel_case("x"), "X");
        assert_eq!(camel_case("camel_case"), "CamelCase");
    }

    #[test]
    fn is_power_of_two_works() {
        assert_eq!(is_power_of_two(1), true);
        assert_eq!(is_power_of_two(2), true);
        assert_eq!(is_power_of_two(4), true);
        assert_eq!(is_power_of_two(8), true);

        assert_eq!(is_power_of_two(3), false);
        assert_eq!(is_power_of_two(7), false);
    }

    #[test]
    fn next_power_of_two_works() {
        assert_eq!(next_power_of_two(0), 1);
        assert_eq!(next_power_of_two(1), 2);
        assert_eq!(next_power_of_two(2), 4);
        assert_eq!(next_power_of_two(3), 4);
        assert_eq!(next_power_of_two(4), 8);
    }
}
