//! Cranelift annotation parser.
//!
//! This loosely follows the pattern used by Alex's parser in [wat].
//! [wat]: https://github.com/bytecodealliance/wat/blob/1b810cb430ee1f1bb11a7641e560a30679e95754/crates/wast/src/parser.rs
//!
//! ```
//! use cranelift_interpreter::clif::parser::{Parse, ParseBuffer, ClifCommand};
//! let buffer = ParseBuffer::new("; test: %add1(1) == 2");
//! let command = ClifCommand::parse(buffer.parser()).unwrap();
//! println!("{:?}", command);
//! ```

use crate::clif::lexer::{Lexer, LexerError, Token, TokenKind};
use crate::value::Value;
use std::cell::Cell;
use std::path::PathBuf;
use std::str::FromStr;
use thiserror::Error;

/// Describes the ways the parser may fail. TODO add location of failure within the original text.
#[derive(Error, Debug)]
pub enum ParseFailure {
    #[error("lexing failure")]
    LexerFailure(#[from] LexerError),
    #[error("no token available for parsing")]
    NoToken,
    #[error("no value found")]
    NoValue,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("unknown command")]
    UnknownCommand,
    #[error("unknown operator")]
    UnknownOperator,
}

pub trait Parse<'a>: Sized {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure>;
}

/// The structure owning the lexed tokens and cursor state.
pub struct ParseBuffer<'a> {
    file: Option<PathBuf>,
    tokens: Box<[Token<'a>]>,
    cur: Cell<usize>,
}

impl<'a> ParseBuffer<'a> {
    /// Construct a parse buffer, immediately tokenizing the text.
    pub fn new(text: &'a str) -> Self {
        let tokens: Vec<Token<'a>> = Lexer::new(text).collect();
        Self {
            file: None,
            tokens: tokens.into_boxed_slice(),
            cur: Cell::new(0),
        }
    }

    /// Assign a file path to the parse buffer (for better error messages).
    pub fn with_file(mut self, file: impl Into<PathBuf>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// Create a parser using a reference to this buffer.
    pub fn parser(&self) -> Parser<'_> {
        Parser { buffer: self }
    }
}

/// A reference to the buffer; contains the parser implementation.
#[derive(Clone, Copy)]
pub struct Parser<'a> {
    buffer: &'a ParseBuffer<'a>,
}

impl<'a> Parser<'a> {
    /// Inspect the next token without consuming it.
    pub fn peek(self) -> Option<Token<'a>> {
        let cursor = self.buffer.cur.get();
        self.buffer.tokens.get(cursor).cloned()
    }

    /// Consume the next token.
    pub fn next(self) -> Option<Token<'a>> {
        let cursor = self.buffer.cur.get();
        self.buffer.cur.set(cursor + 1);
        self.buffer.tokens.get(cursor).cloned()
    }

    /// Consume the next token if it matches the expected `TokenKind`; otherwise fail.
    pub fn expect(self, expected: TokenKind) -> Result<Token<'a>, ParseFailure> {
        match self.next() {
            None => Err(ParseFailure::NoToken),
            Some(t) => {
                if t.kind == expected {
                    Ok(t)
                } else {
                    Err(ParseFailure::UnexpectedToken)
                }
            }
        }
    }

    /// Consume the next token if it matches the expected `Token`; otherwise fail.
    pub fn expect_exact(self, expected: Token<'a>) -> Result<Token<'a>, ParseFailure> {
        match self.next() {
            None => Err(ParseFailure::NoToken),
            Some(t) => {
                if t == expected {
                    Ok(t)
                } else {
                    Err(ParseFailure::UnexpectedToken)
                }
            }
        }
    }

    /// Check if the next token matches the expected `TokenKind`.
    pub fn has_next(self, expected: TokenKind) -> bool {
        match self.peek() {
            Some(ref t) if t.kind == expected => true,
            _ => false,
        }
    }

    /// Check if the next token matches the expected `Token`.
    pub fn has_next_exact(self, expected: Token) -> bool {
        match self.peek() {
            Some(ref t) if t == &expected => true,
            _ => false,
        }
    }
}

/// The top-level CLIF command; e.g. `test: %fn(0) == 1`.
#[derive(Debug, PartialEq)]
pub enum ClifCommand {
    Run(Invoke),
    Test(Invoke, Comparison, Vec<Value>),
}

impl<'a> Parse<'a> for ClifCommand {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure> {
        // Skip initial comment delimiters, ';'.
        while parser.has_next_exact(Token::new(TokenKind::Punctuation, ";")) {
            parser.next();
        }

        // Parse command, '[run|test]:'
        let command = parser.expect(TokenKind::Name)?;
        parser.expect_exact(Token::new(TokenKind::Punctuation, ":"))?;

        // Parse command contents.
        match command.text {
            "run" => Ok(Self::Run(Invoke::parse(parser)?)),
            "test" => Ok(Self::Test(
                Invoke::parse(parser)?,
                Comparison::parse(parser)?,
                Vec::<Value>::parse(parser)?,
            )),
            _ => Err(ParseFailure::UnknownCommand),
        }
    }
}

/// A CLIF function invocation; e.g. `%fn(0)`.
#[derive(Debug, PartialEq)]
pub struct Invoke {
    pub func: String,
    pub arguments: Vec<Value>,
}

impl<'a> Parse<'a> for Invoke {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure> {
        let function_name = parser.expect(TokenKind::Name)?.text.to_string();

        parser.expect_exact(Token::new(TokenKind::Punctuation, "("))?;
        let arguments = Vec::<Value>::parse(parser)?;
        parser.expect_exact(Token::new(TokenKind::Punctuation, ")"))?;

        Ok(Self {
            func: function_name,
            arguments,
        })
    }
}

/// A CLIF comparison operation; e.g. `==`.
#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equals,
    NotEquals,
}

impl<'a> Parse<'a> for Comparison {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure> {
        let first = parser.expect(TokenKind::Punctuation)?;
        let second = parser.expect(TokenKind::Punctuation)?;
        match (first.text, second.text) {
            ("=", "=") => Ok(Comparison::Equals),
            ("!", "=") => Ok(Comparison::NotEquals),
            _ => Err(ParseFailure::UnknownOperator),
        }
    }
}

/// To avoid mixing parsing with interpreter functionality, this implementation is included here
/// instead of the `value` module.
impl<'a> Parse<'a> for Value {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure> {
        match parser.peek() {
            Some(Token {
                kind: TokenKind::Number,
                text,
            }) => {
                parser.next();
                let n = i64::from_str(text).expect("a valid integer");
                Ok(Value::Int(n))
            }
            Some(Token {
                kind: TokenKind::Keyword,
                text,
            }) => match text {
                "true" => {
                    parser.next();
                    Ok(Value::Bool(true))
                }
                "false" => {
                    parser.next();
                    Ok(Value::Bool(false))
                }
                _ => Err(ParseFailure::NoValue),
            },
            _ => Err(ParseFailure::NoValue),
        }
    }
}

fn has_next_value(parser: Parser<'_>) -> bool {
    if let Some(token) = parser.peek() {
        token.kind == TokenKind::Number
            || (token.kind == TokenKind::Keyword && (token.text == "true" || token.text == "false"))
    } else {
        false
    }
}

/// Parse comma-delimited sequences of values; e.g. `42, 43, false`
impl<'a> Parse<'a> for Vec<Value> {
    fn parse(parser: Parser<'a>) -> Result<Self, ParseFailure> {
        // TODO this likely should distinguish between a single value (e.g. `42`) and multiple
        // values (e.g. bracketed as in `[42, 43, 44]`)
        let mut values = Vec::new();
        // TODO must parse according to signature; e.g. in `fn(2000)` the `2000` could be either a
        // float or integer, or could be out of range for an 8-bit integer
        while has_next_value(parser) {
            values.push(Value::parse(parser)?);
            if !parser.has_next_exact(Token::new(TokenKind::Punctuation, ",")) {
                break;
            } else {
                parser.next();
            }
        }
        Ok(values)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_command() {
        let buffer = ParseBuffer::new("run: fn1(2, false)");
        let command = ClifCommand::parse(buffer.parser()).unwrap();
        assert_eq!(
            command,
            ClifCommand::Run(Invoke {
                func: "fn1".to_string(),
                arguments: vec![Value::Int(2), Value::Bool(false)]
            })
        );
    }

    #[test]
    fn test_command() {
        let buffer = ParseBuffer::new("; test: fibonacci_recursive(6) == 8");
        let command = ClifCommand::parse(buffer.parser()).unwrap();
        assert_eq!(
            command,
            ClifCommand::Test(
                Invoke {
                    func: "fibonacci_recursive".to_string(),
                    arguments: vec![Value::Int(6)]
                },
                Comparison::Equals,
                vec![Value::Int(8)]
            )
        );
    }
}
