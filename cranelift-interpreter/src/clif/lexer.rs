//! Cranelift annotation lexer.
//!
//! A lexer for the CLIF test language (e.g. `run: fn(1) == 2`). This is loosely based on Alex's
//! work in [wat].
//! [wat]: https://github.com/bytecodealliance/wat/blob/1b810cb430ee1f1bb11a7641e560a30679e95754/crates/wast/src/lexer.rs
//!
//! ```
//! use cranelift_interpreter::clif::lexer::{Lexer, Token};
//! let tokens: Vec<Token> = Lexer::new("true false %fn0 ()").collect();
//! println!("{:?}", tokens);
//! ```

use std::iter::Peekable;
use std::path::PathBuf;
use thiserror::Error;

/// A lexer for the CLIF test language (e.g. `run: fn(1) == 2`).
pub struct Lexer<'a> {
    it: Peekable<std::str::CharIndices<'a>>,
    input: &'a str,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer which will lex the `input` source string.
    pub fn new(input: &str) -> Lexer<'_> {
        Lexer {
            it: input.char_indices().peekable(),
            input,
        }
    }

    /// Retrieve the next token from the input.
    pub fn token(&mut self) -> Result<Option<Token<'a>>, LexerError> {
        let token;
        loop {
            let pos = self.cur();
            token = match self.peek() {
                Some(_) if self.has_keyword(pos, "true") => Some(self.keyword("true")?),
                Some(_) if self.has_keyword(pos, "false") => Some(self.keyword("false")?),
                Some(_) if self.has_keyword(pos, "NaN") => Some(self.keyword("NaN")?),
                Some(_) if self.has_keyword(pos, "Inf") => Some(self.keyword("Inf")?),
                Some(b) if b.is_numeric() || b == '-' => Some(self.number()?),
                Some(b) if b.is_ascii_punctuation() && b != '%' => Some(self.punctuation()?),
                Some(b) if b.is_alphabetic() || b == '%' => Some(self.name()?),
                Some(_) => {
                    self.it.next();
                    continue;
                } // skip unknown characters by repeating the loop
                _ => None,
            };
            break;
        }
        Ok(token)
    }

    /// Consume characters from the input while the condition is true; returns the start and end
    /// indexes of the consumed string.
    fn eat_while<F>(&mut self, condition: F) -> (usize, usize)
    where
        F: Fn(char) -> bool,
    {
        let start = self.cur();
        while let Some(ch) = self.peek() {
            if condition(ch) {
                self.it.next();
            } else {
                break;
            }
        }
        let end = self.cur();
        (start, end)
    }

    /// Tokenize a `Number`.
    fn number(&mut self) -> Result<Token<'a>, LexerError> {
        // FIXME this implementation has various issues to fix:
        // - does not yet handle hex values, e.g. 0x...
        // - allows multiple - and . in the string
        let (start, end) = self.eat_while(|c| c.is_numeric() || c == '-' || c == '.');
        self.expect_span_is_not_empty(start, end)?;
        let span = &self.input[start..end];
        Ok(Token::new(TokenKind::Number, span))
    }

    /// Tokenize `Punctuation`.
    fn punctuation(&mut self) -> Result<Token<'a>, LexerError> {
        let start = self.cur();
        if let Some(c) = self.peek() {
            if c.is_ascii_punctuation() {
                self.it.next();
                return Ok(Token::new(
                    TokenKind::Punctuation,
                    &self.input[start..=start],
                ));
            }
        }
        Err(self.error(start, LexerErrorKind::NotFound))
    }

    /// Tokenize an `Entity`.
    fn name(&mut self) -> Result<Token<'a>, LexerError> {
        let (start, end) = self.eat_while(|c| c.is_alphanumeric() || c == '_' || c == '%');
        self.expect_span_is_not_empty(start, end)?;
        let span = &self.input[start..end];
        Ok(Token::new(TokenKind::Name, span))
    }

    /// Check if the character stream has the keyword `text` at position `pos`.
    fn has_keyword(&self, pos: usize, text: &str) -> bool {
        self.input[pos..].starts_with(text)
    }

    // Tokenize a `Keyword`.
    fn keyword(&mut self, text: &str) -> Result<Token<'a>, LexerError> {
        let span = &self.input[self.cur()..self.cur() + text.len()];
        self.it.nth(text.len() - 1);
        Ok(Token::new(TokenKind::Keyword, span))
    }

    /// Raise an error if the span is empty.
    fn expect_span_is_not_empty(&self, start: usize, end: usize) -> Result<(), LexerError> {
        if start == end {
            Err(self.error(start, LexerErrorKind::NotFound))
        } else {
            Ok(())
        }
    }

    /// Peek at the next character in the input stream; returns None if no character found.
    fn peek(&mut self) -> Option<char> {
        self.it.peek().map(|(_, c)| *c)
    }

    /// Return the current position of our iterator through the input string.
    fn cur(&mut self) -> usize {
        self.it
            .peek()
            .map(|p| p.0)
            .unwrap_or_else(|| self.input.len())
    }

    /// Return the position `pos` as a 0-based line and column pair.
    fn cur_as_linecol(&self, pos: usize) -> (usize, usize) {
        let mut cur = 0;
        for (i, line) in self.input.split_terminator('\n').enumerate() {
            if cur + line.len() + 1 > pos {
                return (i, pos - cur);
            }
            cur += line.len() + 1;
        }
        (self.input.lines().count(), 0)
    }

    /// Create an error at the current position with the specified `kind`.
    fn error(&self, pos: usize, kind: LexerErrorKind) -> LexerError {
        let (line, col) = self.cur_as_linecol(pos);
        let snippet = self.input.lines().nth(line).expect("a line").to_string();
        let text = Text { line, col, snippet };
        LexerError {
            text,
            kind,
            file: None,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token().expect("a token")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, text: &'a str) -> Self {
        Self { kind, text }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword,
    Name,
    Number,
    Punctuation,
}

#[derive(Error, Debug)]
#[error("lexer failure")]
pub struct LexerError {
    text: Text,
    file: Option<PathBuf>,
    kind: LexerErrorKind,
}

#[derive(Debug, PartialEq)]
pub enum LexerErrorKind {
    NotFound,
}

#[derive(Debug)]
pub struct Text {
    line: usize,
    col: usize,
    snippet: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(s: &str) -> Token<'_> {
        Lexer::new(s).token().unwrap().unwrap()
    }
    fn lex_all(s: &str) -> Vec<Token<'_>> {
        Lexer::new(s).collect()
    }
    fn name(text: &str) -> Token {
        Token::new(TokenKind::Name, text)
    }
    fn number(text: &str) -> Token {
        Token::new(TokenKind::Number, text)
    }
    fn punctuation(text: &str) -> Token {
        Token::new(TokenKind::Punctuation, text)
    }
    fn keyword(text: &str) -> Token {
        Token::new(TokenKind::Keyword, text)
    }

    #[test]
    fn lex_name() {
        assert_eq!(lex(" fn32 "), name("fn32"));
    }

    #[test]
    fn lex_parentheses() {
        assert_eq!(lex_all(" ()"), vec![punctuation("("), punctuation(")")]);
    }

    #[test]
    fn lex_integer() {
        assert_eq!(lex(" 42 "), number("42"));
    }

    #[test]
    fn lex_float() {
        assert_eq!(lex(" -0.32 "), number("-0.32"));
    }

    #[test]
    fn lex_boolean() {
        assert_eq!(
            lex_all(" truefalse "),
            vec![keyword("true"), keyword("false")]
        );
    }

    #[test]
    fn lex_function_invocation() {
        assert_eq!(
            lex_all(" fn32(42, 43) \n "),
            vec![
                name("fn32"),
                punctuation("("),
                number("42"),
                punctuation(","),
                number("43"),
                punctuation(")")
            ]
        );
    }

    #[test]
    fn not_found() {
        let mut lexer = Lexer::new(" ");
        let result = lexer.number();
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind, LexerErrorKind::NotFound);
    }

    #[test]
    fn external_name() {
        assert_eq!(lex("%fn0"), Token::new(TokenKind::Name, "%fn0"))
    }
}
