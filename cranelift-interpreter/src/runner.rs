//! Helper for running the Cranelift IR interpreter on CLIF files with annotations.

use crate::clif::parser::{ClifCommand, Comparison, Invoke, Parse, ParseBuffer, ParseFailure};
use crate::environment::Environment;
use crate::interpreter::{ControlFlow, Interpreter, Trap};
use crate::value::Value;
use cranelift_reader::{parse_test, ParseError, ParseOptions};
use log::debug;
use std::path::PathBuf;
use std::{fs, io};
use thiserror::Error;

/// Contains CLIF code that can be executed with [FileRunner::run].
pub struct FileRunner {
    path: Option<PathBuf>,
    contents: String,
}

impl FileRunner {
    /// Construct a file runner from a CLIF file path.
    pub fn from_path(path: impl Into<PathBuf>) -> Result<Self, io::Error> {
        let path = path.into();
        debug!("New file runner from path: {}:", path.to_string_lossy());
        let contents = fs::read_to_string(&path)?;
        Ok(Self {
            path: Some(path),
            contents,
        })
    }

    /// Construct a file runner from a CLIF code string.
    pub fn from_inline_code(contents: String) -> Self {
        debug!("New file runner from inline code: {}:", &contents[..20]);
        Self {
            path: None,
            contents,
        }
    }

    /// Return the path of the file runner or `[inline code]`.
    pub fn path(&self) -> String {
        match self.path {
            None => "[inline code]".to_string(),
            Some(ref p) => p.to_string_lossy().to_string(),
        }
    }

    /// Run the file; this searches for annotations like `; run: %fn0(42)` or
    /// `; test: %fn0(42) == 2` and executes them, performing any test comparisons if necessary.
    pub fn run(&self) -> Result<(), FileRunnerFailure> {
        // parse file
        let test = parse_test(&self.contents, ParseOptions::default())
            .map_err(|e| FileRunnerFailure::ParsingClif(self.path(), e))?;

        // collect functions
        let mut env = Environment::default();
        let mut comments = vec![];
        comments.append(&mut test.preamble_comments.clone());
        for (func, details) in test.functions.into_iter() {
            // FIXME func.name is truncating function name
            env.add(func.name.to_string(), func);
            comments.append(&mut details.comments.clone());
        }

        // run assertions
        let interpreter = Interpreter::new(env);
        for comment in comments {
            if !(comment.text.starts_with("; run:") || comment.text.starts_with("; test:")) {
                continue;
            }
            let mut parse_buffer = ParseBuffer::new(comment.text);
            if let Some(ref p) = self.path {
                parse_buffer = parse_buffer.with_file(p);
            }
            let command = ClifCommand::parse(parse_buffer.parser())?;
            match command {
                ClifCommand::Run(invoke) => {
                    let results = invoke_function(&invoke, &interpreter)?;
                    println!("{:?}", results)
                }
                ClifCommand::Test(invoke, compare, values) => {
                    let results = invoke_function(&invoke, &interpreter)?;
                    let pass = match compare {
                        Comparison::Equals => values == results,
                        Comparison::NotEquals => values != results,
                    };
                    if !pass {
                        let expected = format!("{:?} {:?} {:?}", invoke, compare, values);
                        let actual = format!("{:?}", results);
                        return Err(FileRunnerFailure::FailedAssertion(expected, actual));
                    }
                }
            }
        }
        Ok(())
    }

    // TODO add call(name, args) -> returns
}

/// Helper for invoking the function.
fn invoke_function(
    invocation: &Invoke,
    interpreter: &Interpreter,
) -> Result<Vec<Value>, FileRunnerFailure> {
    match interpreter.call_by_name(&invocation.func, &invocation.arguments)? {
        ControlFlow::Return(results) => Ok(results),
        _ => panic!("Unexpected returned control flow--this is likely a bug."),
    }
}

/// Possible sources of failure in this file.
#[derive(Error, Debug)]
pub enum FileRunnerFailure {
    // reading
    #[error("failure reading file")]
    Io(#[from] io::Error),
    #[error("failure parsing file {0}: {1}")]
    ParsingClif(String, ParseError),
    #[error("failure parsing test annotations: {0}")]
    ParsingClifAnnotation(#[from] ParseFailure),

    // executing
    #[error("failed interpretation")]
    Trap(#[from] Trap),
    #[error("Unknown function: {0}")]
    UnknownFunction(String),
    #[error("expected: {0}, found: {1}")]
    FailedAssertion(String, String),
}
