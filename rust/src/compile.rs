use crate::lex;
use crate::parse::{self, ParseError};
use crate::settings::{Optimizations, Stage};
use std::fs;

/// Errors that can occur while compiling a source file.
#[derive(Debug)]
pub enum CompileError {
    Io(std::io::Error),
    Lex(String),
    Parse(ParseError),
}

/// Compile a source file through the requested stage.
///
/// The Rust port currently supports lexing and parsing stages. Later
/// stages are stubs so that other modules can build on top of this
/// interface.
pub fn compile(
    stage: Stage,
    _optimizations: Optimizations,
    src_file: &str,
) -> Result<(), CompileError> {
    let source = fs::read_to_string(src_file).map_err(CompileError::Io)?;
    let tokens = lex::lex(&source).map_err(CompileError::Lex)?;
    if stage == Stage::Lex {
        return Ok(());
    }
    let ast = parse::parse(tokens).map_err(CompileError::Parse)?;
    if stage == Stage::Parse {
        println!("{:?}", ast);
        return Ok(());
    }
    // Further stages are not yet implemented in the Rust version.
    Ok(())
}
