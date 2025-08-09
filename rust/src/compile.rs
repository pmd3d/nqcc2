use crate::lex;
use crate::parse;
use crate::settings::{Optimizations, Stage};
use std::fs;

/// Compile a source file through the requested stage.
///
/// The Rust port currently supports lexing and parsing stages. Later
/// stages are stubs so that other modules can build on top of this
/// interface.
pub fn compile(stage: Stage, _optimizations: Optimizations, src_file: &str) {
    let source = fs::read_to_string(src_file).expect("unable to read source file");
    let tokens = match lex::lex(&source) {
        Ok(t) => t,
        Err(_) => return,
    };
    if stage == Stage::Lex {
        return;
    }
    let ast = match parse::parse(tokens) {
        Ok(prog) => prog,
        Err(_) => return,
    };
    if stage == Stage::Parse {
        println!("{:?}", ast);
        return;
    }
    // Further stages are not yet implemented in the Rust version.
}
