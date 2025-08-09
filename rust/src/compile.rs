use crate::label_loops;
use crate::lex;
use crate::optimizations::optimize::optimize as optimize_tacky;
use crate::parse::{self, ParseError};
use crate::resolve;
use crate::settings::{Optimizations, Stage};
use crate::tacky_gen;
use crate::tacky_print;
use crate::typecheck;
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
/// This translates the OCaml `compile.ml` pipeline into Rust but leaves the
/// optimisation passes as stubs.  The compilation can be driven up to the
/// generation of TACKY IR which is sufficient for consumers that only need the
/// semantic analysis infrastructure.
pub fn compile(
    stage: Stage,
    optimizations: Optimizations,
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

    // Semantic analysis steps
    let resolved_ast = resolve::resolve(ast);
    let annotated_ast = label_loops::label_loops(resolved_ast);
    let typed_ast = typecheck::typecheck(annotated_ast);
    if stage == Stage::Validate {
        return Ok(());
    }

    // Generate TACKY IR and optionally optimise it (optimisations are stubbed)
    let tacky = tacky_gen::r#gen(typed_ast);
    tacky_print::debug_print_tacky(src_file, &tacky);
    let _tacky = optimize_tacky(optimizations, src_file, tacky);
    if stage == Stage::Tacky {
        return Ok(());
    }

    // Later stages such as code generation are intentionally left unimplemented
    // in this educational port.
    Ok(())
}

