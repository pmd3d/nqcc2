use crate::assembly::{Program as AsmProgram, TopLevel as AsmTopLevel};
use crate::tacky::{Program as TackyProgram, TopLevel as TackyTopLevel};
use crate::type_utils;

/// Translate a TACKY program into assembly.
///
/// This is a skeletal implementation that mirrors the structure of the OCaml
/// `codegen.ml` module.  The full instruction selection and lowering logic has
/// not yet been ported, but the function and variable level scaffolding is in
/// place so that later passes can build upon it.  Optimisation stages remain
/// intentionally stubbed.
pub fn generate(prog: TackyProgram) -> AsmProgram {
    let tls = prog
        .0
        .into_iter()
        .map(|tl| match tl {
            TackyTopLevel::Function { name, global, .. } => AsmTopLevel::Function {
                name,
                global,
                instructions: Vec::new(),
            },
            TackyTopLevel::StaticVariable { name, t, global, init } => AsmTopLevel::StaticVariable {
                name,
                global,
                alignment: type_utils::get_alignment(&t) as i32,
                init,
            },
            TackyTopLevel::StaticConstant { name, t, init } => AsmTopLevel::StaticConstant {
                name,
                alignment: type_utils::get_alignment(&t) as i32,
                init,
            },
        })
        .collect();

    AsmProgram(tls)
}
