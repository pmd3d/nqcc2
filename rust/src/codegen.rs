use crate::assembly::Program as AsmProgram;
use crate::tacky::Program as TackyProgram;

/// Translate a TACKY program into assembly. Currently unimplemented in the
/// Rust port.
pub fn generate(_prog: TackyProgram) -> AsmProgram {
    unimplemented!("code generation not yet ported")
}
