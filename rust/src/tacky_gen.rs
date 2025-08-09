use crate::ast::{self, Exp, Statement, typed};
use crate::consts::Const;
use crate::tacky::{self, Instruction, Program as TackyProgram, TackyVal, TopLevel};

/// Result of translating an expression: list of instructions and resulting value.
fn emit_tacky_for_exp(e: &Exp) -> (Vec<Instruction>, TackyVal) {
    match e {
        Exp::Constant(c) => (vec![], TackyVal::Constant(c.clone())),
        _ => unimplemented!("expression translation not yet implemented"),
    }
}

/// Translate a statement to a list of TACKY instructions.
fn emit_tacky_for_statement(stmt: &Statement) -> Vec<Instruction> {
    match stmt {
        Statement::Return(None) => vec![Instruction::Return(None)],
        Statement::Return(Some(exp)) => {
            let (mut instrs, val) = emit_tacky_for_exp(exp);
            instrs.push(Instruction::Return(Some(val)));
            instrs
        }
        _ => unimplemented!("statement translation not yet implemented"),
    }
}

/// Generate a TACKY program from a typed AST program.
///
/// The current Rust port only supports a very small subset of the
/// original compiler.  As a result the generated program is typically
/// empty, but this function exists so that subsequent ports can hook
/// into it.
pub fn r#gen(_prog: typed::Program) -> tacky::Program {
    // The OCaml version converts an entire AST into a list of top level
    // TACKY definitions.  The Rust translation does not yet have a
    // complete AST so for now we simply return an empty program.
    TackyProgram(Vec::<TopLevel>::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_constant() {
        let stmt = Statement::Return(Some(Exp::Constant(Const::Int(5))));
        let instrs = emit_tacky_for_statement(&stmt);
        assert_eq!(instrs.len(), 1);
        match &instrs[0] {
            Instruction::Return(Some(TackyVal::Constant(Const::Int(i)))) => assert_eq!(*i, 5),
            other => panic!("unexpected instruction: {:?}", other),
        }
    }
}
