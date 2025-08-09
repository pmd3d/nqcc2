use std::collections::BTreeSet;
use crate::assembly::Program;

/// Allocate registers for the given assembly program. Stub implementation.
pub fn allocate_registers(_aliased_vars: &BTreeSet<String>, _prog: Program) -> Program {
    unimplemented!("register allocation not yet ported")
}
