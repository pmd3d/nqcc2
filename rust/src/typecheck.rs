use crate::ast::untyped as U;
use crate::ast::typed as T;

/// Placeholder for semantic type checking.
///
/// The original project performs extensive validation and converts the
/// untyped AST into a typed AST.  The Rust port currently only stubs out
/// this functionality.
pub fn typecheck(_p: U::Program) -> T::Program {
    unimplemented!("type checking is not yet implemented");
}
