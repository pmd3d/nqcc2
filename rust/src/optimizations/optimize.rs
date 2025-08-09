use crate::{settings::Optimizations, tacky::Program};

pub fn optimize(_opts: Optimizations, _src_file: &str, prog: Program) -> Program {
    prog
}
