use std::collections::HashSet;
use crate::{cfg::Cfg, tacky::Instruction};

pub fn optimize(_aliased_vars: &HashSet<String>, cfg: Cfg<(), Instruction>) -> Cfg<(), Instruction> {
    cfg
}
