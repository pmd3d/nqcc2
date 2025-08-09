use crate::tacky::{Instruction, TackyVal};

pub fn get_dst(_instr: &Instruction) -> Option<TackyVal> {
    None
}

pub fn is_static(_name: &str) -> bool {
    false
}
