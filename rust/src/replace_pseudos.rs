use crate::assembly::{self, AsmType, Instruction, Operand, Reg, TopLevel, Program};
use crate::assembly_symbols;
use crate::rounding;
use std::collections::BTreeMap;

struct ReplacementState {
    current_offset: i32,
    offset_map: BTreeMap<String, i32>,
}

fn calculate_offset(state: &mut ReplacementState, name: &str) -> i32 {
    let size = assembly_symbols::get_size(name);
    let alignment = assembly_symbols::get_alignment(name);
    state.current_offset = rounding::round_away_from_zero(alignment, state.current_offset - size);
    state.offset_map.insert(name.into(), state.current_offset);
    state.current_offset
}

fn replace_operand(state: &mut ReplacementState, op: Operand) -> Operand {
    match op {
        Operand::Pseudo(s) => {
            if assembly_symbols::is_static(&s) {
                Operand::Data(s, 0)
            } else {
                let offset = match state.offset_map.get(&s) {
                    Some(off) => *off,
                    None => calculate_offset(state, &s),
                };
                Operand::Memory(Reg::BP, offset)
            }
        }
        Operand::PseudoMem(s, offset) => {
            if assembly_symbols::is_static(&s) {
                Operand::Data(s, offset)
            } else {
                let var_offset = match state.offset_map.get(&s) {
                    Some(off) => *off,
                    None => calculate_offset(state, &s),
                };
                Operand::Memory(Reg::BP, offset + var_offset)
            }
        }
        other => other,
    }
}

fn replace_pseudos_in_instruction(state: &mut ReplacementState, instr: Instruction) -> Instruction {
    use Instruction::*;
    match instr {
        Mov(t, src, dst) => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Mov(t, src1, dst1)
        }
        Movsx { src_type, dst_type, src, dst } => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Movsx { src_type, dst_type, src: src1, dst: dst1 }
        }
        MovZeroExtend { src_type, dst_type, src, dst } => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            MovZeroExtend { src_type, dst_type, src: src1, dst: dst1 }
        }
        Lea(src, dst) => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Lea(src1, dst1)
        }
        Unary(op, t, dst) => {
            let dst1 = replace_operand(state, dst);
            Unary(op, t, dst1)
        }
        Binary { op, t, src, dst } => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Binary { op, t, src: src1, dst: dst1 }
        }
        Cmp(t, op1, op2) => {
            let op1p = replace_operand(state, op1);
            let op2p = replace_operand(state, op2);
            Cmp(t, op1p, op2p)
        }
        Idiv(t, op) => Idiv(t, replace_operand(state, op)),
        Div(t, op) => Div(t, replace_operand(state, op)),
        SetCC(code, op) => SetCC(code, replace_operand(state, op)),
        Push(op) => Push(replace_operand(state, op)),
        Cvttsd2si(t, src, dst) => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Cvttsd2si(t, src1, dst1)
        }
        Cvtsi2sd(t, src, dst) => {
            let src1 = replace_operand(state, src);
            let dst1 = replace_operand(state, dst);
            Cvtsi2sd(t, src1, dst1)
        }
        other @ (Ret | Cdq(_) | Label(_) | JmpCC(_, _) | Jmp(_) | Call(_)) => other,
        Pop(_) => panic!("Internal error"),
    }
}

fn replace_pseudos_in_tl(tl: TopLevel) -> TopLevel {
    match tl {
        TopLevel::Function { name, global, instructions } => {
            let starting_offset = if assembly_symbols::returns_on_stack(&name) { -8 } else { 0 };
            let mut state = ReplacementState { current_offset: starting_offset, offset_map: BTreeMap::new() };
            let fixed_instructions: Vec<Instruction> =
                instructions.into_iter().map(|i| replace_pseudos_in_instruction(&mut state, i)).collect();
            assembly_symbols::set_bytes_required(&name, state.current_offset);
            TopLevel::Function { name, global, instructions: fixed_instructions }
        }
        other => other,
    }
}

pub fn replace_pseudos(Program(tls): Program) -> Program {
    Program(tls.into_iter().map(replace_pseudos_in_tl).collect())
}
