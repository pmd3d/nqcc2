use crate::assembly::{AsmType, BinaryOperator, Instruction, Operand, Program, Reg as Register, TopLevel};
use crate::assembly_symbols;
use crate::int8::Int8;
use crate::rounding;

const INT32_MAX: i64 = i32::MAX as i64;
const INT32_MIN: i64 = i32::MIN as i64;

fn is_large(imm: i64) -> bool {
    imm > INT32_MAX || imm < INT32_MIN
}

fn is_larger_than_uint(imm: i64) -> bool {
    let max_i: i64 = 4_294_967_295; // 2^32 - 1
    imm > max_i || imm < INT32_MIN
}

fn is_larger_than_byte(imm: i64) -> bool {
    imm >= 256 || imm < -128
}

fn is_constant(op: &Operand) -> bool {
    matches!(op, Operand::Imm(_))
}

fn is_memory(op: &Operand) -> bool {
    matches!(op, Operand::Memory(..) | Operand::Data(..) | Operand::Indexed { .. })
}

fn is_xmm(reg: Register) -> bool {
    matches!(
        reg,
        Register::XMM0
            | Register::XMM1
            | Register::XMM2
            | Register::XMM3
            | Register::XMM4
            | Register::XMM5
            | Register::XMM6
            | Register::XMM7
            | Register::XMM8
            | Register::XMM9
            | Register::XMM10
            | Register::XMM11
            | Register::XMM12
            | Register::XMM13
            | Register::XMM14
            | Register::XMM15
    )
}

fn fixup_instruction(callee_saved_regs: &[Register], instr: Instruction) -> Vec<Instruction> {
    use AsmType::*;
    use BinaryOperator::*;
    use Operand::*;
    match instr {
        Instruction::Mov(t, src, dst)
            if is_memory(&src) && is_memory(&dst) =>
        {
            let scratch = if t == Double {
                Reg(Register::XMM14)
            } else {
                Reg(Register::R10)
            };
            vec![
                Instruction::Mov(t.clone(), src, scratch.clone()),
                Instruction::Mov(t, scratch, dst),
            ]
        }
        Instruction::Mov(Quadword, src @ Imm(i), dst)
            if is_memory(&dst) && is_large(i) =>
        {
            vec![
                Instruction::Mov(Quadword, src, Reg(Register::R10)),
                Instruction::Mov(Quadword, Reg(Register::R10), dst),
            ]
        }
        Instruction::Mov(Longword, Imm(i), dst) if is_larger_than_uint(i) => {
            let bitmask: i64 = 0xFFFF_FFFF;
            let reduced = i & bitmask;
            vec![Instruction::Mov(Longword, Imm(reduced), dst)]
        }
        Instruction::Mov(Byte, Imm(i), dst) if is_larger_than_byte(i) => {
            let reduced = Int8::of_int64(i).to_int64();
            vec![Instruction::Mov(Byte, Imm(reduced), dst)]
        }
        Instruction::Movsx { src_type, dst_type, src, dst }
            if matches!(src, Imm(_)) && is_memory(&dst) =>
        {
            vec![
                Instruction::Mov(src_type.clone(), src, Reg(Register::R10)),
                Instruction::Movsx {
                    src_type,
                    dst_type: dst_type.clone(),
                    src: Reg(Register::R10),
                    dst: Reg(Register::R11),
                },
                Instruction::Mov(dst_type, Reg(Register::R11), dst),
            ]
        }
        Instruction::Movsx { src_type, dst_type, src, dst }
            if matches!(src, Imm(_)) =>
        {
            vec![
                Instruction::Mov(src_type.clone(), src, Reg(Register::R10)),
                Instruction::Movsx { src_type, dst_type, src: Reg(Register::R10), dst },
            ]
        }
        Instruction::Movsx { src_type, dst_type, src, dst }
            if is_memory(&dst) =>
        {
            vec![
                Instruction::Movsx { src_type, dst_type: dst_type.clone(), src, dst: Reg(Register::R11) },
                Instruction::Mov(dst_type, Reg(Register::R11), dst),
            ]
        }
        Instruction::MovZeroExtend { src_type: Byte, src: Imm(i), dst_type, dst } => {
            if is_memory(&dst) {
                vec![
                    Instruction::Mov(Byte, Imm(i), Reg(Register::R10)),
                    Instruction::MovZeroExtend {
                        src_type: Byte,
                        src: Reg(Register::R10),
                        dst_type: dst_type.clone(),
                        dst: Reg(Register::R11),
                    },
                    Instruction::Mov(dst_type, Reg(Register::R11), dst),
                ]
            } else {
                vec![
                    Instruction::Mov(Byte, Imm(i), Reg(Register::R10)),
                    Instruction::MovZeroExtend {
                        src_type: Byte,
                        src: Reg(Register::R10),
                        dst_type,
                        dst,
                    },
                ]
            }
        }
        Instruction::MovZeroExtend { src_type: Byte, dst_type, src, dst }
            if is_memory(&dst) =>
        {
            vec![
                Instruction::MovZeroExtend {
                    src_type: Byte,
                    dst_type: dst_type.clone(),
                    src,
                    dst: Reg(Register::R11),
                },
                Instruction::Mov(dst_type, Reg(Register::R11), dst),
            ]
        }
        Instruction::MovZeroExtend { src_type: Longword, dst_type, src, dst }
            if is_memory(&dst) =>
        {
            vec![
                Instruction::Mov(Longword, src, Reg(Register::R11)),
                Instruction::Mov(dst_type, Reg(Register::R11), dst),
            ]
        }
        Instruction::MovZeroExtend { src_type: Longword, src, dst, .. } => {
            vec![Instruction::Mov(Longword, src, dst)]
        }
        Instruction::Idiv(t, Imm(i)) => {
            vec![
                Instruction::Mov(t.clone(), Imm(i), Reg(Register::R10)),
                Instruction::Idiv(t, Reg(Register::R10)),
            ]
        }
        Instruction::Div(t, Imm(i)) => {
            vec![
                Instruction::Mov(t.clone(), Imm(i), Reg(Register::R10)),
                Instruction::Div(t, Reg(Register::R10)),
            ]
        }
        Instruction::Lea(src, dst) if is_memory(&dst) => {
            vec![
                Instruction::Lea(src, Reg(Register::R11)),
                Instruction::Mov(Quadword, Reg(Register::R11), dst),
            ]
        }
        instr @ Instruction::Binary { t: Double, dst: Reg(_), .. } => vec![instr],
        Instruction::Binary { op, t: Double, src, dst } => {
            let dst_clone = dst.clone();
            vec![
                Instruction::Mov(Double, dst, Reg(Register::XMM15)),
                Instruction::Binary { op, t: Double, src, dst: Reg(Register::XMM15) },
                Instruction::Mov(Double, Reg(Register::XMM15), dst_clone),
            ]
        }
        Instruction::Binary { op, t: Quadword, src: Imm(i), dst }
            if matches!(op, Add | Sub | And | Or) && is_large(i) =>
        {
            vec![
                Instruction::Mov(Quadword, Imm(i), Reg(Register::R10)),
                Instruction::Binary { op, t: Quadword, src: Reg(Register::R10), dst },
            ]
        }
        Instruction::Binary { op, t, src, dst }
            if matches!(op, Add | Sub | And | Or) && is_memory(&src) && is_memory(&dst) =>
        {
            vec![
                Instruction::Mov(t.clone(), src, Reg(Register::R10)),
                Instruction::Binary { op, t, src: Reg(Register::R10), dst },
            ]
        }
        Instruction::Binary { op: Mult, t: Quadword, src: Imm(i), dst }
            if is_large(i) && is_memory(&dst) =>
        {
            let dst_clone = dst.clone();
            vec![
                Instruction::Mov(Quadword, Imm(i), Reg(Register::R10)),
                Instruction::Mov(Quadword, dst, Reg(Register::R11)),
                Instruction::Binary { op: Mult, t: Quadword, src: Reg(Register::R10), dst: Reg(Register::R11) },
                Instruction::Mov(Quadword, Reg(Register::R11), dst_clone),
            ]
        }
        Instruction::Binary { op: Mult, t: Quadword, src: Imm(i), dst }
            if is_large(i) =>
        {
            vec![
                Instruction::Mov(Quadword, Imm(i), Reg(Register::R10)),
                Instruction::Binary { op: Mult, t: Quadword, src: Reg(Register::R10), dst },
            ]
        }
        Instruction::Binary { op: Mult, t, src, dst }
            if is_memory(&dst) =>
        {
            let dst_clone = dst.clone();
            vec![
                Instruction::Mov(t.clone(), dst, Reg(Register::R11)),
                Instruction::Binary { op: Mult, t: t.clone(), src, dst: Reg(Register::R11) },
                Instruction::Mov(t, Reg(Register::R11), dst_clone),
            ]
        }
        instr @ Instruction::Cmp(Double, _, Reg(_)) => vec![instr],
        Instruction::Cmp(Double, src, dst) => {
            vec![
                Instruction::Mov(Double, dst, Reg(Register::XMM15)),
                Instruction::Cmp(Double, src, Reg(Register::XMM15)),
            ]
        }
        Instruction::Cmp(t, src, dst)
            if is_memory(&src) && is_memory(&dst) =>
        {
            vec![
                Instruction::Mov(t.clone(), src, Reg(Register::R10)),
                Instruction::Cmp(t, Reg(Register::R10), dst),
            ]
        }
        Instruction::Cmp(Quadword, src @ Imm(i), dst @ Imm(_)) if is_large(i) => {
            vec![
                Instruction::Mov(Quadword, src, Reg(Register::R10)),
                Instruction::Mov(Quadword, dst, Reg(Register::R11)),
                Instruction::Cmp(Quadword, Reg(Register::R10), Reg(Register::R11)),
            ]
        }
        Instruction::Cmp(Quadword, src @ Imm(i), dst) if is_large(i) => {
            vec![
                Instruction::Mov(Quadword, src, Reg(Register::R10)),
                Instruction::Cmp(Quadword, Reg(Register::R10), dst),
            ]
        }
        Instruction::Cmp(t, src, Imm(i)) => {
            vec![
                Instruction::Mov(t.clone(), Imm(i), Reg(Register::R11)),
                Instruction::Cmp(t, src, Reg(Register::R11)),
            ]
        }
        Instruction::Push(Reg(r)) if is_xmm(r) => {
            vec![
                Instruction::Binary {
                    op: Sub,
                    t: Quadword,
                    src: Imm(8),
                    dst: Reg(Register::SP),
                },
                Instruction::Mov(Double, Reg(r), Memory(Register::SP, 0)),
            ]
        }
        Instruction::Push(src @ Imm(i)) if is_large(i) => {
            vec![
                Instruction::Mov(Quadword, src, Reg(Register::R10)),
                Instruction::Push(Reg(Register::R10)),
            ]
        }
        Instruction::Cvttsd2si(t, src, dst) if is_memory(&dst) => {
            vec![
                Instruction::Cvttsd2si(t.clone(), src, Reg(Register::R11)),
                Instruction::Mov(t, Reg(Register::R11), dst),
            ]
        }
        Instruction::Cvtsi2sd(t, src, dst) => {
            if is_constant(&src) && is_memory(&dst) {
                vec![
                    Instruction::Mov(t.clone(), src, Reg(Register::R10)),
                    Instruction::Cvtsi2sd(t.clone(), Reg(Register::R10), Reg(Register::XMM15)),
                    Instruction::Mov(Double, Reg(Register::XMM15), dst),
                ]
            } else if is_constant(&src) {
                vec![
                    Instruction::Mov(t.clone(), src, Reg(Register::R10)),
                    Instruction::Cvtsi2sd(t, Reg(Register::R10), dst),
                ]
            } else if is_memory(&dst) {
                vec![
                    Instruction::Cvtsi2sd(t.clone(), src, Reg(Register::XMM15)),
                    Instruction::Mov(Double, Reg(Register::XMM15), dst),
                ]
            } else {
                vec![Instruction::Cvtsi2sd(t, src, dst)]
            }
        }
        Instruction::Ret => {
            let mut restore_regs: Vec<Instruction> =
                callee_saved_regs.iter().rev().map(|r| Instruction::Pop(*r)).collect();
            restore_regs.push(Instruction::Ret);
            restore_regs
        }
        other => vec![other],
    }
}

fn emit_stack_adjustment(bytes_for_locals: i32, callee_saved_count: usize) -> Instruction {
    let callee_saved_bytes = 8 * callee_saved_count as i32;
    let total_stack_bytes = callee_saved_bytes + bytes_for_locals;
    let adjusted_stack_bytes = rounding::round_away_from_zero(16, total_stack_bytes);
    let stack_adjustment = (adjusted_stack_bytes - callee_saved_bytes) as i64;
    Instruction::Binary {
        op: BinaryOperator::Sub,
        t: AsmType::Quadword,
        src: Operand::Imm(stack_adjustment),
        dst: Operand::Reg(Register::SP),
    }
}

fn fixup_tl(tl: TopLevel) -> TopLevel {
    match tl {
        TopLevel::Function { name, global, instructions } => {
            let stack_bytes = -assembly_symbols::get_bytes_required(&name);
            let callee_saved_regs: Vec<Register> =
                assembly_symbols::get_callee_saved_regs_used(&name).iter().cloned().collect();
            let adjust_rsp = emit_stack_adjustment(stack_bytes, callee_saved_regs.len());
            let mut setup_instructions = Vec::new();
            setup_instructions.push(adjust_rsp);
            for r in &callee_saved_regs {
                setup_instructions.push(Instruction::Push(Operand::Reg(*r)));
            }
            let mut fixed_body = Vec::new();
            for instr in instructions {
                fixed_body.extend(fixup_instruction(&callee_saved_regs, instr));
            }
            let mut all_instructions = setup_instructions;
            all_instructions.extend(fixed_body);
            TopLevel::Function { name, global, instructions: all_instructions }
        }
        other => other,
    }
}

/// Fix up invalid or unsupported assembly instructions.
pub fn fixup_program(Program(tls): Program) -> Program {
    let fixed_functions = tls.into_iter().map(fixup_tl).collect();
    Program(fixed_functions)
}

