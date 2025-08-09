use crate::assembly::{
    AsmType, BinaryOperator, CondCode, Instruction, Operand, Program, Reg, TopLevel, UnaryOperator,
};
use crate::assembly_symbols;
use crate::initializers::{self, StaticInit};
use crate::settings::{PLATFORM, Platform};
use crate::utils::string_util;
use std::fs::File;
use std::io::{BufWriter, Write};

fn suffix(t: &AsmType) -> &'static str {
    match t {
        AsmType::Byte => "b",
        AsmType::Longword => "l",
        AsmType::Quadword => "q",
        AsmType::Double => "sd",
        AsmType::ByteArray { .. } => {
            panic!("Internal error: found instruction w/ non-scalar operand type")
        }
    }
}

fn align_directive() -> &'static str {
    match *PLATFORM.lock().unwrap() {
        Platform::OSX => ".balign",
        Platform::Linux => ".align",
    }
}

fn show_label(name: &str) -> String {
    match *PLATFORM.lock().unwrap() {
        Platform::OSX => format!("_{}", name),
        Platform::Linux => name.to_string(),
    }
}

fn show_local_label(label: &str) -> String {
    match *PLATFORM.lock().unwrap() {
        Platform::OSX => format!("L{}", label),
        Platform::Linux => format!(".L{}", label),
    }
}

fn show_fun_name(name: &str) -> String {
    match *PLATFORM.lock().unwrap() {
        Platform::OSX => format!("_{}", name),
        Platform::Linux => {
            if assembly_symbols::is_defined(name) {
                name.to_string()
            } else {
                format!("{}@PLT", name)
            }
        }
    }
}

fn show_long_reg(reg: Reg) -> &'static str {
    match reg {
        Reg::AX => "%eax",
        Reg::BX => "%ebx",
        Reg::CX => "%ecx",
        Reg::DX => "%edx",
        Reg::DI => "%edi",
        Reg::SI => "%esi",
        Reg::R8 => "%r8d",
        Reg::R9 => "%r9d",
        Reg::R10 => "%r10d",
        Reg::R11 => "%r11d",
        Reg::R12 => "%r12d",
        Reg::R13 => "%r13d",
        Reg::R14 => "%r14d",
        Reg::R15 => "%r15d",
        Reg::SP => panic!("Internal error: no 32-bit RSP"),
        Reg::BP => panic!("Internal error: no 32-bit RBP"),
        _ => panic!("Internal error: can't store longword type in XMM register"),
    }
}

fn show_quadword_reg(reg: Reg) -> &'static str {
    match reg {
        Reg::AX => "%rax",
        Reg::BX => "%rbx",
        Reg::CX => "%rcx",
        Reg::DX => "%rdx",
        Reg::DI => "%rdi",
        Reg::SI => "%rsi",
        Reg::R8 => "%r8",
        Reg::R9 => "%r9",
        Reg::R10 => "%r10",
        Reg::R11 => "%r11",
        Reg::R12 => "%r12",
        Reg::R13 => "%r13",
        Reg::R14 => "%r14",
        Reg::R15 => "%r15",
        Reg::SP => "%rsp",
        Reg::BP => "%rbp",
        _ => panic!("Internal error: can't store quadword type in XMM register"),
    }
}

fn show_double_reg(reg: Reg) -> &'static str {
    match reg {
        Reg::XMM0 => "%xmm0",
        Reg::XMM1 => "%xmm1",
        Reg::XMM2 => "%xmm2",
        Reg::XMM3 => "%xmm3",
        Reg::XMM4 => "%xmm4",
        Reg::XMM5 => "%xmm5",
        Reg::XMM6 => "%xmm6",
        Reg::XMM7 => "%xmm7",
        Reg::XMM8 => "%xmm8",
        Reg::XMM9 => "%xmm9",
        Reg::XMM10 => "%xmm10",
        Reg::XMM11 => "%xmm11",
        Reg::XMM12 => "%xmm12",
        Reg::XMM13 => "%xmm13",
        Reg::XMM14 => "%xmm14",
        Reg::XMM15 => "%xmm15",
        _ => panic!("Internal error: can't store double type in general-purpose register",),
    }
}

fn show_byte_reg(reg: Reg) -> &'static str {
    match reg {
        Reg::AX => "%al",
        Reg::BX => "%bl",
        Reg::CX => "%cl",
        Reg::DX => "%dl",
        Reg::DI => "%dil",
        Reg::SI => "%sil",
        Reg::R8 => "%r8b",
        Reg::R9 => "%r9b",
        Reg::R10 => "%r10b",
        Reg::R11 => "%r11b",
        Reg::R12 => "%r12b",
        Reg::R13 => "%r13b",
        Reg::R14 => "%r14b",
        Reg::R15 => "%r15b",
        Reg::SP => panic!("Internal error: no one-byte RSP"),
        Reg::BP => panic!("Internal error: no one-byte RBP"),
        _ => panic!("Internal error: can't store byte type in XMM register"),
    }
}

fn show_operand(t: &AsmType, operand: &Operand) -> String {
    match operand {
        Operand::Reg(r) => match t {
            AsmType::Byte => show_byte_reg(*r).to_string(),
            AsmType::Longword => show_long_reg(*r).to_string(),
            AsmType::Quadword => show_quadword_reg(*r).to_string(),
            AsmType::Double => show_double_reg(*r).to_string(),
            AsmType::ByteArray { .. } => {
                panic!("Internal error: can't store non-scalar operand in register")
            }
        },
        Operand::Imm(i) => format!("${}", i),
        Operand::Memory(r, 0) => format!("({})", show_quadword_reg(*r)),
        Operand::Memory(r, i) => format!("{}({})", i, show_quadword_reg(*r)),
        Operand::Data(name, offset) => {
            let lbl = if assembly_symbols::is_constant(name) {
                show_local_label(name)
            } else {
                show_label(name)
            };
            if *offset == 0 {
                format!("{}(%rip)", lbl)
            } else {
                format!("{}+{}(%rip)", lbl, offset)
            }
        }
        Operand::Indexed { base, index, scale } => format!(
            "({}, {}, {})",
            show_quadword_reg(*base),
            show_quadword_reg(*index),
            scale
        ),
        Operand::Pseudo(name) => format!("%{}", name),
        Operand::PseudoMem(name, offset) => format!("{}(%{})", offset, name),
    }
}

fn show_byte_operand(op: &Operand) -> String {
    match op {
        Operand::Reg(r) => show_byte_reg(*r).to_string(),
        other => show_operand(&AsmType::Longword, other),
    }
}

fn show_unary_instruction(op: UnaryOperator) -> &'static str {
    match op {
        UnaryOperator::Neg => "neg",
        UnaryOperator::Not => "not",
        UnaryOperator::Shr => "shr",
    }
}

fn show_binary_instruction(op: BinaryOperator) -> &'static str {
    match op {
        BinaryOperator::Add => "add",
        BinaryOperator::Sub => "sub",
        BinaryOperator::Mult => "imul",
        BinaryOperator::DivDouble => "div",
        BinaryOperator::And => "and",
        BinaryOperator::Or => "or",
        BinaryOperator::Shl => "shl",
        BinaryOperator::ShrBinop => "shr",
        BinaryOperator::Xor => {
            panic!("Internal error, should handle xor as special case")
        }
    }
}

fn show_cond_code(code: CondCode) -> &'static str {
    match code {
        CondCode::E => "e",
        CondCode::NE => "ne",
        CondCode::G => "g",
        CondCode::GE => "ge",
        CondCode::L => "l",
        CondCode::LE => "le",
        CondCode::A => "a",
        CondCode::AE => "ae",
        CondCode::B => "b",
        CondCode::BE => "be",
    }
}

fn emit_instruction<W: Write>(w: &mut W, instr: &Instruction) {
    match instr {
        Instruction::Mov(t, src, dst) => {
            writeln!(
                w,
                "\tmov{} {}, {}",
                suffix(t),
                show_operand(t, src),
                show_operand(t, dst)
            )
            .unwrap();
        }
        Instruction::Unary(op, t, dst) => {
            writeln!(
                w,
                "\t{}{} {}",
                show_unary_instruction(*op),
                suffix(t),
                show_operand(t, dst)
            )
            .unwrap();
        }
        Instruction::Binary {
            op: BinaryOperator::Xor,
            t: AsmType::Double,
            src,
            dst,
        } => {
            writeln!(
                w,
                "\txorpd {}, {}",
                show_operand(&AsmType::Double, src),
                show_operand(&AsmType::Double, dst)
            )
            .unwrap();
        }
        Instruction::Binary {
            op: BinaryOperator::Mult,
            t: AsmType::Double,
            src,
            dst,
        } => {
            writeln!(
                w,
                "\tmulsd {}, {}",
                show_operand(&AsmType::Double, src),
                show_operand(&AsmType::Double, dst)
            )
            .unwrap();
        }
        Instruction::Binary { op, t, src, dst } => {
            writeln!(
                w,
                "\t{}{} {}, {}",
                show_binary_instruction(*op),
                suffix(t),
                show_operand(t, src),
                show_operand(t, dst)
            )
            .unwrap();
        }
        Instruction::Cmp(AsmType::Double, src, dst) => {
            writeln!(
                w,
                "\tcomisd {}, {}",
                show_operand(&AsmType::Double, src),
                show_operand(&AsmType::Double, dst)
            )
            .unwrap();
        }
        Instruction::Cmp(t, src, dst) => {
            writeln!(
                w,
                "\tcmp{} {}, {}",
                suffix(t),
                show_operand(t, src),
                show_operand(t, dst)
            )
            .unwrap();
        }
        Instruction::Idiv(t, operand) => {
            writeln!(w, "\tidiv{} {}", suffix(t), show_operand(t, operand)).unwrap();
        }
        Instruction::Div(t, operand) => {
            writeln!(w, "\tdiv{} {}", suffix(t), show_operand(t, operand)).unwrap();
        }
        Instruction::Lea(src, dst) => {
            writeln!(
                w,
                "\tleaq {}, {}",
                show_operand(&AsmType::Quadword, src),
                show_operand(&AsmType::Quadword, dst)
            )
            .unwrap();
        }
        Instruction::Cdq(AsmType::Longword) => {
            writeln!(w, "\tcdq").unwrap();
        }
        Instruction::Cdq(AsmType::Quadword) => {
            writeln!(w, "\tcqo").unwrap();
        }
        Instruction::Cdq(_) => {
            panic!("Internal error: can't apply cdq to a byte or non-integer type");
        }
        Instruction::Jmp(lbl) => {
            writeln!(w, "\tjmp {}", show_local_label(lbl)).unwrap();
        }
        Instruction::JmpCC(code, lbl) => {
            writeln!(w, "\tj{} {}", show_cond_code(*code), show_local_label(lbl)).unwrap();
        }
        Instruction::SetCC(code, operand) => {
            writeln!(
                w,
                "\tset{} {}",
                show_cond_code(*code),
                show_byte_operand(operand)
            )
            .unwrap();
        }
        Instruction::Label(lbl) => {
            writeln!(w, "{}:", show_local_label(lbl)).unwrap();
        }
        Instruction::Push(op) => {
            writeln!(w, "\tpushq {}", show_operand(&AsmType::Quadword, op)).unwrap();
        }
        Instruction::Pop(r) => {
            writeln!(w, "\tpopq {}", show_quadword_reg(*r)).unwrap();
        }
        Instruction::Call(f) => {
            writeln!(w, "\tcall {}", show_fun_name(f)).unwrap();
        }
        Instruction::Movsx {
            src_type,
            dst_type,
            src,
            dst,
        } => {
            writeln!(
                w,
                "\tmovs{}{} {}, {}",
                suffix(src_type),
                suffix(dst_type),
                show_operand(src_type, src),
                show_operand(dst_type, dst)
            )
            .unwrap();
        }
        Instruction::MovZeroExtend {
            src_type,
            dst_type,
            src,
            dst,
        } => {
            writeln!(
                w,
                "\tmovz{}{} {}, {}",
                suffix(src_type),
                suffix(dst_type),
                show_operand(src_type, src),
                show_operand(dst_type, dst)
            )
            .unwrap();
        }
        Instruction::Cvtsi2sd(t, src, dst) => {
            writeln!(
                w,
                "\tcvtsi2sd{} {}, {}",
                suffix(t),
                show_operand(t, src),
                show_operand(&AsmType::Double, dst)
            )
            .unwrap();
        }
        Instruction::Cvttsd2si(t, src, dst) => {
            writeln!(
                w,
                "\tcvttsd2si{} {}, {}",
                suffix(t),
                show_operand(&AsmType::Double, src),
                show_operand(t, dst)
            )
            .unwrap();
        }
        Instruction::Ret => {
            writeln!(w, "\tmovq %rbp, %rsp").unwrap();
            writeln!(w, "\tpopq %rbp").unwrap();
            writeln!(w, "\tret").unwrap();
        }
    }
}

fn emit_global_directive<W: Write>(w: &mut W, global: bool, label: &str) {
    if global {
        writeln!(w, "\t.globl {}", label).unwrap();
    }
}

fn escape(s: &str) -> String {
    s.chars()
        .map(|c| {
            if string_util::is_alnum(c) {
                c.to_string()
            } else {
                format!("\\{:03o}", c as u32)
            }
        })
        .collect()
}

fn emit_init<W: Write>(w: &mut W, init: &StaticInit) {
    match init {
        StaticInit::IntInit(i) => {
            writeln!(w, "\t.long {}", i).unwrap();
        }
        StaticInit::LongInit(l) => {
            writeln!(w, "\t.quad {}", l).unwrap();
        }
        StaticInit::UIntInit(u) => {
            writeln!(w, "\t.long {}", u).unwrap();
        }
        StaticInit::ULongInit(u) => {
            writeln!(w, "\t.quad {}", u).unwrap();
        }
        StaticInit::CharInit(c) => {
            writeln!(w, "\t.byte {}", c).unwrap();
        }
        StaticInit::UCharInit(c) => {
            writeln!(w, "\t.byte {}", c).unwrap();
        }
        StaticInit::DoubleInit(d) => {
            let bits = i64::from_ne_bytes(d.to_bits().to_ne_bytes());
            writeln!(w, "\t.quad {}", bits).unwrap();
        }
        StaticInit::ZeroInit(bytes) => {
            writeln!(w, "\t.zero {}", bytes).unwrap();
        }
        StaticInit::StringInit(s, true) => {
            writeln!(w, "\t.asciz \"{}\"", escape(s)).unwrap();
        }
        StaticInit::StringInit(s, false) => {
            writeln!(w, "\t.ascii \"{}\"", escape(s)).unwrap();
        }
        StaticInit::PointerInit(lbl) => {
            writeln!(w, "\t.quad {}", show_local_label(lbl)).unwrap();
        }
    }
}

fn emit_constant<W: Write>(w: &mut W, name: &str, alignment: i32, init: &StaticInit) {
    let constant_section_name = match (*PLATFORM.lock().unwrap(), init) {
        (Platform::Linux, _) => ".section .rodata",
        (Platform::OSX, StaticInit::StringInit(_, _)) => ".cstring",
        (Platform::OSX, _) => {
            if alignment == 8 {
                ".literal8"
            } else if alignment == 16 {
                ".literal16"
            } else {
                panic!("Internal error: found constant with bad alignment");
            }
        }
    };

    writeln!(w, "{}", constant_section_name).unwrap();
    writeln!(w, "\t{} {}", align_directive(), alignment).unwrap();
    writeln!(w, "{}:", show_local_label(name)).unwrap();
    emit_init(w, init);
    if constant_section_name == ".literal16" {
        emit_init(w, &StaticInit::LongInit(0));
    }
}

fn emit_tl<W: Write>(w: &mut W, tl: &TopLevel) {
    match tl {
        TopLevel::Function {
            name,
            global,
            instructions,
        } => {
            let label = show_label(name);
            emit_global_directive(w, *global, &label);
            writeln!(w, "\t.text").unwrap();
            writeln!(w, "{}:", label).unwrap();
            writeln!(w, "\tpushq %rbp").unwrap();
            writeln!(w, "\tmovq %rsp, %rbp").unwrap();
            for instr in instructions {
                emit_instruction(w, instr);
            }
        }
        TopLevel::StaticVariable {
            name,
            alignment,
            global,
            init,
        } if init.iter().all(|i| initializers::is_zero(i)) => {
            let label = show_label(name);
            emit_global_directive(w, *global, &label);
            writeln!(w, "\t.bss").unwrap();
            writeln!(w, "\t{} {}", align_directive(), alignment).unwrap();
            writeln!(w, "{}:", label).unwrap();
            for i in init {
                emit_init(w, i);
            }
        }
        TopLevel::StaticVariable {
            name,
            alignment,
            global,
            init,
        } => {
            let label = show_label(name);
            emit_global_directive(w, *global, &label);
            writeln!(w, "\t.data").unwrap();
            writeln!(w, "\t{} {}", align_directive(), alignment).unwrap();
            writeln!(w, "{}:", label).unwrap();
            for i in init {
                emit_init(w, i);
            }
        }
        TopLevel::StaticConstant {
            name,
            alignment,
            init,
        } => {
            emit_constant(w, name, *alignment, init);
        }
    }
}

fn emit_stack_note<W: Write>(w: &mut W) {
    match *PLATFORM.lock().unwrap() {
        Platform::OSX => {}
        Platform::Linux => {
            writeln!(w, "\t.section .note.GNU-stack,\"\",@progbits").unwrap();
        }
    }
}

pub fn emit(filename: &str, Program(tls): Program) {
    let file = File::create(filename).expect("unable to create file");
    let mut writer = BufWriter::new(file);
    for tl in tls {
        emit_tl(&mut writer, &tl);
    }
    emit_stack_note(&mut writer);
}
