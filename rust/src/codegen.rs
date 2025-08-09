use crate::assembly::{
    self, AsmType, BinaryOperator as BinOp, CondCode, Instruction, Operand, Program as AsmProgram,
    Reg, TopLevel as AsmTopLevel, UnaryOperator as UnOp,
};
use crate::assembly_symbols;
use crate::consts::Const;
use crate::initializers::StaticInit;
use crate::symbols;
use crate::tacky::{
    self, BinaryOperator as TBinOp, Instruction as TackyInstruction, Program as TackyProgram,
    TackyVal, TopLevel as TackyTopLevel, UnaryOperator as TUnOp,
};
use crate::type_utils;
use crate::types::Type;
use crate::unique_ids;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Mutex;

const INT_PARAM_REGS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];
const DBL_PARAM_REGS: [Reg; 8] = [
    Reg::XMM0, Reg::XMM1, Reg::XMM2, Reg::XMM3, Reg::XMM4, Reg::XMM5, Reg::XMM6, Reg::XMM7,
];

static CONSTANTS: Lazy<Mutex<HashMap<u64, (String, i32)>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

fn add_constant_with_alignment(d: f64, alignment: i32) -> String {
    let key = d.to_bits();
    let mut tbl = CONSTANTS.lock().unwrap();
    if let Some((name, old)) = tbl.get_mut(&key) {
        *old = (*old).max(alignment);
        name.clone()
    } else {
        let name = unique_ids::make_label("dbl");
        tbl.insert(key, (name.clone(), alignment));
        name
    }
}

fn add_constant(d: f64) -> String {
    add_constant_with_alignment(d, 8)
}

fn convert_val(v: &TackyVal) -> Operand {
    match v {
        TackyVal::Constant(Const::Char(c)) => Operand::Imm(*c as i64),
        TackyVal::Constant(Const::UChar(c)) => Operand::Imm(*c as i64),
        TackyVal::Constant(Const::Int(i)) => Operand::Imm(*i as i64),
        TackyVal::Constant(Const::Long(l)) => Operand::Imm(*l),
        TackyVal::Constant(Const::UInt(u)) => Operand::Imm(*u as i64),
        TackyVal::Constant(Const::ULong(u)) => Operand::Imm(*u as i64),
        TackyVal::Constant(Const::Double(d)) => Operand::Data(add_constant(*d), 0),
        TackyVal::Var(name) => {
            if type_utils::is_scalar(&symbols::get(name).t) {
                Operand::Pseudo(name.clone())
            } else {
                Operand::PseudoMem(name.clone(), 0)
            }
        }
    }
}

fn convert_type(t: &Type) -> AsmType {
    match t {
        Type::Int | Type::UInt => AsmType::Longword,
        Type::Long | Type::ULong | Type::Pointer(_) => AsmType::Quadword,
        Type::Char | Type::SChar | Type::UChar => AsmType::Byte,
        Type::Double => AsmType::Double,
        Type::Array { .. } | Type::Structure(_) => AsmType::ByteArray {
            size: type_utils::get_size(t) as i32,
            alignment: type_utils::get_alignment(t) as i32,
        },
        Type::FunType { .. } | Type::Void => panic!("unsupported type in codegen"),
    }
}

fn asm_type(v: &TackyVal) -> AsmType {
    convert_type(&tacky::type_of_val(v))
}

fn convert_unop(op: TUnOp) -> UnOp {
    match op {
        TUnOp::Complement => UnOp::Not,
        TUnOp::Negate => UnOp::Neg,
        TUnOp::Not => panic!("cannot convert TACKY not directly"),
    }
}

fn convert_binop(op: TBinOp) -> BinOp {
    match op {
        TBinOp::Add => BinOp::Add,
        TBinOp::Subtract => BinOp::Sub,
        TBinOp::Multiply => BinOp::Mult,
        TBinOp::Divide => BinOp::DivDouble,
        _ => panic!("unsupported binop"),
    }
}

fn convert_cond_code(signed: bool, op: TBinOp) -> CondCode {
    match op {
        TBinOp::Equal => CondCode::E,
        TBinOp::NotEqual => CondCode::NE,
        TBinOp::GreaterThan => if signed { CondCode::G } else { CondCode::A },
        TBinOp::GreaterOrEqual => if signed { CondCode::GE } else { CondCode::AE },
        TBinOp::LessThan => if signed { CondCode::L } else { CondCode::B },
        TBinOp::LessOrEqual => if signed { CondCode::LE } else { CondCode::BE },
        _ => panic!("not a condition code"),
    }
}

fn classify_params(params: &[TackyVal]) -> (Vec<(AsmType, Operand)>, Vec<Operand>, Vec<(AsmType, Operand)>) {
    let mut int_regs = Vec::new();
    let mut dbl_regs = Vec::new();
    let mut stack = Vec::new();
    for v in params {
        let t = asm_type(v);
        let op = convert_val(v);
        match tacky::type_of_val(v) {
            Type::Double => {
                if dbl_regs.len() < DBL_PARAM_REGS.len() {
                    dbl_regs.push(op);
                } else {
                    stack.push((AsmType::Double, op));
                }
            }
            _ => {
                if int_regs.len() < INT_PARAM_REGS.len() {
                    int_regs.push((t, op));
                } else {
                    stack.push((t, op));
                }
            }
        }
    }
    (int_regs, dbl_regs, stack)
}

fn classify_return_type(t: &Type) -> (Vec<Reg>, bool) {
    match t {
        Type::Void => (Vec::new(), false),
        Type::Double => (vec![Reg::XMM0], false),
        _ => (vec![Reg::AX], false),
    }
}

fn convert_function_call(name: &str, args: &[TackyVal], dst: Option<&TackyVal>) -> Vec<Instruction> {
    let mut insts = Vec::new();
    let (int_args, dbl_args, stack_args) = classify_params(args);

    let stack_padding: i64 = if stack_args.len() % 2 == 0 { 0 } else { 8 };
    if stack_padding != 0 {
        insts.push(Instruction::Binary {
            op: BinOp::Sub,
            t: AsmType::Quadword,
            src: Operand::Imm(stack_padding),
            dst: Operand::Reg(Reg::SP),
        });
    }

    for (i, (t, op)) in int_args.iter().enumerate() {
        insts.push(Instruction::Mov(t.clone(), op.clone(), Operand::Reg(INT_PARAM_REGS[i])));
    }
    for (i, op) in dbl_args.iter().enumerate() {
        insts.push(Instruction::Mov(AsmType::Double, op.clone(), Operand::Reg(DBL_PARAM_REGS[i])));
    }

    for (t, op) in stack_args.iter().rev() {
        match t {
            AsmType::Quadword | AsmType::Double | AsmType::Longword | AsmType::Byte => {
                insts.push(Instruction::Push(op.clone()));
            }
            AsmType::ByteArray { size, .. } => {
                insts.push(Instruction::Binary {
                    op: BinOp::Sub,
                    t: AsmType::Quadword,
                    src: Operand::Imm(8),
                    dst: Operand::Reg(Reg::SP),
                });
                insts.push(Instruction::Mov(t.clone(), op.clone(), Operand::Memory(Reg::SP, 0)));
                if *size > 8 {
                    // naive copy
                }
            }
        }
    }

    insts.push(Instruction::Call(name.to_string()));

    let bytes_to_remove = (stack_args.len() as i64) * 8 + stack_padding;
    if bytes_to_remove > 0 {
        insts.push(Instruction::Binary {
            op: BinOp::Add,
            t: AsmType::Quadword,
            src: Operand::Imm(bytes_to_remove),
            dst: Operand::Reg(Reg::SP),
        });
    }

    if let Some(d) = dst {
        match tacky::type_of_val(d) {
            Type::Double => insts.push(Instruction::Mov(AsmType::Double, Operand::Reg(Reg::XMM0), convert_val(d))),
            Type::Void => {}
            _ => insts.push(Instruction::Mov(asm_type(d), Operand::Reg(Reg::AX), convert_val(d))),
        }
    }

    insts
}

fn convert_return_instruction(v: Option<TackyVal>) -> Vec<Instruction> {
    match v {
        None => vec![Instruction::Ret],
        Some(val) => {
            let mut insts = Vec::new();
            match tacky::type_of_val(&val) {
                Type::Double => insts.push(Instruction::Mov(AsmType::Double, convert_val(&val), Operand::Reg(Reg::XMM0))),
                _ => insts.push(Instruction::Mov(asm_type(&val), convert_val(&val), Operand::Reg(Reg::AX))),
            }
            insts.push(Instruction::Ret);
            insts
        }
    }
}

fn convert_instruction(instr: TackyInstruction) -> Vec<Instruction> {
    use TackyInstruction::*;
    match instr {
        Copy { src, dst } if type_utils::is_scalar(&tacky::type_of_val(&src)) => {
            vec![Instruction::Mov(asm_type(&src), convert_val(&src), convert_val(&dst))]
        }
        Copy { src, dst } => {
            let size = type_utils::get_size(&tacky::type_of_val(&src)) as i32;
            let mut insts = Vec::new();
            for i in (0..size).step_by(8) {
                let off = i;
                let src_op = Operand::PseudoMem(match &src { TackyVal::Var(s) => s.clone(), _ => panic!() }, off);
                let dst_op = Operand::PseudoMem(match &dst { TackyVal::Var(s) => s.clone(), _ => panic!() }, off);
                insts.push(Instruction::Mov(AsmType::Quadword, src_op, dst_op));
            }
            insts
        }
        Return(v) => convert_return_instruction(v),
        Unary { op: TUnOp::Not, src, dst } => {
            vec![
                Instruction::Cmp(asm_type(&src), Operand::Imm(0), convert_val(&src)),
                Instruction::Mov(asm_type(&dst), Operand::Imm(0), convert_val(&dst)),
                Instruction::SetCC(CondCode::E, convert_val(&dst)),
            ]
        }
        Unary { op, src, dst } => {
            vec![
                Instruction::Mov(asm_type(&src), convert_val(&src), convert_val(&dst)),
                Instruction::Unary(convert_unop(op), asm_type(&src), convert_val(&dst)),
            ]
        }
        Binary { op, src1, src2, dst } => {
            let src_t = asm_type(&src1);
            let asm_src1 = convert_val(&src1);
            let asm_src2 = convert_val(&src2);
            let asm_dst = convert_val(&dst);
            match op {
                TBinOp::Equal
                | TBinOp::NotEqual
                | TBinOp::GreaterThan
                | TBinOp::GreaterOrEqual
                | TBinOp::LessThan
                | TBinOp::LessOrEqual => {
                    let signed = type_utils::is_signed(&tacky::type_of_val(&src1));
                    vec![
                        Instruction::Cmp(src_t, asm_src2, asm_src1),
                        Instruction::Mov(asm_type(&dst), Operand::Imm(0), asm_dst.clone()),
                        Instruction::SetCC(convert_cond_code(signed, op), asm_dst),
                    ]
                }
                _ => vec![
                    Instruction::Mov(src_t.clone(), asm_src1.clone(), asm_dst.clone()),
                    Instruction::Binary { op: convert_binop(op), t: src_t, src: asm_src2, dst: asm_dst },
                ],
            }
        }
        Load { src_ptr, dst } => vec![
            Instruction::Mov(AsmType::Quadword, convert_val(&src_ptr), Operand::Reg(Reg::R9)),
            Instruction::Mov(asm_type(&dst), Operand::Memory(Reg::R9, 0), convert_val(&dst)),
        ],
        Store { src, dst_ptr } => vec![
            Instruction::Mov(AsmType::Quadword, convert_val(&dst_ptr), Operand::Reg(Reg::R9)),
            Instruction::Mov(asm_type(&src), convert_val(&src), Operand::Memory(Reg::R9, 0)),
        ],
        GetAddress { src, dst } => vec![Instruction::Lea(convert_val(&src), convert_val(&dst))],
        Jump(l) => vec![Instruction::Jmp(l)],
        JumpIfZero(v, l) => vec![
            Instruction::Cmp(asm_type(&v), Operand::Imm(0), convert_val(&v)),
            Instruction::JmpCC(CondCode::E, l),
        ],
        JumpIfNotZero(v, l) => vec![
            Instruction::Cmp(asm_type(&v), Operand::Imm(0), convert_val(&v)),
            Instruction::JmpCC(CondCode::NE, l),
        ],
        Label(l) => vec![Instruction::Label(l)],
        FunCall { f, args, dst } => convert_function_call(&f, &args, dst.as_ref()),
        SignExtend { src, dst } => vec![Instruction::Movsx {
            src_type: asm_type(&src),
            dst_type: asm_type(&dst),
            src: convert_val(&src),
            dst: convert_val(&dst),
        }],
        ZeroExtend { src, dst } => vec![Instruction::MovZeroExtend {
            src_type: asm_type(&src),
            dst_type: asm_type(&dst),
            src: convert_val(&src),
            dst: convert_val(&dst),
        }],
        DoubleToInt { src, dst } => vec![Instruction::Cvttsd2si(
            asm_type(&dst),
            convert_val(&src),
            convert_val(&dst),
        )],
        IntToDouble { src, dst } => vec![Instruction::Cvtsi2sd(
            asm_type(&src),
            convert_val(&src),
            convert_val(&dst),
        )],
        UIntToDouble { src, dst } => vec![Instruction::Cvtsi2sd(
            asm_type(&src),
            convert_val(&src),
            convert_val(&dst),
        )],
        DoubleToUInt { src, dst } => vec![Instruction::Cvttsd2si(
            asm_type(&dst),
            convert_val(&src),
            convert_val(&dst),
        )],
        Truncate { src, dst } => vec![Instruction::Mov(
            asm_type(&dst),
            convert_val(&src),
            convert_val(&dst),
        )],
        CopyToOffset { src, dst, offset } => vec![Instruction::Mov(
            asm_type(&src),
            convert_val(&src),
            Operand::PseudoMem(dst, offset as i32),
        )],
        CopyFromOffset { src, dst, offset } => vec![Instruction::Mov(
            asm_type(&dst),
            Operand::PseudoMem(src, offset as i32),
            convert_val(&dst),
        )],
        AddPtr { ptr, index, scale, dst } => vec![
            Instruction::Mov(AsmType::Quadword, convert_val(&ptr), Operand::Reg(Reg::R8)),
            Instruction::Mov(AsmType::Quadword, convert_val(&index), Operand::Reg(Reg::R9)),
            Instruction::Lea(
                Operand::Indexed { base: Reg::R8, index: Reg::R9, scale: scale as i32 },
                convert_val(&dst),
            ),
        ],
    }
}

fn pass_params(params: &[TackyVal]) -> Vec<Instruction> {
    let mut insts = Vec::new();
    let (int_args, dbl_args, stack_args) = classify_params(params);
    for (i, (t, op)) in int_args.iter().enumerate() {
        insts.push(Instruction::Mov(t.clone(), Operand::Reg(INT_PARAM_REGS[i]), op.clone()));
    }
    for (i, op) in dbl_args.iter().enumerate() {
        insts.push(Instruction::Mov(AsmType::Double, Operand::Reg(DBL_PARAM_REGS[i]), op.clone()));
    }
    for (i, (t, op)) in stack_args.iter().enumerate() {
        let dst = Operand::Memory(Reg::BP, 16 + (i as i32) * 8);
        insts.push(Instruction::Mov(t.clone(), dst, op.clone()));
    }
    insts
}

fn convert_top_level(tl: TackyTopLevel) -> AsmTopLevel {
    match tl {
        TackyTopLevel::Function { name, global, params, body } => {
            let params_vals: Vec<TackyVal> = params.iter().map(|p| TackyVal::Var(p.clone())).collect();
            let mut insts = pass_params(&params_vals);
            for i in body {
                insts.extend(convert_instruction(i));
            }
            AsmTopLevel::Function { name, global, instructions: insts }
        }
        TackyTopLevel::StaticVariable { name, t, global, init } => AsmTopLevel::StaticVariable {
            name,
            global,
            alignment: type_utils::get_alignment(&t) as i32,
            init,
        },
        TackyTopLevel::StaticConstant { name, t, init } => AsmTopLevel::StaticConstant {
            name,
            alignment: type_utils::get_alignment(&t) as i32,
            init,
        },
    }
}

fn convert_constant((key, (name, alignment)): (u64, (String, i32))) -> AsmTopLevel {
    let dbl = f64::from_bits(key);
    assembly_symbols::add_constant(&name, AsmType::Double);
    AsmTopLevel::StaticConstant { name, alignment, init: StaticInit::DoubleInit(dbl) }
}

fn convert_symbol(name: &str, sym: symbols::Entry) {
    use symbols::IdentifierAttrs::*;
    match sym.attrs {
        FunAttr { defined, .. } => {
            if let Type::FunType { param_types, ret_type } = sym.t {
                let (ret_regs, _) = classify_return_type(&ret_type);
                let param_regs: Vec<Reg> = param_types
                    .iter()
                    .map(|t| classify_return_type(t).0.first().cloned().unwrap_or(Reg::AX))
                    .collect();
                assembly_symbols::add_fun(name, defined, false, param_regs, ret_regs);
            }
        }
        ConstAttr(_) => {
            assembly_symbols::add_constant(name, convert_type(&sym.t));
        }
        StaticAttr { .. } => {
            assembly_symbols::add_var(name, convert_type(&sym.t), true);
        }
        LocalAttr => {
            assembly_symbols::add_var(name, convert_type(&sym.t), false);
        }
    }
}

pub fn generate(TackyProgram(tls): TackyProgram) -> AsmProgram {
    CONSTANTS.lock().unwrap().clear();
    let mut asms: Vec<AsmTopLevel> = tls.into_iter().map(convert_top_level).collect();
    let consts: Vec<AsmTopLevel> = CONSTANTS
        .lock()
        .unwrap()
        .clone()
        .into_iter()
        .map(convert_constant)
        .collect();
    asms.splice(0..0, consts);
    symbols::iter(|n, e| convert_symbol(n, e.clone()));
    AsmProgram(asms)
}

