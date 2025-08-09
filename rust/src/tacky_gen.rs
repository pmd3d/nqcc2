use crate::ast::{self, typed};
use crate::consts::Const;
use crate::initializers;
use crate::symbols::{self, IdentifierAttrs, InitialValue};
use crate::tacky::{self, BinaryOperator as TBinOp, Instruction, Program as TackyProgram, TackyVal, TopLevel, UnaryOperator as TUnOp};
use crate::type_table::{self, MemberEntry};
use crate::type_utils;
use crate::types::Type;
use crate::unique_ids;

/// label helpers for break/continue
fn break_label(id: &str) -> String {
    format!("break.{}", id)
}

fn continue_label(id: &str) -> String {
    format!("continue.{}", id)
}

fn dummy_operand() -> TackyVal {
    TackyVal::Constant(Const::Int(0))
}

fn create_tmp(t: &Type) -> String {
    let name = unique_ids::make_temporary();
    symbols::add_automatic_var(name.clone(), t.clone());
    name
}

fn get_ptr_scale(t: &Type) -> usize {
    match t {
        Type::Pointer(inner) => type_utils::get_size(inner),
        _ => panic!("Internal error: tried to get scale of non-pointer type: {}", t),
    }
}

fn get_member_offset(member: &str, t: &Type) -> usize {
    match t {
        Type::Structure(tag) => {
            let entry = type_table::find(tag);
            entry
                .members
                .get(member)
                .map(|m| m.offset)
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error: failed to find member {} in structure {}",
                        member, tag
                    )
                })
        }
        _ => panic!(
            "Internal error: tried to get offset of member {} within non-structure type {}",
            member, t
        ),
    }
}

fn get_member_pointer_offset(member: &str, t: &Type) -> usize {
    match t {
        Type::Pointer(inner) => get_member_offset(member, inner),
        _ => panic!(
            "Internal error: trying to get member through pointer but {} is not a pointer type",
            t
        ),
    }
}

fn convert_op(op: &ast::UnaryOperator) -> TUnOp {
    match op {
        ast::UnaryOperator::Complement => TUnOp::Complement,
        ast::UnaryOperator::Negate => TUnOp::Negate,
        ast::UnaryOperator::Not => TUnOp::Not,
    }
}

fn convert_binop(op: &ast::BinaryOperator) -> TBinOp {
    match op {
        ast::BinaryOperator::Add => TBinOp::Add,
        ast::BinaryOperator::Subtract => TBinOp::Subtract,
        ast::BinaryOperator::Multiply => TBinOp::Multiply,
        ast::BinaryOperator::Divide => TBinOp::Divide,
        ast::BinaryOperator::Mod => TBinOp::Mod,
        ast::BinaryOperator::Equal => TBinOp::Equal,
        ast::BinaryOperator::NotEqual => TBinOp::NotEqual,
        ast::BinaryOperator::LessThan => TBinOp::LessThan,
        ast::BinaryOperator::LessOrEqual => TBinOp::LessOrEqual,
        ast::BinaryOperator::GreaterThan => TBinOp::GreaterThan,
        ast::BinaryOperator::GreaterOrEqual => TBinOp::GreaterOrEqual,
        ast::BinaryOperator::And | ast::BinaryOperator::Or => {
            panic!("Internal error, cannot convert these directly to TACKY binops")
        }
    }
}

fn eval_size(t: &Type) -> TackyVal {
    let size = type_utils::get_size(t);
    TackyVal::Constant(Const::ULong(size as u64))
}

/// An expression result that may or may not be lvalue converted.
enum ExpResult {
    PlainOperand(TackyVal),
    DereferencedPointer(TackyVal),
    SubObject { base: String, offset: usize },
}

/// Return list of instructions to evaluate expression and resulting ExpResult.
fn emit_tacky_for_exp(exp: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    match &exp.e {
        typed::InnerExp::Constant(c) => {
            (vec![], ExpResult::PlainOperand(TackyVal::Constant(c.clone())))
        }
        typed::InnerExp::Var(v) => {
            (vec![], ExpResult::PlainOperand(TackyVal::Var(v.clone())))
        }
        typed::InnerExp::String(s) => {
            let str_id = symbols::add_string(s);
            (vec![], ExpResult::PlainOperand(TackyVal::Var(str_id)))
        }
        typed::InnerExp::Cast { target_type, e } => emit_cast_expression(target_type, e),
        typed::InnerExp::Unary(op, inner) => emit_unary_expression(&exp.t, op, inner),
        typed::InnerExp::Binary(ast::BinaryOperator::And, e1, e2) => emit_and_expression(e1, e2),
        typed::InnerExp::Binary(ast::BinaryOperator::Or, e1, e2) => emit_or_expression(e1, e2),
        typed::InnerExp::Binary(ast::BinaryOperator::Add, e1, e2)
            if type_utils::is_pointer(&exp.t) => emit_pointer_addition(&exp.t, e1, e2),
        typed::InnerExp::Binary(ast::BinaryOperator::Subtract, ptr, index)
            if type_utils::is_pointer(&exp.t) => emit_subtraction_from_pointer(&exp.t, ptr, index),
        typed::InnerExp::Binary(ast::BinaryOperator::Subtract, e1, e2)
            if type_utils::is_pointer(&e1.t) => emit_pointer_diff(&exp.t, e1, e2),
        typed::InnerExp::Binary(op, e1, e2) => emit_binary_expression(&exp.t, op, e1, e2),
        typed::InnerExp::Assignment(lhs, rhs) => emit_assignment(lhs, rhs),
        typed::InnerExp::Conditional {
            condition,
            then_result,
            else_result,
        } => emit_conditional_expression(&exp.t, condition, then_result, else_result),
        typed::InnerExp::FunCall { f, args } => emit_fun_call(&exp.t, f, args),
        typed::InnerExp::Dereference(inner) => emit_dereference(inner),
        typed::InnerExp::AddrOf(inner) => emit_addr_of(&exp.t, inner),
        typed::InnerExp::Subscript { ptr, index } => emit_subscript(&exp.t, ptr, index),
        typed::InnerExp::SizeOfT(t) => {
            (vec![], ExpResult::PlainOperand(eval_size(t)))
        }
        typed::InnerExp::SizeOf(inner) => {
            (vec![], ExpResult::PlainOperand(eval_size(&inner.t)))
        }
        typed::InnerExp::Dot { strct, member } => {
            emit_dot_operator(&exp.t, strct, member)
        }
        typed::InnerExp::Arrow { strct, member } => {
            emit_arrow_operator(&exp.t, strct, member)
        }
    }
}

fn emit_unary_expression(t: &Type, op: &ast::UnaryOperator, inner: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_inner, v) = emit_tacky_and_convert(inner);
    let dst_name = create_tmp(t);
    let dst = TackyVal::Var(dst_name.clone());
    let tacky_op = convert_op(op);
    eval_inner.push(Instruction::Unary { op: tacky_op, src: v, dst: dst.clone() });
    (eval_inner, ExpResult::PlainOperand(dst))
}

fn emit_cast_expression(target_type: &Type, inner: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (eval_inner, result) = emit_tacky_and_convert(inner);
    let inner_type = type_utils::get_type(inner);
    if inner_type == *target_type || *target_type == Type::Void {
        (eval_inner, ExpResult::PlainOperand(result))
    } else {
        let dst_name = create_tmp(target_type);
        let dst = TackyVal::Var(dst_name.clone());
        let cast_instruction = match (target_type, &inner_type) {
            (Type::Double, _) => {
                if type_utils::is_signed(&inner_type) {
                    Instruction::IntToDouble { src: result.clone(), dst: dst.clone() }
                } else {
                    Instruction::UIntToDouble { src: result.clone(), dst: dst.clone() }
                }
            }
            (_, Type::Double) => {
                if type_utils::is_signed(target_type) {
                    Instruction::DoubleToInt { src: result.clone(), dst: dst.clone() }
                } else {
                    Instruction::DoubleToUInt { src: result.clone(), dst: dst.clone() }
                }
            }
            _ => {
                if type_utils::get_size(target_type) == type_utils::get_size(&inner_type) {
                    Instruction::Copy { src: result.clone(), dst: dst.clone() }
                } else if type_utils::get_size(target_type) < type_utils::get_size(&inner_type) {
                    Instruction::Truncate { src: result.clone(), dst: dst.clone() }
                } else if type_utils::is_signed(&inner_type) {
                    Instruction::SignExtend { src: result.clone(), dst: dst.clone() }
                } else {
                    Instruction::ZeroExtend { src: result.clone(), dst: dst.clone() }
                }
            }
        };
        let mut instructions = eval_inner;
        instructions.push(cast_instruction);
        (instructions, ExpResult::PlainOperand(dst))
    }
}

fn emit_pointer_addition(t: &Type, e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let dst_name = create_tmp(t);
    let dst = TackyVal::Var(dst_name.clone());
    let (ptr, index) = if e1.t == *t { (v1.clone(), v2.clone()) } else { (v2.clone(), v1.clone()) };
    let scale = get_ptr_scale(t);
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::AddPtr { ptr, index, scale, dst: dst.clone() });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_subscript(t: &Type, e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (instructions, result) = emit_pointer_addition(&Type::Pointer(Box::new(t.clone())), e1, e2);
    match result {
        ExpResult::PlainOperand(dst) => (instructions, ExpResult::DereferencedPointer(dst)),
        _ => panic!("Internal error: expected result of pointer addition to be lvalue converted"),
    }
}

fn emit_subtraction_from_pointer(t: &Type, ptr_e: &typed::Exp, idx_e: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, ptr) = emit_tacky_and_convert(ptr_e);
    let (mut eval_v2, index) = emit_tacky_and_convert(idx_e);
    let dst_name = create_tmp(t);
    let dst = TackyVal::Var(dst_name.clone());
    let negated_index = TackyVal::Var(create_tmp(&Type::Long));
    let scale = get_ptr_scale(t);
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::Unary { op: TUnOp::Negate, src: index.clone(), dst: negated_index.clone() });
    instructions.push(Instruction::AddPtr { ptr, index: negated_index, scale, dst: dst.clone() });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_pointer_diff(t: &Type, e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let ptr_diff = TackyVal::Var(create_tmp(&Type::Long));
    let dst_name = create_tmp(t);
    let dst = TackyVal::Var(dst_name.clone());
    let scale = TackyVal::Constant(Const::Long(get_ptr_scale(&e1.t) as i64));
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::Binary { op: TBinOp::Subtract, src1: v1, src2: v2, dst: ptr_diff.clone() });
    instructions.push(Instruction::Binary { op: TBinOp::Divide, src1: ptr_diff, src2: scale, dst: dst.clone() });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_binary_expression(t: &Type, op: &ast::BinaryOperator, e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let dst_name = create_tmp(t);
    let dst = TackyVal::Var(dst_name.clone());
    let tacky_op = convert_binop(op);
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::Binary { op: tacky_op, src1: v1, src2: v2, dst: dst.clone() });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_and_expression(e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let false_label = unique_ids::make_label("and_false");
    let end_label = unique_ids::make_label("and_end");
    let dst_name = create_tmp(&Type::Int);
    let dst = TackyVal::Var(dst_name.clone());
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.push(Instruction::JumpIfZero(v1, false_label.clone()));
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::JumpIfZero(v2, false_label.clone()));
    instructions.push(Instruction::Copy { src: TackyVal::Constant(Const::Int(1)), dst: dst.clone() });
    instructions.push(Instruction::Jump(end_label.clone()));
    instructions.push(Instruction::Label(false_label));
    instructions.push(Instruction::Copy { src: TackyVal::Constant(Const::Int(0)), dst: dst.clone() });
    instructions.push(Instruction::Label(end_label));
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_or_expression(e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let true_label = unique_ids::make_label("or_true");
    let end_label = unique_ids::make_label("or_end");
    let dst_name = create_tmp(&Type::Int);
    let dst = TackyVal::Var(dst_name.clone());
    let mut instructions = Vec::new();
    instructions.append(&mut eval_v1);
    instructions.push(Instruction::JumpIfNotZero(v1, true_label.clone()));
    instructions.append(&mut eval_v2);
    instructions.push(Instruction::JumpIfNotZero(v2, true_label.clone()));
    instructions.push(Instruction::Copy { src: TackyVal::Constant(Const::Int(0)), dst: dst.clone() });
    instructions.push(Instruction::Jump(end_label.clone()));
    instructions.push(Instruction::Label(true_label));
    instructions.push(Instruction::Copy { src: TackyVal::Constant(Const::Int(1)), dst: dst.clone() });
    instructions.push(Instruction::Label(end_label));
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_assignment(lhs: &typed::Exp, rhs: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut lhs_instructions, lval) = emit_tacky_for_exp(lhs);
    let (mut rhs_instructions, rval) = emit_tacky_and_convert(rhs);
    let mut instructions = Vec::new();
    instructions.append(&mut lhs_instructions);
    instructions.append(&mut rhs_instructions);
    match lval {
        ExpResult::PlainOperand(o) => {
            instructions.push(Instruction::Copy { src: rval.clone(), dst: o.clone() });
            (instructions, ExpResult::PlainOperand(o))
        }
        ExpResult::DereferencedPointer(ptr) => {
            instructions.push(Instruction::Store { src: rval.clone(), dst_ptr: ptr.clone() });
            (instructions, ExpResult::PlainOperand(rval))
        }
        ExpResult::SubObject { base, offset } => {
            instructions.push(Instruction::CopyToOffset { src: rval.clone(), dst: base.clone(), offset });
            (instructions, ExpResult::PlainOperand(rval))
        }
    }
}

fn emit_conditional_expression(t: &Type, condition: &typed::Exp, e1: &typed::Exp, e2: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (mut eval_cond, c) = emit_tacky_and_convert(condition);
    let (mut eval_v1, v1) = emit_tacky_and_convert(e1);
    let (mut eval_v2, v2) = emit_tacky_and_convert(e2);
    let e2_label = unique_ids::make_label("conditional_else");
    let end_label = unique_ids::make_label("conditional_end");
    let dst = if *t == Type::Void {
        dummy_operand()
    } else {
        let dst_name = create_tmp(t);
        TackyVal::Var(dst_name)
    };
    let mut instructions = Vec::new();
    instructions.append(&mut eval_cond);
    instructions.push(Instruction::JumpIfZero(c, e2_label.clone()));
    instructions.append(&mut eval_v1);
    if *t != Type::Void {
        instructions.push(Instruction::Copy { src: v1, dst: dst.clone() });
    }
    instructions.push(Instruction::Jump(end_label.clone()));
    instructions.push(Instruction::Label(e2_label));
    instructions.append(&mut eval_v2);
    if *t != Type::Void {
        instructions.push(Instruction::Copy { src: v2, dst: dst.clone() });
    }
    instructions.push(Instruction::Label(end_label));
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_fun_call(t: &Type, f: &str, args: &[typed::Exp]) -> (Vec<Instruction>, ExpResult) {
    let dst = if *t == Type::Void {
        None
    } else {
        let dst_name = create_tmp(t);
        Some(TackyVal::Var(dst_name))
    };
    let mut arg_instructions = Vec::new();
    let mut arg_vals = Vec::new();
    for arg in args {
        let (instrs, v) = emit_tacky_and_convert(arg);
        arg_instructions.push(instrs);
        arg_vals.push(v);
    }
    let mut instructions = Vec::new();
    for mut instrs in arg_instructions {
        instructions.append(&mut instrs);
    }
    instructions.push(Instruction::FunCall { f: f.to_string(), args: arg_vals, dst: dst.clone() });
    let dst_val = dst.unwrap_or_else(|| dummy_operand());
    (instructions, ExpResult::PlainOperand(dst_val))
}

fn emit_dereference(inner: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (instructions, result) = emit_tacky_and_convert(inner);
    (instructions, ExpResult::DereferencedPointer(result))
}

fn emit_dot_operator(t: &Type, strct: &typed::Exp, member: &str) -> (Vec<Instruction>, ExpResult) {
    let member_offset = get_member_offset(member, &strct.t);
    let (instructions, inner_object) = emit_tacky_for_exp(strct);
    match inner_object {
        ExpResult::PlainOperand(TackyVal::Var(v)) => {
            (instructions, ExpResult::SubObject { base: v, offset: member_offset })
        }
        ExpResult::SubObject { base, offset } => {
            (instructions, ExpResult::SubObject { base, offset: offset + member_offset })
        }
        ExpResult::DereferencedPointer(ptr) => {
            if member_offset == 0 {
                (instructions, ExpResult::DereferencedPointer(ptr))
            } else {
                let dst = TackyVal::Var(create_tmp(&Type::Pointer(Box::new(t.clone()))));
                let index = TackyVal::Constant(Const::Long(member_offset as i64));
                let mut instrs = instructions;
                instrs.push(Instruction::AddPtr { ptr, index, scale: 1, dst: dst.clone() });
                (instrs, ExpResult::DereferencedPointer(dst))
            }
        }
        ExpResult::PlainOperand(TackyVal::Constant(_)) => {
            panic!("Internal error: found dot operator applied to constant")
        }
    }
}

fn emit_arrow_operator(t: &Type, strct: &typed::Exp, member: &str) -> (Vec<Instruction>, ExpResult) {
    let member_offset = get_member_pointer_offset(member, &strct.t);
    let (mut instructions, ptr) = emit_tacky_and_convert(strct);
    if member_offset == 0 {
        (instructions, ExpResult::DereferencedPointer(ptr))
    } else {
        let dst = TackyVal::Var(create_tmp(&Type::Pointer(Box::new(t.clone()))));
        let index = TackyVal::Constant(Const::Long(member_offset as i64));
        instructions.push(Instruction::AddPtr { ptr, index, scale: 1, dst: dst.clone() });
        (instructions, ExpResult::DereferencedPointer(dst))
    }
}

fn emit_addr_of(t: &Type, inner: &typed::Exp) -> (Vec<Instruction>, ExpResult) {
    let (instructions, result) = emit_tacky_for_exp(inner);
    match result {
        ExpResult::PlainOperand(o) => {
            let dst = TackyVal::Var(create_tmp(t));
            let mut instrs = instructions;
            instrs.push(Instruction::GetAddress { src: o.clone(), dst: dst.clone() });
            (instrs, ExpResult::PlainOperand(dst))
        }
        ExpResult::DereferencedPointer(ptr) => (instructions, ExpResult::PlainOperand(ptr)),
        ExpResult::SubObject { base, offset } => {
            let dst = TackyVal::Var(create_tmp(t));
            let mut instrs = instructions;
            instrs.push(Instruction::GetAddress { src: TackyVal::Var(base.clone()), dst: dst.clone() });
            if offset == 0 {
                (instrs, ExpResult::PlainOperand(dst))
            } else {
                let index = TackyVal::Constant(Const::Long(offset as i64));
                instrs.push(Instruction::AddPtr { ptr: dst.clone(), index, scale: 1, dst: dst.clone() });
                (instrs, ExpResult::PlainOperand(dst))
            }
        }
    }
}

fn emit_tacky_and_convert(e: &typed::Exp) -> (Vec<Instruction>, TackyVal) {
    let (mut instructions, result) = emit_tacky_for_exp(e);
    match result {
        ExpResult::PlainOperand(o) => (instructions, o),
        ExpResult::DereferencedPointer(ptr) => {
            let dst = TackyVal::Var(create_tmp(&e.t));
            instructions.push(Instruction::Load { src_ptr: ptr, dst: dst.clone() });
            (instructions, dst)
        }
        ExpResult::SubObject { base, offset } => {
            let dst = TackyVal::Var(create_tmp(&e.t));
            instructions.push(Instruction::CopyFromOffset { src: base, offset, dst: dst.clone() });
            (instructions, dst)
        }
    }
}

fn emit_string_init(dst: &str, offset: usize, s: &[u8]) -> Vec<Instruction> {
    if s.is_empty() {
        vec![]
    } else if s.len() >= 8 {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(&s[..8]);
        let l = i64::from_le_bytes(arr);
        let instr = Instruction::CopyToOffset { src: TackyVal::Constant(Const::Long(l)), dst: dst.to_string(), offset };
        let mut rest = emit_string_init(dst, offset + 8, &s[8..]);
        let mut v = vec![instr];
        v.append(&mut rest);
        v
    } else if s.len() >= 4 {
        let mut arr = [0u8; 4];
        arr.copy_from_slice(&s[..4]);
        let i = i32::from_le_bytes(arr);
        let instr = Instruction::CopyToOffset { src: TackyVal::Constant(Const::Int(i)), dst: dst.to_string(), offset };
        let mut rest = emit_string_init(dst, offset + 4, &s[4..]);
        let mut v = vec![instr];
        v.append(&mut rest);
        v
    } else {
        let c = s[0] as i8;
        let instr = Instruction::CopyToOffset { src: TackyVal::Constant(Const::Char(c)), dst: dst.to_string(), offset };
        let mut rest = emit_string_init(dst, offset + 1, &s[1..]);
        let mut v = vec![instr];
        v.append(&mut rest);
        v
    }
}

fn emit_compound_init(name: &str, offset: usize, init: &typed::Initializer) -> Vec<Instruction> {
    match init {
        typed::Initializer::SingleInit(exp) => {
            match &exp.e {
                typed::InnerExp::String(s) => {
                    if let Type::Array { size, .. } = exp.t {
                        let mut bytes = s.as_bytes().to_vec();
                        if size > s.len() {
                            bytes.extend(vec![0; size - s.len()]);
                        }
                        emit_string_init(name, offset, &bytes)
                    } else {
                        let (mut eval_init, v) = emit_tacky_and_convert(exp);
                        eval_init.push(Instruction::CopyToOffset { src: v, dst: name.to_string(), offset });
                        eval_init
                    }
                }
                _ => {
                    let (mut eval_init, v) = emit_tacky_and_convert(exp);
                    eval_init.push(Instruction::CopyToOffset { src: v, dst: name.to_string(), offset });
                    eval_init
                }
            }
        }
        typed::Initializer::CompoundInit(t, inits) => match t {
            Type::Array { elem_type, .. } => {
                let mut all = Vec::new();
                for (idx, elem_init) in inits.iter().enumerate() {
                    let new_offset = offset + idx * type_utils::get_size(elem_type);
                    all.extend(emit_compound_init(name, new_offset, elem_init));
                }
                all
            }
            Type::Structure(tag) => {
                let members: Vec<MemberEntry> = type_table::get_members(tag);
                let mut all = Vec::new();
                for (elem_init, memb) in inits.iter().zip(members.iter()) {
                    let new_offset = offset + memb.offset;
                    all.extend(emit_compound_init(name, new_offset, elem_init));
                }
                all
            }
            _ => {
                panic!("Internal error: compound init has non-array type!");
            }
        },
    }
}

fn emit_tacky_for_statement(stmt: &typed::Statement) -> Vec<Instruction> {
    match stmt {
        typed::Statement::Return(e) => {
            let (eval_exp, v) = match e {
                Some(exp) => {
                    let (instrs, result) = emit_tacky_and_convert(exp);
                    (instrs, Some(result))
                }
                None => (vec![], None),
            };
            let mut instrs = eval_exp;
            instrs.push(Instruction::Return(v));
            instrs
        }
        typed::Statement::Expression(e) => {
            let (eval_exp, _) = emit_tacky_for_exp(e);
            eval_exp
        }
        typed::Statement::If { condition, then_clause, else_clause } => {
            emit_tacky_for_if_statement(condition, then_clause, else_clause)
        }
        typed::Statement::Compound(typed::Block(items)) => {
            items.iter().flat_map(emit_tacky_for_block_item).collect()
        }
        typed::Statement::Break(id) => vec![Instruction::Jump(break_label(id))],
        typed::Statement::Continue(id) => vec![Instruction::Jump(continue_label(id))],
        typed::Statement::DoWhile { body, condition, id } => {
            emit_tacky_for_do_loop(body, condition, id)
        }
        typed::Statement::While { condition, body, id } => {
            emit_tacky_for_while_loop(condition, body, id)
        }
        typed::Statement::For { init, condition, post, body, id } => {
            emit_tacky_for_for_loop(init, condition, post, body, id)
        }
        typed::Statement::Null => vec![],
    }
}

fn emit_tacky_for_block_item(item: &typed::BlockItem) -> Vec<Instruction> {
    match item {
        typed::BlockItem::S(s) => emit_tacky_for_statement(s),
        typed::BlockItem::D(d) => emit_local_declaration(d),
    }
}

fn emit_local_declaration(d: &typed::Declaration) -> Vec<Instruction> {
    match d {
        typed::Declaration::VarDecl(vd) => {
            if vd.storage_class.is_some() {
                vec![]
            } else {
                emit_var_declaration(vd)
            }
        }
        _ => vec![],
    }
}

fn emit_var_declaration(vd: &typed::VariableDeclaration) -> Vec<Instruction> {
    match &vd.init {
        Some(init @ typed::Initializer::SingleInit(exp))
            if matches!(exp.e, typed::InnerExp::String(_)) && matches!(exp.t, Type::Array { .. }) =>
        {
            emit_compound_init(&vd.name, 0, init)
        }
        Some(typed::Initializer::SingleInit(exp)) => {
            let lhs = typed::Exp { e: typed::InnerExp::Var(vd.name.clone()), t: vd.var_type.clone() };
            let (instrs, _) = emit_assignment(&lhs, exp);
            instrs
        }
        Some(init) => emit_compound_init(&vd.name, 0, init),
        None => vec![],
    }
}

fn emit_tacky_for_if_statement(condition: &typed::Exp, then_clause: &typed::Statement, else_clause: &Option<Box<typed::Statement>>) -> Vec<Instruction> {
    match else_clause {
        None => {
            let end_label = unique_ids::make_label("if_end");
            let (mut eval_condition, c) = emit_tacky_and_convert(condition);
            eval_condition.push(Instruction::JumpIfZero(c, end_label.clone()));
            let mut instructions = eval_condition;
            instructions.extend(emit_tacky_for_statement(then_clause));
            instructions.push(Instruction::Label(end_label));
            instructions
        }
        Some(else_clause) => {
            let else_label = unique_ids::make_label("else");
            let end_label = unique_ids::make_label("if_end");
            let (mut eval_condition, c) = emit_tacky_and_convert(condition);
            eval_condition.push(Instruction::JumpIfZero(c, else_label.clone()));
            let mut instructions = eval_condition;
            instructions.extend(emit_tacky_for_statement(then_clause));
            instructions.push(Instruction::Jump(end_label.clone()));
            instructions.push(Instruction::Label(else_label));
            instructions.extend(emit_tacky_for_statement(else_clause));
            instructions.push(Instruction::Label(end_label));
            instructions
        }
    }
}

fn emit_tacky_for_do_loop(body: &typed::Statement, condition: &typed::Exp, id: &str) -> Vec<Instruction> {
    let start_label = unique_ids::make_label("do_loop_start");
    let cont_label = continue_label(id);
    let br_label = break_label(id);
    let (mut eval_condition, c) = emit_tacky_and_convert(condition);
    let mut instructions = Vec::new();
    instructions.push(Instruction::Label(start_label.clone()));
    instructions.extend(emit_tacky_for_statement(body));
    instructions.push(Instruction::Label(cont_label.clone()));
    instructions.append(&mut eval_condition);
    instructions.push(Instruction::JumpIfNotZero(c, start_label));
    instructions.push(Instruction::Label(br_label));
    instructions
}

fn emit_tacky_for_while_loop(condition: &typed::Exp, body: &typed::Statement, id: &str) -> Vec<Instruction> {
    let cont_label = continue_label(id);
    let br_label = break_label(id);
    let (mut eval_condition, c) = emit_tacky_and_convert(condition);
    let mut instructions = Vec::new();
    instructions.push(Instruction::Label(cont_label.clone()));
    instructions.append(&mut eval_condition);
    instructions.push(Instruction::JumpIfZero(c, br_label.clone()));
    instructions.extend(emit_tacky_for_statement(body));
    instructions.push(Instruction::Jump(cont_label));
    instructions.push(Instruction::Label(br_label));
    instructions
}

fn emit_tacky_for_for_loop(init: &typed::ForInit, condition: &Option<typed::Exp>, post: &Option<typed::Exp>, body: &typed::Statement, id: &str) -> Vec<Instruction> {
    let start_label = unique_ids::make_label("for_start");
    let cont_label = continue_label(id);
    let br_label = break_label(id);
    let for_init_instructions = match init {
        typed::ForInit::InitDecl(d) => emit_var_declaration(d),
        typed::ForInit::InitExp(e_opt) => match e_opt {
            Some(e) => {
                let (instrs, _) = emit_tacky_for_exp(e);
                instrs
            }
            None => vec![],
        },
    };
    let test_condition = match condition {
        Some(cond) => {
            let (mut instrs, v) = emit_tacky_and_convert(cond);
            instrs.push(Instruction::JumpIfZero(v, br_label.clone()));
            instrs
        }
        None => vec![],
    };
    let post_instructions = match post {
        Some(p) => {
            let (instrs, _) = emit_tacky_for_exp(p);
            instrs
        }
        None => vec![],
    };
    let mut instructions = Vec::new();
    instructions.extend(for_init_instructions);
    instructions.push(Instruction::Label(start_label.clone()));
    instructions.extend(test_condition);
    instructions.extend(emit_tacky_for_statement(body));
    instructions.push(Instruction::Label(cont_label.clone()));
    instructions.extend(post_instructions);
    instructions.push(Instruction::Jump(start_label));
    instructions.push(Instruction::Label(br_label));
    instructions
}

fn emit_fun_declaration(decl: &typed::Declaration) -> Option<TopLevel> {
    match decl {
        typed::Declaration::FunDecl(fun_decl) => {
            if let Some(typed::Block(block_items)) = &fun_decl.body {
                let global = symbols::is_global(&fun_decl.name);
                let mut body_instructions = Vec::new();
                for item in block_items {
                    body_instructions.extend(emit_tacky_for_block_item(item));
                }
                let extra_return = Instruction::Return(Some(TackyVal::Constant(Const::Int(0))));
                body_instructions.push(extra_return);
                Some(TopLevel::Function {
                    name: fun_decl.name.clone(),
                    global,
                    params: fun_decl.params.clone(),
                    body: body_instructions,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn convert_symbols_to_tacky(all_symbols: Vec<(String, symbols::Entry)>) -> Vec<TopLevel> {
    let mut result = Vec::new();
    for (name, entry) in all_symbols {
        match entry.attrs {
            IdentifierAttrs::StaticAttr { init, global } => match init {
                InitialValue::Initial(i) => result.push(TopLevel::StaticVariable { name, t: entry.t, global, init: i }),
                InitialValue::Tentative => result.push(TopLevel::StaticVariable { name, t: entry.t.clone(), global, init: initializers::zero(&entry.t) }),
                InitialValue::NoInitializer => {}
            },
            IdentifierAttrs::ConstAttr(init) => {
                result.push(TopLevel::StaticConstant { name, t: entry.t, init })
            }
            _ => {}
        }
    }
    result
}

pub fn r#gen(prog: typed::Program) -> tacky::Program {
    let typed::Program(decls) = prog;
    let mut tacky_fn_defs = Vec::new();
    for decl in &decls {
        if let Some(tl) = emit_fun_declaration(decl) {
            tacky_fn_defs.push(tl);
        }
    }
    let tacky_var_defs = convert_symbols_to_tacky(symbols::bindings());
    let mut all_defs = tacky_var_defs;
    all_defs.extend(tacky_fn_defs);
    TackyProgram(all_defs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn return_constant() {
        let stmt = typed::Statement::Return(Some(typed::Exp { e: typed::InnerExp::Constant(Const::Int(5)), t: Type::Int }));
        let instrs = emit_tacky_for_statement(&stmt);
        assert_eq!(instrs.len(), 1);
        match &instrs[0] {
            Instruction::Return(Some(TackyVal::Constant(Const::Int(i)))) => assert_eq!(*i, 5),
            other => panic!("unexpected instruction: {:?}", other),
        }
    }
}

