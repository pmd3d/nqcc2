use std::fmt::{self, Write};

use crate::consts::Const;
use crate::initializers::StaticInit;
use crate::tacky::{BinaryOperator, Instruction, Program, TackyVal, TopLevel, UnaryOperator};
use crate::types::Type;

fn const_to_string(c: &Const) -> String {
    c.to_string()
}

fn pp_tacky_val(v: &TackyVal) -> String {
    match v {
        TackyVal::Constant(c) => const_to_string(c),
        TackyVal::Var(s) => s.clone(),
    }
}

fn pp_unary_operator(op: &UnaryOperator) -> &'static str {
    match op {
        UnaryOperator::Complement => "~",
        UnaryOperator::Negate => "-",
        UnaryOperator::Not => "!",
    }
}

fn pp_binary_operator(op: &BinaryOperator) -> &'static str {
    match op {
        BinaryOperator::Add => "+",
        BinaryOperator::Subtract => "-",
        BinaryOperator::Multiply => "*",
        BinaryOperator::Divide => "/",
        BinaryOperator::Mod => "%",
        BinaryOperator::Equal => "==",
        BinaryOperator::NotEqual => "!=",
        BinaryOperator::LessThan => "<",
        BinaryOperator::LessOrEqual => "<=",
        BinaryOperator::GreaterThan => ">",
        BinaryOperator::GreaterOrEqual => ">=",
    }
}

fn pp_instruction(inst: &Instruction, out: &mut String) {
    use Instruction::*;
    match inst {
        Return(None) => {
            writeln!(out, "Return").unwrap();
        }
        Return(Some(v)) => {
            writeln!(out, "Return({})", pp_tacky_val(v)).unwrap();
        }
        Unary { op, src, dst } => {
            writeln!(out, "{} = {}{}", pp_tacky_val(dst), pp_unary_operator(op), pp_tacky_val(src)).unwrap();
        }
        Binary { op, src1, src2, dst } => {
            writeln!(out, "{} = {} {} {}", pp_tacky_val(dst), pp_tacky_val(src1), pp_binary_operator(op), pp_tacky_val(src2)).unwrap();
        }
        Copy { src, dst } => {
            writeln!(out, "{} = {}", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        Jump(target) => {
            writeln!(out, "Jump({})", target).unwrap();
        }
        JumpIfZero(cond, target) => {
            writeln!(out, "JumpIfZero({}, {})", pp_tacky_val(cond), target).unwrap();
        }
        JumpIfNotZero(cond, target) => {
            writeln!(out, "JumpIfNotZero({}, {})", pp_tacky_val(cond), target).unwrap();
        }
        Label(s) => {
            writeln!(out, "{}:", s).unwrap();
        }
        FunCall { f, args, dst } => {
            let args_str = args.iter().map(pp_tacky_val).collect::<Vec<_>>().join(", ");
            if let Some(d) = dst {
                writeln!(out, "{} = {}({})", pp_tacky_val(d), f, args_str).unwrap();
            } else {
                writeln!(out, "{}({})", f, args_str).unwrap();
            }
        }
        SignExtend { src, dst } => {
            writeln!(out, "{} = SignExtend({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        ZeroExtend { src, dst } => {
            writeln!(out, "{} = ZeroExtend({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        Truncate { src, dst } => {
            writeln!(out, "{} = Truncate({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        DoubleToInt { src, dst } => {
            writeln!(out, "{} = DoubleToInt({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        DoubleToUInt { src, dst } => {
            writeln!(out, "{} = DoubleToUInt({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        IntToDouble { src, dst } => {
            writeln!(out, "{} = IntToDouble({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        UIntToDouble { src, dst } => {
            writeln!(out, "{} = UIntToDouble({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        GetAddress { src, dst } => {
            writeln!(out, "{} = GetAddress({})", pp_tacky_val(dst), pp_tacky_val(src)).unwrap();
        }
        Load { src_ptr, dst } => {
            writeln!(out, "{} = Load({})", pp_tacky_val(dst), pp_tacky_val(src_ptr)).unwrap();
        }
        Store { src, dst_ptr } => {
            writeln!(out, "*({}) = {}", pp_tacky_val(dst_ptr), pp_tacky_val(src)).unwrap();
        }
        AddPtr { ptr, index, scale, dst } => {
            writeln!(out, "{} = {} + {} * {}", pp_tacky_val(dst), pp_tacky_val(ptr), pp_tacky_val(index), scale).unwrap();
        }
        CopyToOffset { src, dst, offset } => {
            writeln!(out, "{}[offset = {}] = {}", dst, offset, pp_tacky_val(src)).unwrap();
        }
        CopyFromOffset { src, offset, dst } => {
            writeln!(out, "{} = {}[offset = {}]", pp_tacky_val(dst), src, offset).unwrap();
        }
    }
}

fn pp_static_init(init: &[StaticInit]) -> String {
    let mut parts = Vec::new();
    for i in init {
        parts.push(match i {
            StaticInit::CharInit(c) => format!("{}", c),
            StaticInit::UCharInit(c) => format!("{}", c),
            StaticInit::IntInit(v) => format!("{}", v),
            StaticInit::LongInit(v) => format!("{}l", v),
            StaticInit::UIntInit(v) => format!("{}u", v),
            StaticInit::ULongInit(v) => format!("{}ul", v),
            StaticInit::DoubleInit(v) => format!("{}", v),
            StaticInit::ZeroInit(sz) => format!("ZeroInit({})", sz),
            StaticInit::StringInit(s, _) => format!("\"{}\"", s),
            StaticInit::PointerInit(p) => format!("&{}", p),
        });
    }
    format!("{{{}}}", parts.join(", "))
}

fn pp_top_level(tl: &TopLevel) -> String {
    match tl {
        TopLevel::Function { name, global, params, body } => {
            let mut out = String::new();
            if *global {
                out.push_str("global ");
            }
            out.push_str(&format!("{}({}):\n", name, params.join(", ")));
            for inst in body {
                pp_instruction(inst, &mut out);
            }
            out
        }
        TopLevel::StaticVariable { name, t, global, init } => {
            let mut s = String::new();
            if *global {
                s.push_str("global ");
            }
            s.push_str(&format!("{} {} = {}", t, name, pp_static_init(init)));
            s
        }
        TopLevel::StaticConstant { name, t, init } => {
            format!("const {} {} = {}", t, name, pp_static_init(&vec![init.clone()]))
        }
    }
}

pub fn pp_program(prog: &Program) -> String {
    let mut out = String::new();
    for (i, tl) in prog.0.iter().enumerate() {
        if i > 0 {
            out.push_str("\n\n");
        }
        out.push_str(&pp_top_level(tl));
    }
    out
}

/// Debug print a tacky program to a file.  This is a greatly simplified
/// version of the OCaml implementation and does not honour any runtime
/// settings; it always writes out the program using the filename with a
/// `.debug.tacky` suffix.
pub fn debug_print_tacky(src_filename: &str, tacky_prog: &Program) {
    let out = pp_program(tacky_prog);
    let tacky_file = format!("{}.debug.tacky", src_filename);
    if let Ok(mut f) = std::fs::File::create(&tacky_file) {
        use std::io::Write as IoWrite;
        let _ = f.write_all(out.as_bytes());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tacky::{Instruction, TopLevel};

    #[test]
    fn print_simple_function() {
        let prog = Program(vec![TopLevel::Function {
            name: "main".into(),
            global: true,
            params: vec![],
            body: vec![Instruction::Return(Some(TackyVal::Constant(Const::Int(0))))],
        }]);
        let printed = pp_program(&prog);
        assert!(printed.contains("Return(0)"));
    }
}

