use crate::consts::{self, Const};
use crate::types::Type;

fn const_to_i64(c: &Const) -> i64 {
    match c {
        Const::Char(v) => *v as i64,
        Const::UChar(v) => *v as i64,
        Const::Int(v) => *v as i64,
        Const::Long(v) => *v,
        Const::UInt(v) => *v as i64,
        Const::ULong(v) => *v as i64,
        Const::Double(d) => *d as i64,
    }
}

fn const_of_i64(v: i64, target: &Type) -> Const {
    match target {
        Type::Char | Type::SChar => Const::Char(v as i8),
        Type::UChar => Const::UChar(v as u8),
        Type::Int => Const::Int(v as i32),
        Type::Long => Const::Long(v),
        Type::UInt => Const::UInt(v as u32),
        Type::ULong | Type::Pointer(_) => Const::ULong(v as u64),
        Type::Double => Const::Double(v as f64),
        _ => panic!("can't convert constant to non-scalar type {}", target),
    }
}

pub fn const_convert(target_type: &Type, c: Const) -> Const {
    if &consts::type_of_const(&c) == target_type {
        c
    } else {
        match (target_type, &c) {
            (Type::Double, Const::ULong(v)) => Const::Double(*v as f64),
            (Type::ULong, Const::Double(d)) => Const::ULong(*d as u64),
            _ => {
                let as_i64 = const_to_i64(&c);
                const_of_i64(as_i64, target_type)
            }
        }
    }
}
