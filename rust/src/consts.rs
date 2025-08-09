use std::fmt;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Char(i8),
    UChar(u8),
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Char(c) => write!(f, "{}", c),
            Const::UChar(c) => write!(f, "{}", c),
            Const::Int(i) => write!(f, "{}", i),
            Const::Long(l) => write!(f, "{}L", l),
            Const::UInt(u) => write!(f, "{}U", u),
            Const::ULong(u) => write!(f, "{}UL", u),
            Const::Double(d) => write!(f, "{}", d),
        }
    }
}

pub fn type_of_const(c: &Const) -> Type {
    match c {
        Const::Char(_) => Type::SChar,
        Const::UChar(_) => Type::UChar,
        Const::Int(_) => Type::Int,
        Const::Long(_) => Type::Long,
        Const::UInt(_) => Type::UInt,
        Const::ULong(_) => Type::ULong,
        Const::Double(_) => Type::Double,
    }
}
