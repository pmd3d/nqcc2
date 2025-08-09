use crate::types::Type;
use crate::type_utils;

#[derive(Debug, Clone, PartialEq)]
pub enum StaticInit {
    CharInit(i8),
    UCharInit(u8),
    IntInit(i32),
    LongInit(i64),
    UIntInit(u32),
    ULongInit(u64),
    DoubleInit(f64),
    // zero out arbitrary number of bytes
    ZeroInit(usize),
    // flag indicates whether the string is null terminated
    StringInit(String, bool),
    // pointer to static variable
    PointerInit(String),
}

pub fn zero(t: &Type) -> Vec<StaticInit> {
    vec![StaticInit::ZeroInit(type_utils::get_size(t))]
}

pub fn is_zero(init: &StaticInit) -> bool {
    match init {
        StaticInit::CharInit(c) => *c == 0,
        StaticInit::IntInit(i) => *i == 0,
        StaticInit::LongInit(l) => *l == 0,
        StaticInit::UCharInit(c) => *c == 0,
        StaticInit::UIntInit(u) => *u == 0,
        StaticInit::ULongInit(u) => *u == 0,
        // NOTE: consider all doubles non-zero since we don't know
        // if it's zero or negative zero
        StaticInit::DoubleInit(_) => false,
        StaticInit::ZeroInit(_) => true,
        StaticInit::PointerInit(_) | StaticInit::StringInit(_, _) => false,
    }
}
