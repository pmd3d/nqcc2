use crate::ast::typed::Exp as TypedExp;
use crate::type_table;
use crate::types::Type;

pub fn get_type<T>(exp: &TypedExp<T>) -> Type {
    exp.t.clone()
}

pub fn set_type<T>(e: T, new_type: Type) -> TypedExp<T> {
    TypedExp { e, t: new_type }
}

pub fn get_size(t: &Type) -> usize {
    match t {
        Type::Char | Type::SChar | Type::UChar => 1,
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong | Type::Double | Type::Pointer(_) => 8,
        Type::Array { elem_type, size } => size * get_size(elem_type),
        Type::Structure(tag) => type_table::find(tag).size,
        Type::FunType { .. } | Type::Void => {
            panic!("Internal error: type doesn't have size: {}", t)
        }
    }
}

pub fn get_alignment(t: &Type) -> usize {
    match t {
        Type::Char | Type::SChar | Type::UChar => 1,
        Type::Int | Type::UInt => 4,
        Type::Long | Type::ULong | Type::Double | Type::Pointer(_) => 8,
        Type::Array { elem_type, .. } => get_alignment(elem_type),
        Type::Structure(tag) => type_table::find(tag).alignment,
        Type::FunType { .. } | Type::Void => {
            panic!("Internal error: type doesn't have alignment: {}", t)
        }
    }
}

pub fn is_signed(t: &Type) -> bool {
    match t {
        Type::Int | Type::Long | Type::Char | Type::SChar => true,
        Type::UInt | Type::ULong | Type::Pointer(_) | Type::UChar => false,
        Type::Double | Type::FunType { .. } | Type::Array { .. } | Type::Void | Type::Structure(_) => {
            panic!(
                "Internal error: signedness doesn't make sense for non-integral type {}",
                t
            )
        }
    }
}

pub fn is_pointer(t: &Type) -> bool {
    matches!(t, Type::Pointer(_))
}

pub fn is_integer(t: &Type) -> bool {
    matches!(
        t,
        Type::Char
            | Type::UChar
            | Type::SChar
            | Type::Int
            | Type::UInt
            | Type::Long
            | Type::ULong
    )
}

pub fn is_array(t: &Type) -> bool {
    matches!(t, Type::Array { .. })
}

pub fn is_character(t: &Type) -> bool {
    matches!(t, Type::Char | Type::SChar | Type::UChar)
}

pub fn is_arithmetic(t: &Type) -> bool {
    matches!(
        t,
        Type::Int
            | Type::UInt
            | Type::Long
            | Type::ULong
            | Type::Char
            | Type::UChar
            | Type::SChar
            | Type::Double
    )
}

pub fn is_scalar(t: &Type) -> bool {
    match t {
        Type::Array { .. } | Type::Void | Type::FunType { .. } | Type::Structure(_) => false,
        Type::Int
        | Type::UInt
        | Type::Long
        | Type::ULong
        | Type::Char
        | Type::UChar
        | Type::SChar
        | Type::Double
        | Type::Pointer(_) => true,
    }
}

pub fn is_complete(t: &Type) -> bool {
    match t {
        Type::Void => false,
        Type::Structure(tag) => type_table::mem(tag),
        _ => true,
    }
}

pub fn is_complete_pointer(t: &Type) -> bool {
    match t {
        Type::Pointer(inner) => is_complete(inner),
        _ => false,
    }
}
