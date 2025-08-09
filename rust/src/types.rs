use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Char,
    SChar,
    UChar,
    Int,
    Long,
    UInt,
    ULong,
    Double,
    Pointer(Box<Type>),
    Void,
    Array { elem_type: Box<Type>, size: usize },
    FunType { param_types: Vec<Type>, ret_type: Box<Type> },
    Structure(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Char => write!(f, "Char"),
            Type::SChar => write!(f, "SChar"),
            Type::UChar => write!(f, "UChar"),
            Type::Int => write!(f, "Int"),
            Type::Long => write!(f, "Long"),
            Type::UInt => write!(f, "UInt"),
            Type::ULong => write!(f, "ULong"),
            Type::Double => write!(f, "Double"),
            Type::Pointer(t) => write!(f, "{}*", t),
            Type::Void => write!(f, "Void"),
            Type::Array { elem_type, size } => write!(f, "({}, {})", elem_type, size),
            Type::FunType { param_types, ret_type } => {
                write!(f, "FunType {{ param_types = [")?;
                for (i, p) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, "]; ret_type = {} }}", ret_type)
            }
            Type::Structure(tag) => write!(f, "Structure({})", tag),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Type;

    #[test]
    fn pointer_display() {
        let t = Type::Pointer(Box::new(Type::Int));
        assert_eq!(format!("{}", t), "Int*");
    }

    #[test]
    fn array_display() {
        let t = Type::Array { elem_type: Box::new(Type::Char), size: 4 };
        assert_eq!(format!("{}", t), "(Char, 4)");
    }
}

