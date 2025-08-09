use std::fmt;
use crate::consts::Const;

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Constant(Const),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(Option<Exp>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program;

pub mod typed {
    use crate::types::Type;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Exp<T> {
        pub e: T,
        pub t: Type,
    }
}
