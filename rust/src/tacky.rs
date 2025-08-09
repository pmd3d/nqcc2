use crate::consts::{self, Const};
use crate::initializers::StaticInit;
use crate::symbols;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TackyVal {
    Constant(Const),
    Var(String),
}

pub fn type_of_val(v: &TackyVal) -> Type {
    match v {
        TackyVal::Constant(c) => consts::type_of_const(c),
        TackyVal::Var(name) => symbols::get(name).t,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Return(Option<TackyVal>),
    SignExtend { src: TackyVal, dst: TackyVal },
    ZeroExtend { src: TackyVal, dst: TackyVal },
    DoubleToInt { src: TackyVal, dst: TackyVal },
    IntToDouble { src: TackyVal, dst: TackyVal },
    DoubleToUInt { src: TackyVal, dst: TackyVal },
    UIntToDouble { src: TackyVal, dst: TackyVal },
    Truncate { src: TackyVal, dst: TackyVal },
    Unary { op: UnaryOperator, src: TackyVal, dst: TackyVal },
    Binary { op: BinaryOperator, src1: TackyVal, src2: TackyVal, dst: TackyVal },
    Copy { src: TackyVal, dst: TackyVal },
    GetAddress { src: TackyVal, dst: TackyVal },
    Load { src_ptr: TackyVal, dst: TackyVal },
    Store { src: TackyVal, dst_ptr: TackyVal },
    AddPtr { ptr: TackyVal, index: TackyVal, scale: usize, dst: TackyVal },
    CopyToOffset { src: TackyVal, dst: String, offset: usize },
    CopyFromOffset { src: String, offset: usize, dst: TackyVal },
    Jump(String),
    JumpIfZero(TackyVal, String),
    JumpIfNotZero(TackyVal, String),
    Label(String),
    FunCall { f: String, args: Vec<TackyVal>, dst: Option<TackyVal> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Function { name: String, global: bool, params: Vec<String>, body: Vec<Instruction> },
    StaticVariable { name: String, t: Type, global: bool, init: Vec<StaticInit> },
    StaticConstant { name: String, t: Type, init: StaticInit },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Vec<TopLevel>);
