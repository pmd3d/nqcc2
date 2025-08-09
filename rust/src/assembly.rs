use crate::initializers::StaticInit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Reg {
    AX,
    BX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    SP,
    BP,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operand {
    Imm(i64),
    Reg(Reg),
    Pseudo(String),
    Memory(Reg, i32),
    Data(String, i32),
    PseudoMem(String, i32),
    Indexed { base: Reg, index: Reg, scale: i32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Neg,
    Not,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    DivDouble,
    And,
    Or,
    Xor,
    Shl,
    ShrBinop,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsmType {
    Byte,
    Longword,
    Quadword,
    Double,
    ByteArray { size: i32, alignment: i32 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Mov(AsmType, Operand, Operand),
    Movsx { src_type: AsmType, dst_type: AsmType, src: Operand, dst: Operand },
    MovZeroExtend { src_type: AsmType, dst_type: AsmType, src: Operand, dst: Operand },
    Lea(Operand, Operand),
    Cvttsd2si(AsmType, Operand, Operand),
    Cvtsi2sd(AsmType, Operand, Operand),
    Unary(UnaryOperator, AsmType, Operand),
    Binary { op: BinaryOperator, t: AsmType, src: Operand, dst: Operand },
    Cmp(AsmType, Operand, Operand),
    Idiv(AsmType, Operand),
    Div(AsmType, Operand),
    Cdq(AsmType),
    Jmp(String),
    JmpCC(CondCode, String),
    SetCC(CondCode, Operand),
    Label(String),
    Push(Operand),
    Pop(Reg),
    Call(String),
    Ret,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Function { name: String, global: bool, instructions: Vec<Instruction> },
    StaticVariable { name: String, alignment: i32, global: bool, init: Vec<StaticInit> },
    StaticConstant { name: String, alignment: i32, init: StaticInit },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Vec<TopLevel>);

