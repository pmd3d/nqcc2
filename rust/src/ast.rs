use crate::consts::Const;
use crate::types::Type;

pub mod ops {
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
        And,
        Or,
        Equal,
        NotEqual,
        LessThan,
        LessOrEqual,
        GreaterThan,
        GreaterOrEqual,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

pub mod untyped {
    use super::StorageClass;
    use super::ops::*;
    use crate::consts::Const;
    use crate::types::Type;

    #[derive(Debug, Clone, PartialEq)]
    pub enum Exp {
        Constant(Const),
        Var(String),
        String(String),
        Cast {
            target_type: Type,
            e: Box<Exp>,
        },
        Unary(UnaryOperator, Box<Exp>),
        Binary(BinaryOperator, Box<Exp>, Box<Exp>),
        Assignment(Box<Exp>, Box<Exp>),
        Conditional {
            condition: Box<Exp>,
            then_result: Box<Exp>,
            else_result: Box<Exp>,
        },
        FunCall {
            f: String,
            args: Vec<Exp>,
        },
        Dereference(Box<Exp>),
        AddrOf(Box<Exp>),
        Subscript {
            ptr: Box<Exp>,
            index: Box<Exp>,
        },
        SizeOf(Box<Exp>),
        SizeOfT(Type),
        Dot {
            strct: Box<Exp>,
            member: String,
        },
        Arrow {
            strct: Box<Exp>,
            member: String,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Initializer {
        SingleInit(Exp),
        CompoundInit(Vec<Initializer>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct MemberDeclaration {
        pub member_name: String,
        pub member_type: Type,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct StructDeclaration {
        pub tag: String,
        pub members: Vec<MemberDeclaration>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct VariableDeclaration {
        pub name: String,
        pub var_type: Type,
        pub init: Option<Initializer>,
        pub storage_class: Option<StorageClass>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ForInit {
        InitDecl(VariableDeclaration),
        InitExp(Option<Exp>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Statement {
        Return(Option<Exp>),
        Expression(Exp),
        If {
            condition: Exp,
            then_clause: Box<Statement>,
            else_clause: Option<Box<Statement>>,
        },
        Compound(Block),
        Break(String),
        Continue(String),
        While {
            condition: Exp,
            body: Box<Statement>,
            id: String,
        },
        DoWhile {
            body: Box<Statement>,
            condition: Exp,
            id: String,
        },
        For {
            init: ForInit,
            condition: Option<Exp>,
            post: Option<Exp>,
            body: Box<Statement>,
            id: String,
        },
        Null,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum BlockItem {
        S(Statement),
        D(Declaration),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Block(pub Vec<BlockItem>);

    #[derive(Debug, Clone, PartialEq)]
    pub struct FunctionDeclaration {
        pub name: String,
        pub fun_type: Type,
        pub params: Vec<String>,
        pub body: Option<Block>,
        pub storage_class: Option<StorageClass>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Declaration {
        FunDecl(FunctionDeclaration),
        VarDecl(VariableDeclaration),
        StructDecl(StructDeclaration),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Program(pub Vec<Declaration>);
}

pub mod typed {
    use super::StorageClass;
    use super::ops::*;
    use crate::consts::Const;
    use crate::types::Type;

    #[derive(Debug, Clone, PartialEq)]
    pub enum InnerExp {
        Constant(Const),
        Var(String),
        String(String),
        Cast {
            target_type: Type,
            e: Box<Exp>,
        },
        Unary(UnaryOperator, Box<Exp>),
        Binary(BinaryOperator, Box<Exp>, Box<Exp>),
        Assignment(Box<Exp>, Box<Exp>),
        Conditional {
            condition: Box<Exp>,
            then_result: Box<Exp>,
            else_result: Box<Exp>,
        },
        FunCall {
            f: String,
            args: Vec<Exp>,
        },
        Dereference(Box<Exp>),
        AddrOf(Box<Exp>),
        Subscript {
            ptr: Box<Exp>,
            index: Box<Exp>,
        },
        SizeOf(Box<Exp>),
        SizeOfT(Type),
        Dot {
            strct: Box<Exp>,
            member: String,
        },
        Arrow {
            strct: Box<Exp>,
            member: String,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Exp {
        pub e: InnerExp,
        pub t: Type,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Initializer {
        SingleInit(Exp),
        CompoundInit(Type, Vec<Initializer>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct MemberDeclaration {
        pub member_name: String,
        pub member_type: Type,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct StructDeclaration {
        pub tag: String,
        pub members: Vec<MemberDeclaration>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct VariableDeclaration {
        pub name: String,
        pub var_type: Type,
        pub init: Option<Initializer>,
        pub storage_class: Option<StorageClass>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ForInit {
        InitDecl(VariableDeclaration),
        InitExp(Option<Exp>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Statement {
        Return(Option<Exp>),
        Expression(Exp),
        If {
            condition: Exp,
            then_clause: Box<Statement>,
            else_clause: Option<Box<Statement>>,
        },
        Compound(Block),
        Break(String),
        Continue(String),
        While {
            condition: Exp,
            body: Box<Statement>,
            id: String,
        },
        DoWhile {
            body: Box<Statement>,
            condition: Exp,
            id: String,
        },
        For {
            init: ForInit,
            condition: Option<Exp>,
            post: Option<Exp>,
            body: Box<Statement>,
            id: String,
        },
        Null,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum BlockItem {
        S(Statement),
        D(Declaration),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Block(pub Vec<BlockItem>);

    #[derive(Debug, Clone, PartialEq)]
    pub struct FunctionDeclaration {
        pub name: String,
        pub fun_type: Type,
        pub params: Vec<String>,
        pub body: Option<Block>,
        pub storage_class: Option<StorageClass>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Declaration {
        FunDecl(FunctionDeclaration),
        VarDecl(VariableDeclaration),
        StructDecl(StructDeclaration),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Program(pub Vec<Declaration>);
}

pub use ops::{BinaryOperator, UnaryOperator};
pub use untyped::*;
