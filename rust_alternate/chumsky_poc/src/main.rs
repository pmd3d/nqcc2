use std::env;
use std::fs::{File, write};
use std::hash::Hash;
use std::io::{self, Read};
use chumsky::prelude::*;

// goals
// 1. get arguments to read in a file as a string. done
// 2. lexer
// 3. parser
// 4. assembly generation
// 5. code emission
// 6. gcc -E -P input_file -o preprocessed_file
// 6. running: cargo run preprocessed_file assembly_file
// 7. gcc assembly_file -o output_file
fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        println!("Usage: {} <input filename> <output filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];

    let mut file = File::open(filename)?;

    let mut src = String::new();
    file.read_to_string(&mut src)?;

    // 1) Lex
    let tokens = match lexer().parse(src) {
        Ok(toks) => toks,
        Err(errs) => {
            eprintln!("Lex errors:");
            for e in errs {
                eprintln!("  {e:?}");
            }
            std::process::exit(1);
        }
    };

    // 2) Parse
    let ast = match parser::parse_program(&mut TokenStream::new(tokens)) {
        Ok(ast) => ast,
        Err(errs) => {
            eprintln!("Parse errors:");
            eprintln!("  {:?}", errs);
            std::process::exit(1);
        }
    };

    let mut counter = 0;

    println!("{:?}", ast.clone());

    let tacky = tacky::emit_tacky(ast, &mut || {
        counter += 1;
        counter.to_string()
    });

    let code = crate::tacky::emit_code(tacky);

    let output = crate::tacky::generate_asm(code);

    write(args[2].clone(), output)?;

    Ok(())
}

mod tacky {
    use crate::parser::*;
    use crate::code::*;
    use std::collections::HashMap;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]    
    pub enum Unaryop {
        Complement,
        Negate
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]    
    pub enum Binaryop {
        Add,
        Subtract,
        Multiply,
        Divide,
        Remainder
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Val {
        Constant(i64),
        Var(String)
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct UnaryInstruction {
        pub op : Unaryop,
        pub src : Val,
        pub dst : Val,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct BinaryInstruction {
        pub op : Binaryop,
        pub src1 : Val,
        pub src2 : Val,
        pub dst : Val,
    }


    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Instruction {
        Return(Val),
        Unary(UnaryInstruction),
        Binary(BinaryInstruction),
    }

    pub struct FunctionDefinition {
        pub identifier : String,
        pub body : Vec<Instruction>,
    }

    pub enum Program {
        Program(FunctionDefinition),
    }

    fn emit_tacky_expr(e : Box<Expr>, instructions: &mut Vec<Instruction>, make_temporary : &mut impl FnMut() -> String) -> Val {
        match *e {
            Expr::Factor(factor) => {
                match *factor {
                    Factor::Int(num) => Val::Constant(num),
                    Factor::UnopExpr(UnopExpr { unop, expr : factor }) => {
                        let dst_name = make_temporary();
                        let src = emit_tacky_expr(Box::new(Expr::Factor(factor)), instructions, make_temporary);
                        let dst = Val::Var(dst_name.to_string());
                        let op = match unop { 
                            Unop::Complement => Unaryop::Complement,
                            Unop::Negative => Unaryop::Negate,
                        };
                        instructions.push(Instruction::Unary(UnaryInstruction { op, src, dst: dst.clone() }));
                        dst
                    },
                    _ => {
                        // todo
                        Val::Constant(0)
                    }
                }
            }
            Expr::Binary(BinaryExpr { op, left, right}) => {
                let v1 = emit_tacky_expr(left, instructions, make_temporary);
                let v2 = emit_tacky_expr(right, instructions, make_temporary);
                let dst_name = make_temporary();
                let dst = Val::Var(dst_name);
                let tacky_op = match op {
                    Binop::Add => Binaryop::Add,
                    Binop::Subtract => Binaryop::Subtract,
                    Binop::Multiply => Binaryop::Multiply,
                    Binop::Divide => Binaryop::Divide,
                    Binop::Modulo => Binaryop::Remainder,
                };
                instructions.push(Instruction::Binary(
                    BinaryInstruction { op: tacky_op, src1: v1, src2: v2, dst: dst.clone()}
                ));
                dst
            }
        }
    }

    pub fn emit_tacky(program: crate::parser::Program, make_temporary : &mut impl FnMut() -> String) -> crate::tacky::Program {
        let crate::parser::Program { funcs } = program;
        // todo: remove unwrap
        let Function { ret_type, name, params, body } = funcs.first().unwrap();
        let mut instructions : Vec<Instruction> = vec![];

        for statement in body {
            let Stmt::Return(expr) = statement;
            // todo: extra clone?
            let final_instruction = Instruction::Return(emit_tacky_expr(Box::new(expr.clone()), &mut instructions, make_temporary));
            instructions.push(final_instruction);
        };

        // todo: extra clone?
        crate::tacky::Program::Program(
            FunctionDefinition {
                identifier : name.clone(),
                body: instructions
            }
        )
    }

    pub fn emit_code(tacky : crate::tacky::Program) -> Code {
        let crate::tacky::Program::Program(FunctionDefinition { identifier , body }) = tacky;
        let mut assembly_instructions : Vec<InstructionCode> = vec![];

        fn map_val_to_operand(v : Val) -> Operand {
            match v {
                Val::Constant(num) => Operand::Imm(num),
                Val::Var(s) => Operand::Pseudo(s)
            }        
        }

        // 1st pass, use pseudo registers
        for instruction in body {
            match instruction {
                Instruction::Unary(UnaryInstruction {op, src, dst}) => {
                    let dest = map_val_to_operand(dst);

                    assembly_instructions.push(InstructionCode::Mov(Mov {
                        src: map_val_to_operand(src),
                        dest : dest.clone()
                    }));
                    assembly_instructions.push(InstructionCode::Unary((
                        match op {
                            Unaryop::Complement => UnaryOperator::Not,
                            Unaryop::Negate => UnaryOperator::Neg,
                        },
                        dest
                    )));
                }, 
                Instruction::Return(val) => {
                    assembly_instructions.push(InstructionCode::Mov(Mov { 
                            src: map_val_to_operand(val),
                            dest: Operand::Register(Reg::AX)}));
                    assembly_instructions.push(InstructionCode::Ret);
                },
                Instruction::Binary(BinaryInstruction { op, src1, src2, dst }) => {
                    match op {
                        Binaryop::Add | Binaryop::Multiply | Binaryop::Subtract => {
                            assembly_instructions.push(InstructionCode::Mov(Mov {
                                src: map_val_to_operand(src1),
                                dest: map_val_to_operand(dst.clone())
                            }));
                            assembly_instructions.push(InstructionCode::Binary((
                                match op {
                                    Binaryop::Add => BinaryOperator::Add,
                                    Binaryop::Multiply => BinaryOperator::Mult,
                                    Binaryop::Subtract => BinaryOperator::Sub,
                                    _ => panic!("invalid operator in emit code")
                                },
                                map_val_to_operand(src2),
                                map_val_to_operand(dst)
                            )));
                        },
                        Binaryop::Divide => {
                            assembly_instructions.push(InstructionCode::Mov(Mov {
                                src: map_val_to_operand(src1),
                                dest: Operand::Register(Reg::AX)
                            }));
                            assembly_instructions.push(InstructionCode::Cdq);
                            assembly_instructions.push(InstructionCode::Idiv(
                                map_val_to_operand(src2)
                            ));
                            assembly_instructions.push(InstructionCode::Mov(Mov {
                                src: Operand::Register(Reg::AX),
                                dest: map_val_to_operand(dst)
                            }))
                        },
                        Binaryop::Remainder => {
                            assembly_instructions.push(InstructionCode::Mov(Mov {
                                src: map_val_to_operand(src1),
                                dest: Operand::Register(Reg::AX)
                            }));
                            assembly_instructions.push(InstructionCode::Cdq);
                            assembly_instructions.push(InstructionCode::Idiv(
                                map_val_to_operand(src2)
                            ));
                            assembly_instructions.push(InstructionCode::Mov(Mov {
                                src: Operand::Register(Reg::DX),
                                dest: map_val_to_operand(dst)
                            }))
                        }
                    }
                }
            }
        }

        let mut stack_pos: i64 = 0;
        let mut pseudo_history : HashMap<String, i64> = HashMap::new();

        // 2nd pass, fix pseudo registers and determine stack space...
        for instruction in &mut assembly_instructions {
            match instruction {
                InstructionCode::Mov(Mov{src: Operand::Pseudo(s1), dest: Operand::Pseudo(s2)}) => {
                    let stack1  = *pseudo_history.entry(s1.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                    let stack2 = *pseudo_history.entry(s2.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});

                    *instruction = InstructionCode::Mov(Mov {
                        src: Operand::Stack(stack1),
                        dest: Operand::Stack(stack2)
                    })
                },
                InstructionCode::Mov(Mov{src: Operand::Pseudo(s), dest}) => {
                    let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                    *instruction = InstructionCode::Mov(Mov {
                        src: Operand::Stack(*stack),
                        dest: dest.clone()
                    })
                },
                InstructionCode::Mov(Mov{src, dest: Operand::Pseudo(s)}) => {
                    let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                    *instruction = InstructionCode::Mov(Mov {
                        src: src.clone(),
                        dest: Operand::Stack(*stack)
                    })
                },
                InstructionCode::Unary((op, Operand::Pseudo(s))) => {
                    let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});                
                    *instruction = InstructionCode::Unary((
                        op.clone(),
                        Operand::Stack(*stack)
                    ));
                },
                InstructionCode::Idiv(Operand::Pseudo(s)) => {
                    let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});                
                    *instruction = InstructionCode::Idiv(
                        Operand::Stack(*stack)
                    );
                },
                InstructionCode::Binary((op, Operand::Pseudo(s1), Operand::Pseudo(s2))) => {
                    let stack1  = *pseudo_history.entry(s1.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                    let stack2 = *pseudo_history.entry(s2.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});

                    *instruction = InstructionCode::Binary((
                        op.clone(),
                        Operand::Stack(stack1),
                        Operand::Stack(stack2)
                    ));
                },
                InstructionCode::Binary((op, operand, Operand::Pseudo(s1))) => {
                    let stack1  = *pseudo_history.entry(s1.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});

                    *instruction = InstructionCode::Binary((
                        op.clone(),
                        operand.clone(),
                        Operand::Stack(stack1)
                    ));
                },
                InstructionCode::Binary((op, Operand::Pseudo(s1), operand)) => {
                    let stack1  = *pseudo_history.entry(s1.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});

                    *instruction = InstructionCode::Binary((
                        op.clone(),
                        Operand::Stack(stack1),
                        operand.clone()
                    ));
                }
                _ => continue,
            }
        }
        
        // 3rd pass, final. fix up double stack operands and add stack allocation now that know how much stack we need...
        assembly_instructions = assembly_instructions
            .into_iter()
            .flat_map(|instruction| {
                match instruction {
                    InstructionCode::Mov(Mov{
                        src: Operand::Stack(s1), dest: Operand::Stack(s2)}) => {
                            vec![
                                InstructionCode::Mov(Mov{ src: Operand::Stack(s1), dest: Operand::Register(Reg::R10)}),
                                InstructionCode::Mov(Mov{ src: Operand::Register(Reg::R10), dest: Operand::Stack(s2)}),
                            ]
                        },
                    _ => vec![instruction]
                }
            })
            .collect();
        assembly_instructions.insert(0, InstructionCode::AllocateStack(stack_pos.abs()));

        Code::ProgramCode(FunctionCode {
            name : identifier,
            instructions : assembly_instructions
        })
    }

    pub fn generate_asm(code : Code) -> String {
        const INDENT : usize = 2;
        let mut result = String::new();
        let Code::ProgramCode(FunctionCode{ name: identifier, instructions}) = code;

        emit_string(&mut result, 0, ".intel_syntax noprefix".to_string());
        emit_string(&mut result, 0, format!(".global {}", identifier));
        emit_string(&mut result, 0, format!("{}:", identifier));
        emit_string(&mut result, INDENT, "pushq rbp".to_string());
        emit_string(&mut result, INDENT, "movq rbp, rsp".to_string());

        for instruction in instructions {
            match instruction {
                InstructionCode::AllocateStack(i) => emit_string(&mut result, INDENT, format!("subq rsp, {}", i)),
                InstructionCode::Ret => {
                    emit_string(&mut result, INDENT, "movq rsp, rbp".to_string());
                    emit_string(&mut result, INDENT, "popq rbp".to_string());
                    emit_string(&mut result, INDENT, "ret".to_string());
                },
                InstructionCode::Mov(Mov{src, dest}) =>
                    emit_string(&mut result, INDENT, format!("mov {}, {}", emit_operand(dest), emit_operand(src))),
                InstructionCode::Unary((op, operand)) => 
                    emit_string(&mut result, INDENT, format!("{} {}", 
                        match op {
                            UnaryOperator::Neg => "neg",
                            UnaryOperator::Not => "not"
                        }, emit_operand(operand)))
            }
        }

        emit_string(&mut result, 0, ".section .note.GNU-stack,\"\",@progbits".to_string());

        result
    }

    fn emit_operand(o : Operand) -> String {
        match o {
            Operand::Imm(i) => format!("{}", i),
            Operand::Register(Reg::AX) => "eax".to_string(),
            Operand::Register(Reg::R10) => "r10d".to_string(),
            Operand::Stack(pos) => format!("DWORD PTR [rbp{}]", pos),
            _ => panic!("leftover pseudo register not fixed")
        }
    }

    fn emit_string(buffer : &mut String, indent: usize, line : String) {
        (*buffer).push_str(&format!("{:>indent$}{line}\n", ""));
    }
}

mod code {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Reg {
        AX,
        DX,
        R10,
        R11,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Operand {
        Imm(i64),
        Register(Reg),
        Pseudo(String),
        Stack(i64),
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum UnaryOperator {
        Neg,
        Not
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum BinaryOperator {
        Add,
        Sub,
        Mult
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Mov {
        pub src : Operand,
        pub dest : Operand,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum InstructionCode {
        Mov(Mov),
        Unary((UnaryOperator, Operand)),
        AllocateStack(i64),
        Ret,
        Binary((BinaryOperator, Operand, Operand)),
        Idiv(Operand),
        Cdq,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct FunctionCode {
        pub name : String,
        pub instructions : Vec<InstructionCode>,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Code {
        ProgramCode(FunctionCode)
    }
}

// ====== Lexer ======

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Token {
    KwInt,
    KwVoid,
    KwReturn,
    Ident(String),
    Num(i64),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Tilde,
    Minus,
    Slash,
    Star,
    Plus,
    DoubleMinus,
    Percent,
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    // ident -> keyword classification is done here (no then_not/char needed)
    let ident_or_kw = text::ident().map(|s: String| match s.as_str() {
        "int" => Token::KwInt,
        "void" => Token::KwVoid,
        "return" => Token::KwReturn,
        _ => Token::Ident(s),
    });

    let num = text::int(10)
        .from_str::<i64>()
        .unwrapped()
        .map(Token::Num);

    let sym = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just(';').to(Token::Semicolon),
        just('~',).to(Token::Tilde),
        just('-').to(Token::Minus),
        just('*').to(Token::Star),
        just('/').to(Token::Slash),
        just('+').to(Token::Plus),
        just('%').to(Token::Percent),
    ));

    let double_sym = just("--").to(Token::DoubleMinus);

    let token = choice((ident_or_kw, num, double_sym, sym))
        .padded_by(text::whitespace());

    token.repeated().collect()
}

// ====== Copied TokenStream for Parser convenience, probably should use iterators instead ======
use std::collections::VecDeque;

pub struct TokenStream {
    queue: VecDeque<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { queue: VecDeque::from(tokens) }
    }

    pub fn take_token(&mut self) -> Option<Token> {
        self.queue.pop_front()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.queue.front()
    }

    pub fn npeek(&self, n: usize) -> Vec<Token> {
        self.queue.iter().take(n).cloned().collect()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}

// ====== Parser converts tokens into an AST ======

mod parser {
    use std::fmt::Binary;

    use crate::Token;
    use crate::Token::*;
    use crate::TokenStream;

    // ====== AST ======
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Type {
        Int,
        Void,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Unop {
        Complement,
        Negative
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Binop {
        Subtract,
        Add,
        Multiply,
        Divide,
        Modulo,
    }
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct UnopExpr {
        pub unop : Unop,
        pub expr : Box<Factor>,
    }    

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct BinaryExpr {
        pub op : Binop,
        pub left : Box<Expr>,
        pub right: Box<Expr>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Factor {
        Int(i64),
        UnopExpr(UnopExpr),
        Expr(Expr) // paren expr
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Expr {
        Factor(Box<Factor>),
        Binary(BinaryExpr)
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Stmt {
        Return(Expr),
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Function {
        pub ret_type: Type,
        pub name: String,
        pub params: Vec<(Type, String)>,
        pub body: Vec<Stmt>,
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Program {
        pub funcs: Vec<Function>,
    }

    // Expressions
    fn get_precedence(tok: &Token) -> Option<i32> {
        match tok {
            Star | Slash => Some(50),
            Plus | Minus => Some(45),
            _ => None,
        }
    }

    fn parse_unop(ts: &mut TokenStream) -> Result<Unop, String> {
        match ts.take_token() {
            Some(Token::Tilde) => Ok(Unop::Complement),
            Some(Token::Minus) => Ok(Unop::Negative),
            other => Err(format!("expected unary operator got {:?}", other)),
        }
    }

    fn parse_binop(ts: &mut TokenStream) -> Result<Binop, String> {
        match ts.take_token() {
            Some(Token::Plus) => Ok(Binop::Add),
            Some(Token::Star) => Ok(Binop::Multiply),
            Some(Token::Minus) => Ok(Binop::Subtract),
            Some(Token::Slash) => Ok(Binop::Divide),
            Some(Token::Percent) => Ok(Binop::Modulo),
            other => Err(format!("expected binary operator got {:?}", other)),
        }
    }

    fn parse_id(ts: &mut TokenStream) -> Result<String, String> {
        match ts.take_token() {
            Some(Ident(id)) => Ok(id),
            other => Err(format!("expected identifier got {:?}", other)),
        }
    }

    fn parse_argument_list(ts: &mut TokenStream) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        args.push(parse_exp(0, ts)?);
        Ok(args)
    }

    fn parse_primary_exp(ts: &mut TokenStream) -> Result<Factor, String> {
        let token = ts.peek().ok_or("EOF")?.clone();

        match token {
            Token::Num(num) => {
                ts.take_token();
                Ok(Factor::Int(num))
            },
            Token::LParen => {
                ts.take_token();
                let e = parse_exp(0, ts)?;
                match ts.peek() {
                    Some(Token::RParen) => {
                        ts.take_token();
                        Ok(Factor::Expr(e))
                    },
                    Some(t) => Err(format!("expected closing paren, got {:?}", t)),
                    None => Err("EOF".to_string())
                }
            },
            Token::Tilde | Token::Minus => {
                let operator = parse_unop(ts)?;
                let inner_exp = parse_primary_exp(ts)?;
                Ok(Factor::UnopExpr(UnopExpr { unop: operator, expr: Box::new(inner_exp) }))
            }
            _ => Err(format!("expected primary expression got {:?}", token)),
        }
    }

    fn parse_exp(min_prec: i32, ts: &mut TokenStream) -> Result<Expr, String> {
        let factor = parse_primary_exp(ts)?;
        let mut left : Expr = Expr::Factor(Box::new(factor));
        loop {
            let next = ts.peek().cloned();
            let Some(tok) = next else { break };
            let Some(prec) = get_precedence(&tok) else { break };
            if prec < min_prec { break; }
            let op = parse_binop(ts)?;
            let right: Expr = parse_exp(prec + 1, ts)?;
            let right = Box::new(right);
            left = Expr::Binary(BinaryExpr{ op, left: Box::new(left), right});
        }
        Ok(left)
    }

    fn parse_param_list(ts: &mut TokenStream) -> Result<Vec<(Type, String)>, String> {
        if ts.npeek(3) == vec![LParen, KwVoid, RParen] {
            ts.take_token();
            ts.take_token();
            ts.take_token();
            Ok(Vec::new())
        } else {
            Err("unexpected param list".to_string())
        }
    }

    fn parse_type_specifier(ts: &mut TokenStream) -> Result<Type, String> {
        let spec = ts.take_token();
        match spec {
            Some(KwInt) => Ok(Type::Int),
            Some(KwVoid) => Ok(Type::Void),
            _ => Err("invalid type specifier".to_string())
        }
    }

    fn parse_statement(ts: &mut TokenStream) -> Result<Stmt, String> {
        match ts.peek() {
            Some(KwReturn) => {
                ts.take_token();
                let exp = parse_exp(0, ts)?;
                let semicolon = ts.take_token();
                if !matches!(semicolon, Some(Token::Semicolon)) {
                    Err("Expected semicolon to end statement".to_string())
                }
                else {
                    Ok(Stmt::Return(exp))
                }
            },
            _ => Err("Unexpected statement".to_string())
        }
    }

    fn parse_function(ts: &mut TokenStream) -> Result<Function, String> {
        let ret_type : Type = parse_type_specifier(ts)?;
        let name = parse_id(ts)?;
        let params = parse_param_list(ts)?;
        if !matches!(ts.take_token(), Some(Token::LBrace)) {
            return Err("missing opening brace for function body".to_string());
        }
        let body = parse_statement(ts)?;
        if let Some(end_token) = ts.take_token() && !matches!(end_token, Token::RBrace) {
            return Err(format!("missing closing brace for function body, got {:?}", end_token));
        } 
        Ok(Function { name, ret_type, params, body: vec![body] })
    }

    pub fn parse_program(ts: &mut TokenStream) -> Result<Program, String> {
        let mut funcs = Vec::new();
        while !ts.is_empty() {
            funcs.push(parse_function(ts)?);
        }
        Ok(Program{funcs})
    }
}
