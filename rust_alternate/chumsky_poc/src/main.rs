use std::collections::HashMap;
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
    let ast = match parser().parse(tokens) {
        Ok(ast) => ast,
        Err(errs) => {
            eprintln!("Parse errors:");
            for e in errs {
                eprintln!("  {e:?}");
            }
            std::process::exit(1);
        }
    };

    let mut counter = 0;

    let tacky = emit_tacky(ast, &mut || {
        counter += 1;
        counter.to_string()
    });

    let code = emit_code(tacky);

    let output = generate_asm(code);

    write(args[2].clone(), output)?;

    Ok(())
}

mod tacky {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]    
    pub enum Unaryop {
        Complement,
        Negate
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
    pub enum Instruction {
        Return(Val),
        Unary(UnaryInstruction)
    }

    pub struct FunctionDefinition {
        pub identifier : String,
        pub body : Vec<Instruction>,
    }

    pub enum Program {
        Program(FunctionDefinition),
    }
}

mod code {
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Reg {
        AX,
        R10,
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
    pub struct Mov {
        pub src : Operand,
        pub dest : Operand,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum InstructionCode {
        Mov(Mov),
        Unary((UnaryOperator, Operand)),
        AllocateStack(i64),
        Ret
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

// ====== AST ======

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Int,
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Unop {
    Complement,
    Negative
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct UnopExpr {
    unop : Unop,
    expr : Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Int(i64),
    UnopExpr(UnopExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Stmt {
    Return(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Function {
    ret_type: Type,
    name: String,
    params: Vec<(Type, String)>,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Program {
    func: Function,
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
        just('-').to(Token::Minus)
    ));

    let token = choice((ident_or_kw, num, sym))
        .padded_by(text::whitespace());

    token.repeated().collect()
}

// ====== Parser over tokens ======

fn parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let kw_int  = select! { Token::KwInt => Type::Int };
    let kw_void = select! { Token::KwVoid => Type::Void };
    let ty = choice((kw_int, kw_void)); // ty short for type

    let ident = select! { Token::Ident(s) => s };

    let lparen = just(Token::LParen);
    let rparen = just(Token::RParen);
    let lbrace = just(Token::LBrace);
    let rbrace = just(Token::RBrace);
    let semi   = just(Token::Semicolon);

    let num = select! { Token::Num(n) => n }.map(Expr::Int);

    let complement = select! { Token::Tilde => Unop::Complement };
    let negate = select! { Token::Minus => Unop::Negative };
    let unary_operation = choice((complement, negate));

    let exp = recursive(|exp| {
        choice((num, 
            unary_operation.then(exp.clone()).map(|(op, e)| {
                Expr::UnopExpr(UnopExpr { unop : op, expr : Box::new(e) })
            }),
            lparen.clone().ignore_then(exp).then_ignore(rparen.clone())))
        });

    let ret_stmt = just(Token::KwReturn)
        .ignore_then(exp)
        .then_ignore(semi.clone())
        .map(Stmt::Return);

    let stmt = ret_stmt;

    // Only support `(void)` or `()` -> empty parameter list for this demo
    let params = lparen
        .ignore_then(
            choice((
                kw_void.map(|_| Vec::<(Type, String)>::new()),
                empty().to(Vec::<(Type, String)>::new()),
            ))
        )
        .then_ignore(rparen);

    let func = ty
        .then(ident)
        .then(params)
        .then(lbrace.ignore_then(stmt.repeated()).then_ignore(rbrace))
        .map(|(((ret_type, name), params), body)| Function { ret_type, name, params, body });

    func.map(|func| Program { func })
}


/*
fn parser_descent_program(tokens : Vec<Token>) -> Result<Program, String> {
    let return_type = parser_descent_type((tokens[0]))?;
    let func_identifier = parser_descent_identifier(tokens[1])?;
    let params = parser_descent_params(tokens[2..])?;
    parser_descent_just_ignore(tokens[0], )
}

fn parser_descent_type(token : Token) -> Result<Type, String> {
    match token {
        Token::KwInt => Ok(Type::Int),
        Token::KwVoid => Ok(Type::Void),
        _ => Err("syntax error, no program type".to_string()),
    }
}

fn parser_descent_identifier(token : Token) -> Result<String, String> {
    match token {
        Token::Ident(s) => Ok(s),
        _ => Err("syntax error, invalid identifier".to_string()),
    }
}

fn parser_descent_params(token : &mut Vec<Token>) -> Result<Vec<(Type, String)>, String> {
    parser_descent_just_ignore(&token[0], Token::LParen)?;
    token.pop();
    parser_descent_just_ignore(&token[0], Token::KwVoid)?;
    token.pop();
    parser_descent_just_ignore(&token[0], Token::RParen)?;
    token.pop();
    Ok(vec![])
}

fn parser_descent_just(current : &Token, expect_token : Token, expect_type : Type) -> Result<Type, String> {
    if *current == expect_token {
        Ok(expect_type)
    }
    else {
        Err(format!("Did not match expected token {:?}", expect_token))
    }
}

fn parser_descent_just_ignore(current : &Token, expect_token : Token) -> Result<(), String> {
    if *current == expect_token {
        Ok(())
    }
    else {
        Err(format!("Did not match expected token {:?}", expect_token))
    }
}
*/

fn emit_tacky_expr(e : Box<Expr>, instructions: &mut Vec<tacky::Instruction>, make_temporary : &mut impl FnMut() -> String) -> tacky::Val {
    match *e {
        Expr::Int(num) => tacky::Val::Constant(num),
        Expr::UnopExpr(UnopExpr { unop, expr }) => {
            let dst_name = make_temporary();
            let src = emit_tacky_expr(expr, instructions, make_temporary);
            let dst = tacky::Val::Var(dst_name.to_string());
            let op = match unop { 
                Unop::Complement => tacky::Unaryop::Complement,
                Unop::Negative => tacky::Unaryop::Negate,
            };
            instructions.push(tacky::Instruction::Unary(tacky::UnaryInstruction { op, src, dst: dst.clone() }));
            dst
        }
    }
}

fn emit_tacky(program: Program, make_temporary : &mut impl FnMut() -> String) -> tacky::Program {
    let Program { func } = program;
    let Function { ret_type, name, params, body } = func;
    let mut instructions : Vec<tacky::Instruction> = vec![];

    for statement in body {
        let Stmt::Return(expr) = statement;
        let final_instruction = tacky::Instruction::Return(emit_tacky_expr(Box::new(expr), &mut instructions, make_temporary));
        instructions.push(final_instruction);
    };

    tacky::Program::Program(
        tacky::FunctionDefinition {
            identifier : name,
            body: instructions
        }
    )
}

fn emit_code(tacky : tacky::Program) -> code::Code {
    let tacky::Program::Program(tacky::FunctionDefinition { identifier , body }) = tacky;
    let mut assembly_instructions : Vec<code::InstructionCode> = vec![];

    fn map_val_to_operand(v : tacky::Val) -> code::Operand {
        match v {
            tacky::Val::Constant(num) => code::Operand::Imm(num),
            tacky::Val::Var(s) => code::Operand::Pseudo(s)
        }        
    }

    // 1st pass, use pseudo registers
    for instruction in body {
        match instruction {
            tacky::Instruction::Unary(tacky::UnaryInstruction {op, src, dst}) => {
                let dest = map_val_to_operand(dst);

                assembly_instructions.push(code::InstructionCode::Mov(code::Mov {
                    src: map_val_to_operand(src),
                    dest : dest.clone()
                }));
                assembly_instructions.push(code::InstructionCode::Unary((
                    match op {
                        tacky::Unaryop::Complement => code::UnaryOperator::Not,
                        tacky::Unaryop::Negate => code::UnaryOperator::Neg,
                    },
                    dest
                )));
            }, 
            tacky::Instruction::Return(val) => {
                assembly_instructions.push(code::InstructionCode::Mov(code::Mov { 
                        src: map_val_to_operand(val),
                        dest: code::Operand::Register(code::Reg::AX)}));
                assembly_instructions.push(code::InstructionCode::Ret);
            },
        }
    }

    let mut stack_pos: i64 = 0;
    let mut pseudo_history : HashMap<String, i64> = HashMap::new();

    // 2nd pass, fix pseudo registers and determine stack space...
    for instruction in &mut assembly_instructions {
        match instruction {
            code::InstructionCode::Mov(code::Mov{src: code::Operand::Pseudo(s1), dest: code::Operand::Pseudo(s2)}) => {
                let stack1  = *pseudo_history.entry(s1.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                let stack2 = *pseudo_history.entry(s2.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});

                *instruction = code::InstructionCode::Mov(code::Mov {
                    src: code::Operand::Stack(stack1),
                    dest: code::Operand::Stack(stack2)
                })

            },
            code::InstructionCode::Mov(code::Mov{src: code::Operand::Pseudo(s), dest}) => {
                let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                *instruction = code::InstructionCode::Mov(code::Mov {
                    src: code::Operand::Stack(*stack),
                    dest: dest.clone()
                })
            },
            code::InstructionCode::Mov(code::Mov{src, dest: code::Operand::Pseudo(s)}) => {
                let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});
                *instruction = code::InstructionCode::Mov(code::Mov {
                    src: src.clone(),
                    dest: code::Operand::Stack(*stack)
                })
            },
            code::InstructionCode::Unary((op, code::Operand::Pseudo(s))) => {
                let stack = pseudo_history.entry(s.clone()).or_insert_with(|| { stack_pos -= 4; stack_pos});                
                *instruction = code::InstructionCode::Unary((
                    op.clone(),
                    code::Operand::Stack(*stack)
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
                code::InstructionCode::Mov(code::Mov{
                    src: code::Operand::Stack(s1), dest: code::Operand::Stack(s2)}) => {
                        vec![
                            code::InstructionCode::Mov(code::Mov{ src: code::Operand::Stack(s1), dest: code::Operand::Register(code::Reg::R10)}),
                            code::InstructionCode::Mov(code::Mov{ src: code::Operand::Register(code::Reg::R10), dest: code::Operand::Stack(s2)}),
                        ]
                    },
                _ => vec![instruction]
            }
        })
        .collect();
    assembly_instructions.insert(0, code::InstructionCode::AllocateStack(stack_pos.abs()));

    code::Code::ProgramCode(code::FunctionCode {
        name : identifier,
        instructions : assembly_instructions
    })
}

fn generate_asm(code : code::Code) -> String {
    const INDENT : usize = 2;
    let mut result = String::new();
    let code::Code::ProgramCode(code::FunctionCode{ name: identifier, instructions}) = code;

    emit_string(&mut result, 0, ".intel_syntax noprefix".to_string());
    emit_string(&mut result, 0, format!(".global {}", identifier));
    emit_string(&mut result, 0, format!("{}:", identifier));
    emit_string(&mut result, INDENT, "pushq rbp".to_string());
    emit_string(&mut result, INDENT, "movq rbp, rsp".to_string());

    for instruction in instructions {
        match instruction {
            code::InstructionCode::AllocateStack(i) => emit_string(&mut result, INDENT, format!("subq rsp, {}", i)),
            code::InstructionCode::Ret => {
                emit_string(&mut result, INDENT, "movq rsp, rbp".to_string());
                emit_string(&mut result, INDENT, "popq rbp".to_string());
                emit_string(&mut result, INDENT, "ret".to_string());
            },
            code::InstructionCode::Mov(code::Mov{src, dest}) =>
                emit_string(&mut result, INDENT, format!("mov {}, {}", emit_operand(dest), emit_operand(src))),
            code::InstructionCode::Unary((op, operand)) => 
                emit_string(&mut result, INDENT, format!("{} {}", 
                    match op {
                        code::UnaryOperator::Neg => "neg",
                        code::UnaryOperator::Not => "not"
                    }, emit_operand(operand)))
        }
    }

    emit_string(&mut result, 0, ".section .note.GNU-stack,\"\",@progbits".to_string());

    result
}

fn emit_operand(o : code::Operand) -> String {
    match o {
        code::Operand::Imm(i) => format!("{}", i),
        code::Operand::Register(code::Reg::AX) => "eax".to_string(),
        code::Operand::Register(code::Reg::R10) => "r10d".to_string(),
        code::Operand::Stack(pos) => format!("DWORD PTR [rbp{}]", pos),
        _ => panic!("leftover pseudo register not fixed")
    }
}

fn emit_string(buffer : &mut String, indent: usize, line : String) {
    (*buffer).push_str(&format!("{:>indent$}{line}\n", ""));
}
