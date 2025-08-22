use chumsky::prelude::*;
use std::fmt;

// ====== AST ======

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Int,
    Void,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Int(i64),
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

    let ret_stmt = just(Token::KwReturn)
        .ignore_then(num)
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

// ====== Demo ======

fn main() {
    let src = r#"
int main(void) {
  return 2;
}
"#;

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
    match parser().parse(tokens) {
        Ok(ast) => {
            println!("{ast:#?}");   // full Debug AST
        }
        Err(errs) => {
            eprintln!("Parse errors:");
            for e in errs {
                eprintln!("  {e:?}");
            }
            std::process::exit(1);
        }
    }
}
