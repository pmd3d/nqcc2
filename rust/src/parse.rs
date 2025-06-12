use crate::{ast::{Exp, Statement, Program}, consts::Const, tokens::Token, token_stream::TokenStream};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError(pub String);

type Result<T> = std::result::Result<T, ParseError>;

pub mod private {
    use super::*;

    fn expect(ts: &mut TokenStream, tok: Token) -> Result<()> {
        match ts.take_token() {
            Some(t) if t == tok => Ok(()),
            Some(actual) => Err(ParseError(format!("Expected {:?} got {:?}", tok, actual))),
            None => Err(ParseError("Unexpected EOF".into())),
        }
    }

    pub fn parse_const(ts: &mut TokenStream) -> Result<Const> {
        match ts.take_token() {
            Some(Token::ConstInt(v)) => {
                if v <= i32::MAX as i128 && v >= i32::MIN as i128 {
                    Ok(Const::Int(v as i32))
                } else {
                    Ok(Const::Long(v as i64))
                }
            }
            Some(Token::ConstLong(v)) => Ok(Const::Long(v as i64)),
            Some(Token::ConstUInt(v)) => {
                if v <= u32::MAX as u128 {
                    Ok(Const::UInt(v as u32))
                } else {
                    Ok(Const::ULong(v as u64))
                }
            }
            Some(Token::ConstULong(v)) => Ok(Const::ULong(v as u64)),
            Some(Token::ConstDouble(d)) => Ok(Const::Double(d)),
            Some(Token::ConstChar(c)) => {
                if let Some(ch) = c.chars().next() {
                    Ok(Const::Int(ch as i32))
                } else {
                    Err(ParseError("empty char".into()))
                }
            }
            other => Err(ParseError(format!("expected const got {:?}", other))),
        }
    }

    pub fn parse_exp(_prec: u32, ts: &mut TokenStream) -> Result<Exp> {
        let cnst = parse_const(ts)?;
        Ok(Exp::Constant(cnst))
    }

    pub fn parse_statement(ts: &mut TokenStream) -> Result<Statement> {
        match ts.peek() {
            Some(Token::KWReturn) => {
                ts.take_token();
                let exp = if let Some(Token::Semicolon) = ts.peek() {
                    None
                } else {
                    Some(parse_exp(0, ts)?)
                };
                expect(ts, Token::Semicolon)?;
                Ok(Statement::Return(exp))
            }
            _ => Err(ParseError("unexpected".into())),
        }
    }

    pub fn parse_program(_ts: &mut TokenStream) -> Result<Program> {
        Err(ParseError("not implemented".into()))
    }
}

pub use private::parse_const;
pub use private::parse_exp;
pub use private::parse_statement;

pub fn parse(tokens: Vec<Token>) -> Result<Program> {
    let mut ts = TokenStream::new(tokens);
    private::parse_program(&mut ts)
}
