use nqcc_rust::{parse::{private, ParseError}, token_stream::TokenStream, tokens::Token, consts::Const, ast::{Exp, Statement}};

#[test]
fn signed_long_constant() {
    let mut ts = TokenStream::new(vec![Token::ConstLong(4611686018427387904)]);
    let c = private::parse_const(&mut ts).unwrap();
    assert_eq!(format!("{}", c), "4611686018427387904L");
}

#[test]
fn unsigned_int_constant() {
    let mut ts = TokenStream::new(vec![Token::ConstUInt(4294967291)]);
    let c = private::parse_const(&mut ts).unwrap();
    assert_eq!(format!("{}", c), "4294967291U");
}

#[test]
fn unsigned_long_constant() {
    let mut ts = TokenStream::new(vec![Token::ConstULong(18446744073709551611)]);
    let c = private::parse_const(&mut ts).unwrap();
    assert_eq!(format!("{}", c), "18446744073709551611UL");
}

#[test]
fn expression() {
    use Token::*;
    let mut ts = TokenStream::new(vec![ConstInt(100), Semicolon]);
    let e = private::parse_exp(40, &mut ts).unwrap();
    assert_eq!(e, Exp::Constant(Const::Int(100)));
}

#[test]
fn statement() {
    use Token::*;
    let mut ts = TokenStream::new(vec![KWReturn, ConstInt(4), Semicolon]);
    let s = private::parse_statement(&mut ts).unwrap();
    assert_eq!(s, Statement::Return(Some(Exp::Constant(Const::Int(4)))));
}

#[test]
fn error() {
    match nqcc_rust::parse::parse(vec![Token::KWInt]) {
        Err(_) => (),
        Ok(_) => panic!("expected error"),
    }
}
