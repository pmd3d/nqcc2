use nqcc_rust::{lex::lex, tokens::Token};

#[test]
fn leading_whitespace() {
    assert_eq!(lex("   return").unwrap(), vec![Token::KWReturn]);
}

#[test]
fn trailing_whitespace() {
    assert_eq!(lex("0;\t\n").unwrap(), vec![Token::ConstInt(0), Token::Semicolon]);
}

#[test]
fn a_full_program() {
    use Token::*;
    let expected = vec![
        KWInt,
        Identifier("main".into()),
        OpenParen,
        KWVoid,
        CloseParen,
        OpenBrace,
        KWReturn,
        ConstInt(0),
        Semicolon,
        CloseBrace,
    ];
    assert_eq!(lex("int main(void){return 0;}" ).unwrap(), expected);
}

#[test]
fn two_hyphens() {
    use Token::*;
    assert_eq!(lex("- -").unwrap(), vec![Hyphen, Hyphen]);
}

#[test]
fn double_hyphen() {
    use Token::*;
    assert_eq!(lex("a--").unwrap(), vec![Identifier("a".into()), DoubleHyphen]);
}

#[test]
fn two_tildes() {
    use Token::*;
    assert_eq!(lex("~~").unwrap(), vec![Tilde, Tilde]);
}
