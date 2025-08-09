use crate::tokens::Token;
use once_cell::sync::Lazy;
use regex::Regex;

struct TokenDef {
    regex: Regex,
    group: usize,
    converter: Box<dyn Fn(&str) -> Token + Sync + Send>,
}

fn def(re_str: &str, group: usize, converter: Box<dyn Fn(&str) -> Token + Sync + Send>) -> TokenDef {
    TokenDef {
        regex: Regex::new(&format!("^{}", re_str)).unwrap(),
        group,
        converter,
    }
}

fn literal(tok: Token) -> Box<dyn Fn(&str) -> Token + Sync + Send> {
    Box::new(move |_s| tok.clone())
}

fn convert_identifier(s: &str) -> Token {
    match s {
        "int" => Token::KWInt,
        "return" => Token::KWReturn,
        "void" => Token::KWVoid,
        "if" => Token::KWIf,
        "else" => Token::KWElse,
        "do" => Token::KWDo,
        "while" => Token::KWWhile,
        "for" => Token::KWFor,
        "break" => Token::KWBreak,
        "continue" => Token::KWContinue,
        "static" => Token::KWStatic,
        "extern" => Token::KWExtern,
        "long" => Token::KWLong,
        "unsigned" => Token::KWUnsigned,
        "signed" => Token::KWSigned,
        "double" => Token::KWDouble,
        "char" => Token::KWChar,
        "sizeof" => Token::KWSizeOf,
        "struct" => Token::KWStruct,
        other => Token::Identifier(other.to_string()),
    }
}

fn convert_int(s: &str) -> Token { Token::ConstInt(s.parse().unwrap()) }
fn convert_long(s: &str) -> Token { Token::ConstLong(s[..s.len()-1].parse().unwrap()) }
fn convert_uint(s: &str) -> Token { Token::ConstUInt(s[..s.len()-1].parse().unwrap()) }
fn convert_ulong(s: &str) -> Token { Token::ConstULong(s[..s.len()-2].parse().unwrap()) }
fn convert_double(s: &str) -> Token { Token::ConstDouble(s.parse().unwrap()) }
fn convert_char(s: &str) -> Token { Token::ConstChar(s[1..s.len()-1].to_string()) }
fn convert_string(s: &str) -> Token { Token::StringLiteral(s[1..s.len()-1].to_string()) }

static TOKEN_DEFS: Lazy<Vec<TokenDef>> = Lazy::new(|| {
    vec![
        def(r"[A-Za-z_][A-Za-z0-9_]*\b", 0, Box::new(convert_identifier)),
        def(r"([0-9]+)[^\w.]", 1, Box::new(convert_int)),
        def(r"([0-9]+[lL])[^\w.]", 1, Box::new(convert_long)),
        def(r"([0-9]+[uU])[^\w.]", 1, Box::new(convert_uint)),
        def(r"([0-9]+([lL][uU]|[uU][lL]))[^\w.]", 1, Box::new(convert_ulong)),
        def(r"(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]", 1, Box::new(convert_double)),
        def(r#"'([^'\\\n]|\\['"?\\abfnrtv])'"#, 0, Box::new(convert_char)),
        def(r#""([^"\\\n]|\\['"\\?abfnrtv])*""#, 0, Box::new(convert_string)),
        def(r"\(", 0, literal(Token::OpenParen)),
        def(r"\)", 0, literal(Token::CloseParen)),
        def(r"\{", 0, literal(Token::OpenBrace)),
        def(r"\}", 0, literal(Token::CloseBrace)),
        def(r";", 0, literal(Token::Semicolon)),
        def(r"-", 0, literal(Token::Hyphen)),
        def(r"--", 0, literal(Token::DoubleHyphen)),
        def(r"~", 0, literal(Token::Tilde)),
        def(r"\+", 0, literal(Token::Plus)),
        def(r"\*", 0, literal(Token::Star)),
        def(r"/", 0, literal(Token::Slash)),
        def(r"%", 0, literal(Token::Percent)),
        def(r"!", 0, literal(Token::Bang)),
        def(r"&&", 0, literal(Token::LogicalAnd)),
        def(r"\|\|", 0, literal(Token::LogicalOr)),
        def(r"==", 0, literal(Token::DoubleEqual)),
        def(r"!=", 0, literal(Token::NotEqual)),
        def(r"<", 0, literal(Token::LessThan)),
        def(r">", 0, literal(Token::GreaterThan)),
        def(r"<=", 0, literal(Token::LessOrEqual)),
        def(r">=", 0, literal(Token::GreaterOrEqual)),
        def(r"=", 0, literal(Token::EqualSign)),
        def(r"\?", 0, literal(Token::QuestionMark)),
        def(r":", 0, literal(Token::Colon)),
        def(r",", 0, literal(Token::Comma)),
        def(r"&", 0, literal(Token::Ampersand)),
        def(r"\[", 0, literal(Token::OpenBracket)),
        def(r"\]", 0, literal(Token::CloseBracket)),
        def(r"->", 0, literal(Token::Arrow)),
        def(r"(\.)[^\d]", 1, literal(Token::Dot)),
    ]
});

pub fn lex(mut input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    while !input.is_empty() {
        if let Some(ws) = Regex::new(r"^\s+").unwrap().find(input) {
            input = &input[ws.end()..];
            continue;
        }
        let mut matches: Vec<(&str, &TokenDef)> = Vec::new();
        for def in TOKEN_DEFS.iter() {
            if let Some(caps) = def.regex.captures(input) {
                if let Some(m) = caps.get(def.group) {
                    matches.push((m.as_str(), def));
                }
            }
        }
        if matches.is_empty() {
            return Err(input.to_string());
        }
        matches.sort_by_key(|(m, _)| m.len());
        let (matched, def) = matches.last().unwrap();
        let tok = (def.converter)(matched);
        tokens.push(tok);
        input = &input[matched.len()..];
    }
    Ok(tokens)
}
