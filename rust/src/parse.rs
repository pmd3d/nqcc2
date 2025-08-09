use crate::{
    ast::*,
    consts::Const,
    token_stream::TokenStream,
    tokens::Token,
    types::Type,
};

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

    fn unescape(s: &str) -> String {
        let escapes: &[(&str, char)] = &[
            ("\\'", '\''),
            ("\\\"", '"'),
            ("\\?", '?'),
            ("\\\\", '\\'),
            ("\\a", '\u{7}'),
            ("\\b", '\u{8}'),
            ("\\f", '\u{c}'),
            ("\\n", '\n'),
            ("\\r", '\r'),
            ("\\t", '\t'),
            ("\\v", '\u{b}'),
        ];
        let mut result = String::new();
        let mut i = 0;
        let bytes = s.as_bytes();
        while i < bytes.len() {
            let mut matched = false;
            for (esc, ch) in escapes {
                if s[i..].starts_with(esc) {
                    result.push(*ch);
                    i += esc.len();
                    matched = true;
                    break;
                }
            }
            if !matched {
                result.push(bytes[i] as char);
                i += 1;
            }
        }
        result
    }

    // specifiers
    fn is_type_specifier(t: &Token) -> bool {
        matches!(
            t,
            Token::KWInt
                | Token::KWLong
                | Token::KWUnsigned
                | Token::KWSigned
                | Token::KWDouble
                | Token::KWChar
                | Token::KWVoid
                | Token::KWStruct
                | Token::Identifier(_)
        )
    }

    fn parse_type_specifier(ts: &mut TokenStream) -> Result<Token> {
        match ts.take_token() {
            Some(Token::KWStruct) => {
                match ts.take_token() {
                    Some(Token::Identifier(tag)) => Ok(Token::Identifier(tag)),
                    other => Err(ParseError(format!(
                        "Expected a structure tag got {:?}",
                        other
                    ))),
                }
            }
            Some(t) if is_type_specifier(&t) => Ok(t),
            other => Err(ParseError(format!("Expected a type specifier got {:?}", other))),
        }
    }

    fn parse_type_specifier_list(ts: &mut TokenStream) -> Result<Vec<Token>> {
        let mut specs = Vec::new();
        specs.push(parse_type_specifier(ts)?);
        while let Some(t) = ts.peek().cloned() {
            if is_type_specifier(&t) {
                specs.push(parse_type_specifier(ts)?);
            } else {
                break;
            }
        }
        Ok(specs)
    }

    fn is_specifier(t: &Token) -> bool {
        matches!(t, Token::KWStatic | Token::KWExtern) || is_type_specifier(t)
    }

    fn parse_specifier(ts: &mut TokenStream) -> Result<Token> {
        let spec = ts.peek().cloned().ok_or_else(|| ParseError("EOF".into()))?;
        if matches!(spec, Token::KWStruct) {
            parse_type_specifier(ts)
        } else if is_specifier(&spec) {
            ts.take_token();
            Ok(spec)
        } else {
            Err(ParseError(format!(
                "Expected a type or storage-class specifier got {:?}",
                spec
            )))
        }
    }

    fn parse_specifier_list(ts: &mut TokenStream) -> Result<Vec<Token>> {
        let mut specs = Vec::new();
        specs.push(parse_specifier(ts)?);
        while let Some(t) = ts.peek().cloned() {
            if is_specifier(&t) {
                specs.push(parse_specifier(ts)?);
            } else {
                break;
            }
        }
        Ok(specs)
    }

    fn parse_storage_class(tok: &Token) -> Result<StorageClass> {
        match tok {
            Token::KWExtern => Ok(StorageClass::Extern),
            Token::KWStatic => Ok(StorageClass::Static),
            _ => Err(ParseError(format!(
                "Expected a storage class specifier got {:?}",
                tok
            ))),
        }
    }

    fn parse_type(specifiers: &[Token]) -> Result<Type> {
        let mut specs = specifiers.to_vec();
        specs.sort_by(|a, b| format!("{:?}", a).cmp(&format!("{:?}", b)));
        let mut ident: Option<String> = None;
        for t in &specs {
            if let Token::Identifier(id) = t {
                ident = Some(id.clone());
            }
        }
        if let Some(tag) = ident {
            if specs.len() == 1 {
                return Ok(Type::Structure(tag));
            }
        }
        use Token::*;
        match specs.as_slice() {
            [KWVoid] => Ok(Type::Void),
            [KWDouble] => Ok(Type::Double),
            [KWChar] => Ok(Type::Char),
            [KWChar, KWSigned] => Ok(Type::SChar),
            [KWChar, KWUnsigned] => Ok(Type::UChar),
            [] => Err(ParseError("Invalid type specifier".into())),
            _ => {
                let has_unsigned = specs.contains(&KWUnsigned);
                let has_long = specs.contains(&KWLong);
                let has_signed = specs.contains(&KWSigned);
                if specs.iter().any(|t| matches!(t, KWDouble | KWChar | KWVoid)) {
                    return Err(ParseError("Invalid type specifier".into()));
                }
                if specs.iter().any(|t| matches!(t, Identifier(_))) {
                    return Err(ParseError("Invalid type specifier".into()));
                }
                if has_unsigned && has_signed {
                    return Err(ParseError("Invalid type specifier".into()));
                }
                if has_unsigned && has_long {
                    Ok(Type::ULong)
                } else if has_unsigned {
                    Ok(Type::UInt)
                } else if has_long {
                    Ok(Type::Long)
                } else {
                    Ok(Type::Int)
                }
            }
        }
    }

    fn parse_type_and_storage_class(specs: &[Token]) -> Result<(Type, Option<StorageClass>)> {
        let (types, storage): (Vec<_>, Vec<_>) = specs.iter().cloned().partition(is_type_specifier);
        let typ = parse_type(&types)?;
        let storage_class = match storage.as_slice() {
            [] => None,
            [sc] => Some(parse_storage_class(sc)?),
            _ => None,
        };
        Ok((typ, storage_class))
    }

    pub fn parse_id(ts: &mut TokenStream) -> Result<String> {
        match ts.take_token() {
            Some(Token::Identifier(id)) => Ok(id),
            other => Err(ParseError(format!("expected identifier got {:?}", other))),
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
                let un = unescape(&c);
                if let Some(ch) = un.chars().next() {
                    Ok(Const::Int(ch as i32))
                } else {
                    Err(ParseError("empty char".into()))
                }
            }
            other => Err(ParseError(format!("expected const got {:?}", other))),
        }
    }

    fn parse_dim(ts: &mut TokenStream) -> Result<usize> {
        expect(ts, Token::OpenBracket)?;
        let dim = match parse_const(ts)? {
            Const::Double(_) => return Err(ParseError("Floating-point array dimensions not allowed".into())),
            Const::Char(c) => c as usize,
            Const::UChar(c) => c as usize,
            Const::Int(i) => i as usize,
            Const::Long(l) => l as usize,
            Const::UInt(u) => u as usize,
            Const::ULong(u) => u as usize,
        };
        expect(ts, Token::CloseBracket)?;
        Ok(dim)
    }

    fn parse_string(ts: &mut TokenStream) -> Result<String> {
        match ts.take_token() {
            Some(Token::StringLiteral(s)) => Ok(unescape(&s)),
            other => Err(ParseError(format!("expected string literal got {:?}", other))),
        }
    }

    // Abstract declarators
    #[derive(Clone)]
    enum AbstractDeclarator {
        Pointer(Box<AbstractDeclarator>),
        Array(Box<AbstractDeclarator>, usize),
        Base,
    }

    fn parse_abstract_array_decl_suffix(base: AbstractDeclarator, ts: &mut TokenStream) -> Result<AbstractDeclarator> {
        let dim = parse_dim(ts)?;
        let new_decl = AbstractDeclarator::Array(Box::new(base), dim);
        if ts.peek() == Some(&Token::OpenBracket) {
            parse_abstract_array_decl_suffix(new_decl, ts)
        } else {
            Ok(new_decl)
        }
    }

    fn parse_abstract_declarator(ts: &mut TokenStream) -> Result<AbstractDeclarator> {
        match ts.peek() {
            Some(Token::Star) => {
                ts.take_token();
                let inner = match ts.peek() {
                    Some(Token::Star) | Some(Token::OpenParen) | Some(Token::OpenBracket) => parse_abstract_declarator(ts)?,
                    _ => AbstractDeclarator::Base,
                };
                Ok(AbstractDeclarator::Pointer(Box::new(inner)))
            }
            _ => parse_direct_abstract_declarator(ts),
        }
    }

    fn parse_direct_abstract_declarator(ts: &mut TokenStream) -> Result<AbstractDeclarator> {
        if ts.peek() == Some(&Token::OpenParen) {
            ts.take_token();
            let decl = parse_abstract_declarator(ts)?;
            expect(ts, Token::CloseParen)?;
            if ts.peek() == Some(&Token::OpenBracket) {
                parse_abstract_array_decl_suffix(decl, ts)
            } else {
                Ok(decl)
            }
        } else {
            parse_abstract_array_decl_suffix(AbstractDeclarator::Base, ts)
        }
    }

    fn process_abstract_declarator(decl: AbstractDeclarator, base: Type) -> Type {
        match decl {
            AbstractDeclarator::Base => base,
            AbstractDeclarator::Pointer(inner) => {
                let derived = Type::Pointer(Box::new(base));
                process_abstract_declarator(*inner, derived)
            }
            AbstractDeclarator::Array(inner, size) => {
                let derived = Type::Array { elem_type: Box::new(base), size };
                process_abstract_declarator(*inner, derived)
            }
        }
    }

    // Expressions
    fn get_precedence(tok: &Token) -> Option<i32> {
        use Token::*;
        match tok {
            Star | Slash | Percent => Some(50),
            Plus | Hyphen => Some(45),
            LessThan | LessOrEqual | GreaterThan | GreaterOrEqual => Some(35),
            DoubleEqual | NotEqual => Some(30),
            LogicalAnd => Some(10),
            LogicalOr => Some(5),
            QuestionMark => Some(3),
            EqualSign => Some(1),
            _ => None,
        }
    }

    fn parse_unop(ts: &mut TokenStream) -> Result<UnaryOperator> {
        match ts.take_token() {
            Some(Token::Tilde) => Ok(UnaryOperator::Complement),
            Some(Token::Hyphen) => Ok(UnaryOperator::Negate),
            Some(Token::Bang) => Ok(UnaryOperator::Not),
            other => Err(ParseError(format!("expected unary operator got {:?}", other))),
        }
    }

    fn parse_binop(ts: &mut TokenStream) -> Result<BinaryOperator> {
        match ts.take_token() {
            Some(Token::Plus) => Ok(BinaryOperator::Add),
            Some(Token::Hyphen) => Ok(BinaryOperator::Subtract),
            Some(Token::Star) => Ok(BinaryOperator::Multiply),
            Some(Token::Slash) => Ok(BinaryOperator::Divide),
            Some(Token::Percent) => Ok(BinaryOperator::Mod),
            Some(Token::LogicalAnd) => Ok(BinaryOperator::And),
            Some(Token::LogicalOr) => Ok(BinaryOperator::Or),
            Some(Token::DoubleEqual) => Ok(BinaryOperator::Equal),
            Some(Token::NotEqual) => Ok(BinaryOperator::NotEqual),
            Some(Token::LessThan) => Ok(BinaryOperator::LessThan),
            Some(Token::LessOrEqual) => Ok(BinaryOperator::LessOrEqual),
            Some(Token::GreaterThan) => Ok(BinaryOperator::GreaterThan),
            Some(Token::GreaterOrEqual) => Ok(BinaryOperator::GreaterOrEqual),
            other => Err(ParseError(format!("expected binary operator got {:?}", other))),
        }
    }

    fn parse_type_name(ts: &mut TokenStream) -> Result<Type> {
        let specs = parse_type_specifier_list(ts)?;
        let base = parse_type(&specs)?;
        if ts.peek() == Some(&Token::CloseParen) {
            Ok(base)
        } else {
            let decl = parse_abstract_declarator(ts)?;
            Ok(process_abstract_declarator(decl, base))
        }
    }

    fn parse_primary_exp(ts: &mut TokenStream) -> Result<Exp> {
        let next = ts.peek().cloned().ok_or_else(|| ParseError("EOF".into()))?;
        match next {
            Token::ConstInt(_)
            | Token::ConstLong(_)
            | Token::ConstUInt(_)
            | Token::ConstULong(_)
            | Token::ConstDouble(_)
            | Token::ConstChar(_) => Ok(Exp::Constant(parse_const(ts)?)),
            Token::Identifier(_) => {
                let id = parse_id(ts)?;
                if ts.peek() == Some(&Token::OpenParen) {
                    ts.take_token();
                    let args = if ts.peek() == Some(&Token::CloseParen) {
                        Vec::new()
                    } else {
                        parse_argument_list(ts)?
                    };
                    expect(ts, Token::CloseParen)?;
                    Ok(Exp::FunCall { f: id, args })
                } else {
                    Ok(Exp::Var(id))
                }
            }
            Token::OpenParen => {
                ts.take_token();
                let e = parse_exp(0, ts)?;
                expect(ts, Token::CloseParen)?;
                Ok(e)
            }
           Token::StringLiteral(_) => {
                let mut s = parse_string(ts)?;
                while let Some(Token::StringLiteral(_)) = ts.peek() {
                    s.push_str(&parse_string(ts)?);
                }
                Ok(Exp::String(s))
            }
            other => Err(ParseError(format!("expected primary expression got {:?}", other))),
        }
    }

    fn parse_argument_list(ts: &mut TokenStream) -> Result<Vec<Exp>> {
        let mut args = Vec::new();
        args.push(parse_exp(0, ts)?);
        while ts.peek() == Some(&Token::Comma) {
            ts.take_token();
            args.push(parse_exp(0, ts)?);
        }
        Ok(args)
    }

    fn parse_postfix_exp(ts: &mut TokenStream) -> Result<Exp> {
        let mut e = parse_primary_exp(ts)?;
        loop {
            match ts.peek() {
                Some(Token::OpenBracket) => {
                    ts.take_token();
                    let sub = parse_exp(0, ts)?;
                    expect(ts, Token::CloseBracket)?;
                    e = Exp::Subscript { ptr: Box::new(e), index: Box::new(sub) };
                }
                Some(Token::Dot) => {
                    ts.take_token();
                    let member = parse_id(ts)?;
                    e = Exp::Dot { strct: Box::new(e), member };
                }
                Some(Token::Arrow) => {
                    ts.take_token();
                    let member = parse_id(ts)?;
                    e = Exp::Arrow { strct: Box::new(e), member };
                }
                _ => break,
            }
        }
        Ok(e)
    }

    fn parse_unary_exp(ts: &mut TokenStream) -> Result<Exp> {
        let look = ts.npeek(3);
        if let Some(t) = look.get(0) {
            match t {
                Token::Star => {
                    ts.take_token();
                    let inner = parse_cast_exp(ts)?;
                    Ok(Exp::Dereference(Box::new(inner)))
                }
                Token::Ampersand => {
                    ts.take_token();
                    let inner = parse_cast_exp(ts)?;
                    Ok(Exp::AddrOf(Box::new(inner)))
                }
                Token::Hyphen | Token::Tilde | Token::Bang => {
                    let op = parse_unop(ts)?;
                    let inner = parse_cast_exp(ts)?;
                    Ok(Exp::Unary(op, Box::new(inner)))
                }
                Token::KWSizeOf => {
                    if matches!(look.get(1), Some(Token::OpenParen)) && look.get(2).map_or(false, is_type_specifier) {
                        ts.take_token();
                        ts.take_token();
                        let typ = parse_type_name(ts)?;
                        expect(ts, Token::CloseParen)?;
                        Ok(Exp::SizeOfT(typ))
                    } else {
                        ts.take_token();
                        let inner = parse_unary_exp(ts)?;
                        Ok(Exp::SizeOf(Box::new(inner)))
                    }
                }
                _ => parse_postfix_exp(ts),
            }
        } else {
            Err(ParseError("Unexpected EOF".into()))
        }
    }

    fn parse_cast_exp(ts: &mut TokenStream) -> Result<Exp> {
        let look = ts.npeek(2);
        if look.len() == 2 && look[0] == Token::OpenParen && is_type_specifier(&look[1]) {
            ts.take_token();
            let target_type = parse_type_name(ts)?;
            expect(ts, Token::CloseParen)?;
            let inner = parse_cast_exp(ts)?;
            Ok(Exp::Cast { target_type, e: Box::new(inner) })
        } else {
            parse_unary_exp(ts)
        }
    }

    fn parse_conditional_middle(ts: &mut TokenStream) -> Result<Exp> {
        expect(ts, Token::QuestionMark)?;
        let e = parse_exp(0, ts)?;
        expect(ts, Token::Colon)?;
        Ok(e)
    }

    pub fn parse_exp(min_prec: i32, ts: &mut TokenStream) -> Result<Exp> {
        let mut left = parse_cast_exp(ts)?;
        loop {
            let next = ts.peek().cloned();
            let Some(tok) = next else { break };
            let Some(prec) = get_precedence(&tok) else { break };
            if prec < min_prec { break; }
            if tok == Token::EqualSign {
                ts.take_token();
                let right = parse_exp(prec, ts)?;
                left = Exp::Assignment(Box::new(left), Box::new(right));
            } else if tok == Token::QuestionMark {
                let middle = parse_conditional_middle(ts)?;
                let right = parse_exp(prec, ts)?;
                left = Exp::Conditional { condition: Box::new(left), then_result: Box::new(middle), else_result: Box::new(right) };
            } else {
                let op = parse_binop(ts)?;
                let right = parse_exp(prec + 1, ts)?;
                left = Exp::Binary(op, Box::new(left), Box::new(right));
            }
        }
        Ok(left)
    }

    fn parse_optional_exp(delim: Token, ts: &mut TokenStream) -> Result<Option<Exp>> {
        if ts.peek() == Some(&delim) {
            ts.take_token();
            Ok(None)
        } else {
            let e = parse_exp(0, ts)?;
            expect(ts, delim)?;
            Ok(Some(e))
        }
    }

    // Declarations
    #[derive(Clone)]
    enum Declarator {
        Ident(String),
        PointerDeclarator(Box<Declarator>),
        ArrayDeclarator(Box<Declarator>, usize),
        FunDeclarator(Vec<ParamInfo>, Box<Declarator>),
    }

    #[derive(Clone)]
    struct ParamInfo(Type, Declarator);

    fn parse_array_decl_suffix(base: Declarator, ts: &mut TokenStream) -> Result<Declarator> {
        let dim = parse_dim(ts)?;
        let new_decl = Declarator::ArrayDeclarator(Box::new(base), dim);
        if ts.peek() == Some(&Token::OpenBracket) {
            parse_array_decl_suffix(new_decl, ts)
        } else {
            Ok(new_decl)
        }
    }

    fn parse_declarator(ts: &mut TokenStream) -> Result<Declarator> {
        match ts.peek() {
            Some(Token::Star) => {
                ts.take_token();
                let inner = parse_declarator(ts)?;
                Ok(Declarator::PointerDeclarator(Box::new(inner)))
            }
            _ => parse_direct_declarator(ts),
        }
    }

    fn parse_direct_declarator(ts: &mut TokenStream) -> Result<Declarator> {
        let simple = parse_simple_declarator(ts)?;
        match ts.peek() {
            Some(Token::OpenParen) => {
                let params = parse_param_list(ts)?;
                Ok(Declarator::FunDeclarator(params, Box::new(simple)))
            }
            Some(Token::OpenBracket) => parse_array_decl_suffix(simple, ts),
            _ => Ok(simple),
        }
    }

    fn parse_param_list(ts: &mut TokenStream) -> Result<Vec<ParamInfo>> {
        if ts.npeek(3) == vec![Token::OpenParen, Token::KWVoid, Token::CloseParen] {
            ts.take_token();
            ts.take_token();
            ts.take_token();
            Ok(Vec::new())
        } else {
            expect(ts, Token::OpenParen)?;
            let mut params = Vec::new();
            params.push(parse_param(ts)?);
            while ts.peek() == Some(&Token::Comma) {
                ts.take_token();
                params.push(parse_param(ts)?);
            }
            expect(ts, Token::CloseParen)?;
            Ok(params)
        }
    }

    fn parse_param(ts: &mut TokenStream) -> Result<ParamInfo> {
        let param_t = parse_type(&parse_type_specifier_list(ts)?)?;
        let param_decl = parse_declarator(ts)?;
        Ok(ParamInfo(param_t, param_decl))
    }

    fn parse_simple_declarator(ts: &mut TokenStream) -> Result<Declarator> {
        match ts.take_token() {
            Some(Token::OpenParen) => {
                let decl = parse_declarator(ts)?;
                expect(ts, Token::CloseParen)?;
                Ok(decl)
            }
            Some(Token::Identifier(id)) => Ok(Declarator::Ident(id)),
            other => Err(ParseError(format!("expected simple declarator got {:?}", other))),
        }
    }

    fn process_declarator(decl: Declarator, base: Type) -> (String, Type, Vec<String>) {
        match decl {
            Declarator::Ident(s) => (s, base, Vec::new()),
            Declarator::PointerDeclarator(d) => {
                let derived = Type::Pointer(Box::new(base));
                process_declarator(*d, derived)
            }
            Declarator::ArrayDeclarator(inner, size) => {
                let derived = Type::Array { elem_type: Box::new(base), size };
                process_declarator(*inner, derived)
            }
            Declarator::FunDeclarator(params, inner) => {
                let mut param_names = Vec::new();
                let mut param_types = Vec::new();
                for ParamInfo(pt, pd) in params {
                    let (name, t, _) = process_declarator(pd, pt);
                    if let Type::FunType { .. } = t {
                        // unsupported
                    }
                    param_names.push(name);
                    param_types.push(t);
                }
                let fun_type = Type::FunType { param_types, ret_type: Box::new(base) };
                match *inner {
                    Declarator::Ident(s) => (s, fun_type, param_names),
                    _ => ("".into(), fun_type, param_names),
                }
            }
        }
    }

    fn parse_initializer(ts: &mut TokenStream) -> Result<Initializer> {
        if ts.peek() == Some(&Token::OpenBrace) {
            ts.take_token();
            let mut inits = Vec::new();
            loop {
                let next = parse_initializer(ts)?;
                inits.push(next);
                match ts.npeek(2).as_slice() {
                    [Token::Comma, Token::CloseBrace] => {
                        ts.take_token();
                        break;
                    }
                    [Token::Comma, _] => {
                        ts.take_token();
                    }
                    _ => break,
                }
            }
            expect(ts, Token::CloseBrace)?;
            Ok(Initializer::CompoundInit(inits))
        } else {
            Ok(Initializer::SingleInit(parse_exp(0, ts)?))
        }
    }

    fn parse_member_declaration(ts: &mut TokenStream) -> Result<MemberDeclaration> {
        let specs = parse_type_specifier_list(ts)?;
        let base = parse_type(&specs)?;
        let decl = parse_declarator(ts)?;
        if let Declarator::FunDeclarator(_, _) = decl {
            return Err(ParseError("Function declarator in struct member".into()));
        }
        expect(ts, Token::Semicolon)?;
        let (member_name, member_type, _) = process_declarator(decl, base);
        Ok(MemberDeclaration { member_name, member_type })
    }

    fn parse_struct_declaration(ts: &mut TokenStream) -> Result<StructDeclaration> {
        expect(ts, Token::KWStruct)?;
        let tag = parse_id(ts)?;
        let members = if ts.peek() == Some(&Token::OpenBrace) {
            ts.take_token();
            let mut mems = Vec::new();
            while ts.peek() != Some(&Token::CloseBrace) {
                mems.push(parse_member_declaration(ts)?);
            }
            expect(ts, Token::CloseBrace)?;
            mems
        } else {
            Vec::new()
        };
        expect(ts, Token::Semicolon)?;
        Ok(StructDeclaration { tag, members })
    }

    fn parse_function_or_variable_declaration(ts: &mut TokenStream) -> Result<Declaration> {
        let specs = parse_specifier_list(ts)?;
        let (base_type, storage_class) = parse_type_and_storage_class(&specs)?;
        let decl = parse_declarator(ts)?;
        let (name, typ, params) = process_declarator(decl, base_type.clone());
        match typ {
            Type::FunType { .. } => {
                let body = if ts.peek() == Some(&Token::Semicolon) {
                    ts.take_token();
                    None
                } else {
                    Some(parse_block(ts)?)
                };
                Ok(Declaration::FunDecl(FunctionDeclaration { name, fun_type: typ, params, body, storage_class }))
            }
            _ => {
                let init = if ts.peek() == Some(&Token::EqualSign) {
                    ts.take_token();
                    Some(parse_initializer(ts)?)
                } else {
                    None
                };
                expect(ts, Token::Semicolon)?;
                Ok(Declaration::VarDecl(VariableDeclaration { name, var_type: typ, init, storage_class }))
            }
        }
    }

    fn parse_declaration(ts: &mut TokenStream) -> Result<Declaration> {
        match ts.npeek(3).as_slice() {
            [Token::KWStruct, Token::Identifier(_), Token::OpenBrace]
            | [Token::KWStruct, Token::Identifier(_), Token::Semicolon] => {
                Ok(Declaration::StructDecl(parse_struct_declaration(ts)?))
            }
            _ => parse_function_or_variable_declaration(ts),
        }
    }

    fn parse_for_init(ts: &mut TokenStream) -> Result<ForInit> {
        if let Some(tok) = ts.peek() {
            if is_specifier(tok) {
                match parse_declaration(ts)? {
                    Declaration::VarDecl(vd) => Ok(ForInit::InitDecl(vd)),
                    _ => Err(ParseError("Found function declaration in for loop".into())),
                }
            } else {
                Ok(ForInit::InitExp(parse_optional_exp(Token::Semicolon, ts)?))
            }
        } else {
            Err(ParseError("Unexpected EOF".into()))
        }
    }

    pub fn parse_statement(ts: &mut TokenStream) -> Result<Statement> {
        match ts.peek() {
            Some(Token::KWReturn) => {
                ts.take_token();
                let exp = parse_optional_exp(Token::Semicolon, ts)?;
                Ok(Statement::Return(exp))
            }
            Some(Token::KWIf) => {
                ts.take_token();
                expect(ts, Token::OpenParen)?;
                let condition = parse_exp(0, ts)?;
                expect(ts, Token::CloseParen)?;
                let then_clause = parse_statement(ts)?;
                let else_clause = if ts.peek() == Some(&Token::KWElse) {
                    ts.take_token();
                    Some(Box::new(parse_statement(ts)?))
                } else {
                    None
                };
                Ok(Statement::If { condition, then_clause: Box::new(then_clause), else_clause })
            }
            Some(Token::OpenBrace) => Ok(Statement::Compound(parse_block(ts)?)),
            Some(Token::KWBreak) => {
                ts.take_token();
                expect(ts, Token::Semicolon)?;
                Ok(Statement::Break(String::new()))
            }
            Some(Token::KWContinue) => {
                ts.take_token();
                expect(ts, Token::Semicolon)?;
                Ok(Statement::Continue(String::new()))
            }
            Some(Token::KWWhile) => {
                ts.take_token();
                expect(ts, Token::OpenParen)?;
                let condition = parse_exp(0, ts)?;
                expect(ts, Token::CloseParen)?;
                let body = parse_statement(ts)?;
                Ok(Statement::While { condition, body: Box::new(body), id: String::new() })
            }
            Some(Token::KWDo) => {
                ts.take_token();
                let body = parse_statement(ts)?;
                expect(ts, Token::KWWhile)?;
                expect(ts, Token::OpenParen)?;
                let condition = parse_exp(0, ts)?;
                expect(ts, Token::CloseParen)?;
                expect(ts, Token::Semicolon)?;
                Ok(Statement::DoWhile { body: Box::new(body), condition, id: String::new() })
            }
            Some(Token::KWFor) => {
                ts.take_token();
                expect(ts, Token::OpenParen)?;
                let init = parse_for_init(ts)?;
                let condition = parse_optional_exp(Token::Semicolon, ts)?;
                let post = parse_optional_exp(Token::CloseParen, ts)?;
                let body = parse_statement(ts)?;
                Ok(Statement::For { init, condition, post, body: Box::new(body), id: String::new() })
            }
            _ => {
                let exp = parse_optional_exp(Token::Semicolon, ts)?;
                match exp {
                    Some(e) => Ok(Statement::Expression(e)),
                    None => Ok(Statement::Null),
                }
            }
        }
    }

    fn parse_block_item(ts: &mut TokenStream) -> Result<BlockItem> {
        if let Some(tok) = ts.peek() {
            if is_specifier(tok) {
                Ok(BlockItem::D(parse_declaration(ts)?))
            } else {
                Ok(BlockItem::S(parse_statement(ts)?))
            }
        } else {
            Err(ParseError("Unexpected EOF".into()))
        }
    }

    fn parse_block(ts: &mut TokenStream) -> Result<Block> {
        expect(ts, Token::OpenBrace)?;
        let mut items = Vec::new();
        while ts.peek() != Some(&Token::CloseBrace) {
            items.push(parse_block_item(ts)?);
        }
        expect(ts, Token::CloseBrace)?;
        Ok(Block(items))
    }

    pub fn parse_program(ts: &mut TokenStream) -> Result<Program> {
        let mut decls = Vec::new();
        while !ts.is_empty() {
            decls.push(parse_declaration(ts)?);
        }
        Ok(Program(decls))
    }
}

pub use private::{parse_const, parse_exp, parse_statement, parse_id};

pub fn parse(tokens: Vec<Token>) -> Result<Program> {
    let mut ts = TokenStream::new(tokens);
    private::parse_program(&mut ts)
}
