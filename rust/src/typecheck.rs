use crate::ast::{typed as T, untyped as U};
use crate::consts;
use crate::symbols;
use crate::types::Type;

/// A very small and incomplete type checker.
///
/// The original OCaml implementation performs full semantic analysis and
/// verifies that every expression has a well defined type.  Porting the whole
/// module is out of scope for this educational repository so the Rust port
/// keeps the structure of the pass but performs only a shallow translation from
/// the untyped AST to the typed AST.  Many of the heavy semantic checks are
/// omitted which keeps the optimisation passes stubbed as in the OCaml
/// reference implementation used for this port.
///
/// Despite being incomplete this module is useful for the parts of the project
/// that expect a typed AST.  Expression types are propagated in a best effort
/// fashion and default to `Type::Int` when the information is unavailable.

fn tc_exp(e: U::Exp) -> T::Exp {
    match e {
        U::Exp::Constant(c) => T::Exp {
            e: T::InnerExp::Constant(c.clone()),
            t: consts::type_of_const(&c),
        },
        U::Exp::Var(v) => {
            let t = symbols::get(&v).t;
            T::Exp {
                e: T::InnerExp::Var(v),
                t,
            }
        }
        U::Exp::String(s) => {
            let t = Type::Array {
                elem_type: Box::new(Type::Char),
                size: s.len() + 1,
            };
            T::Exp {
                e: T::InnerExp::String(s),
                t,
            }
        }
        U::Exp::Cast {
            target_type,
            e: inner,
        } => {
            let inner_tc = tc_exp(*inner);
            T::Exp {
                e: T::InnerExp::Cast {
                    target_type: target_type.clone(),
                    e: Box::new(inner_tc),
                },
                t: target_type,
            }
        }
        U::Exp::Unary(op, inner) => {
            let inner_tc = tc_exp(*inner);
            let t = inner_tc.t.clone();
            T::Exp {
                e: T::InnerExp::Unary(op, Box::new(inner_tc)),
                t,
            }
        }
        U::Exp::Binary(op, e1, e2) => {
            let lhs = tc_exp(*e1);
            let rhs = tc_exp(*e2);
            let t = lhs.t.clone();
            T::Exp {
                e: T::InnerExp::Binary(op, Box::new(lhs), Box::new(rhs)),
                t,
            }
        }
        U::Exp::Assignment(lhs, rhs) => {
            let lhs_tc = tc_exp(*lhs);
            let rhs_tc = tc_exp(*rhs);
            let t = lhs_tc.t.clone();
            T::Exp {
                e: T::InnerExp::Assignment(Box::new(lhs_tc), Box::new(rhs_tc)),
                t,
            }
        }
        U::Exp::Conditional {
            condition,
            then_result,
            else_result,
        } => {
            let cond = tc_exp(*condition);
            let then_tc = tc_exp(*then_result);
            let else_tc = tc_exp(*else_result);
            let t = then_tc.t.clone();
            T::Exp {
                e: T::InnerExp::Conditional {
                    condition: Box::new(cond),
                    then_result: Box::new(then_tc.clone()),
                    else_result: Box::new(else_tc),
                },
                t,
            }
        }
        U::Exp::FunCall { f, args } => {
            let typed_args: Vec<T::Exp> = args.into_iter().map(tc_exp).collect();
            let ret_type = match symbols::get(&f).t {
                Type::FunType { ret_type, .. } => (*ret_type).clone(),
                _ => Type::Int,
            };
            T::Exp {
                e: T::InnerExp::FunCall {
                    f,
                    args: typed_args,
                },
                t: ret_type,
            }
        }
        U::Exp::Dereference(inner) => {
            let inner_tc = tc_exp(*inner);
            let t = match &inner_tc.t {
                Type::Pointer(inner) => *inner.clone(),
                _ => inner_tc.t.clone(),
            };
            T::Exp {
                e: T::InnerExp::Dereference(Box::new(inner_tc)),
                t,
            }
        }
        U::Exp::AddrOf(inner) => {
            let inner_tc = tc_exp(*inner);
            let t = Type::Pointer(Box::new(inner_tc.t.clone()));
            T::Exp {
                e: T::InnerExp::AddrOf(Box::new(inner_tc)),
                t,
            }
        }
        U::Exp::Subscript { ptr, index } => {
            let ptr_tc = tc_exp(*ptr);
            let index_tc = tc_exp(*index);
            let t = match &ptr_tc.t {
                Type::Pointer(inner) => *inner.clone(),
                Type::Array { elem_type, .. } => *elem_type.clone(),
                _ => Type::Int,
            };
            T::Exp {
                e: T::InnerExp::Subscript {
                    ptr: Box::new(ptr_tc),
                    index: Box::new(index_tc),
                },
                t,
            }
        }
        U::Exp::SizeOf(inner) => {
            let inner_tc = tc_exp(*inner);
            T::Exp {
                e: T::InnerExp::SizeOf(Box::new(inner_tc)),
                t: Type::ULong,
            }
        }
        U::Exp::SizeOfT(t) => T::Exp {
            e: T::InnerExp::SizeOfT(t),
            t: Type::ULong,
        },
        U::Exp::Dot { strct, member } => {
            let strct_tc = tc_exp(*strct);
            T::Exp {
                e: T::InnerExp::Dot {
                    strct: Box::new(strct_tc),
                    member,
                },
                t: Type::Int,
            }
        }
        U::Exp::Arrow { strct, member } => {
            let strct_tc = tc_exp(*strct);
            T::Exp {
                e: T::InnerExp::Arrow {
                    strct: Box::new(strct_tc),
                    member,
                },
                t: Type::Int,
            }
        }
    }
}

fn tc_initializer(init: U::Initializer) -> T::Initializer {
    match init {
        U::Initializer::SingleInit(e) => T::Initializer::SingleInit(tc_exp(e)),
        U::Initializer::CompoundInit(inits) => {
            let typed_inits = inits.into_iter().map(tc_initializer).collect();
            T::Initializer::CompoundInit(Type::Int, typed_inits)
        }
    }
}

fn tc_var_decl(vd: U::VariableDeclaration) -> T::VariableDeclaration {
    T::VariableDeclaration {
        name: vd.name,
        var_type: vd.var_type.clone(),
        init: vd.init.map(tc_initializer),
        storage_class: vd.storage_class,
    }
}

fn tc_struct_decl(sd: U::StructDeclaration) -> T::StructDeclaration {
    let members = sd
        .members
        .into_iter()
        .map(|m| T::MemberDeclaration {
            member_name: m.member_name,
            member_type: m.member_type,
        })
        .collect();
    T::StructDeclaration {
        tag: sd.tag,
        members,
    }
}

fn tc_for_init(fi: U::ForInit) -> T::ForInit {
    match fi {
        U::ForInit::InitDecl(d) => T::ForInit::InitDecl(tc_var_decl(d)),
        U::ForInit::InitExp(e) => T::ForInit::InitExp(e.map(tc_exp)),
    }
}

fn tc_statement(s: U::Statement) -> T::Statement {
    use U::Statement as US;
    match s {
        US::Return(e) => T::Statement::Return(e.map(tc_exp)),
        US::Expression(e) => T::Statement::Expression(tc_exp(e)),
        US::If {
            condition,
            then_clause,
            else_clause,
        } => T::Statement::If {
            condition: tc_exp(condition),
            then_clause: Box::new(tc_statement(*then_clause)),
            else_clause: else_clause.map(|b| Box::new(tc_statement(*b))),
        },
        US::Compound(b) => T::Statement::Compound(tc_block(b)),
        US::Break(id) => T::Statement::Break(id),
        US::Continue(id) => T::Statement::Continue(id),
        US::While {
            condition,
            body,
            id,
        } => T::Statement::While {
            condition: tc_exp(condition),
            body: Box::new(tc_statement(*body)),
            id,
        },
        US::DoWhile {
            body,
            condition,
            id,
        } => T::Statement::DoWhile {
            body: Box::new(tc_statement(*body)),
            condition: tc_exp(condition),
            id,
        },
        US::For {
            init,
            condition,
            post,
            body,
            id,
        } => T::Statement::For {
            init: tc_for_init(init),
            condition: condition.map(tc_exp),
            post: post.map(tc_exp),
            body: Box::new(tc_statement(*body)),
            id,
        },
        US::Null => T::Statement::Null,
    }
}

fn tc_block(U::Block(items): U::Block) -> T::Block {
    let items = items
        .into_iter()
        .map(|bi| match bi {
            U::BlockItem::S(s) => T::BlockItem::S(tc_statement(s)),
            U::BlockItem::D(d) => T::BlockItem::D(tc_local_decl(d)),
        })
        .collect();
    T::Block(items)
}

fn tc_local_decl(d: U::Declaration) -> T::Declaration {
    match d {
        U::Declaration::VarDecl(vd) => T::Declaration::VarDecl(tc_var_decl(vd)),
        U::Declaration::FunDecl(fd) => T::Declaration::FunDecl(tc_fn_decl(fd)),
        U::Declaration::StructDecl(sd) => T::Declaration::StructDecl(tc_struct_decl(sd)),
    }
}

fn tc_fn_decl(fd: U::FunctionDeclaration) -> T::FunctionDeclaration {
    let body = fd.body.map(tc_block);
    T::FunctionDeclaration {
        name: fd.name,
        fun_type: fd.fun_type,
        params: fd.params,
        body,
        storage_class: fd.storage_class,
    }
}

fn tc_global_decl(d: U::Declaration) -> T::Declaration {
    match d {
        U::Declaration::FunDecl(fd) => T::Declaration::FunDecl(tc_fn_decl(fd)),
        U::Declaration::VarDecl(vd) => T::Declaration::VarDecl(tc_var_decl(vd)),
        U::Declaration::StructDecl(sd) => T::Declaration::StructDecl(tc_struct_decl(sd)),
    }
}

/// Convert the untyped AST into a typed AST.
///
/// This translation only performs a shallow pass and does **not** implement the
/// exhaustive semantic checks from the original compiler.  It is sufficient for
/// the parts of the code base that require a typed AST without engaging the
/// optimisation infrastructure which remains stubbed.
pub fn typecheck(U::Program(decls): U::Program) -> T::Program {
    T::Program(decls.into_iter().map(tc_global_decl).collect())
}
