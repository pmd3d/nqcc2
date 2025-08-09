use crate::ast::untyped as U;
use crate::unique_ids;

fn label_statement(current_label: Option<String>, s: U::Statement) -> U::Statement {
    use U::Statement::*;
    match s {
        Break(_) => match current_label {
            Some(l) => Break(l),
            None => panic!("Break outside of loop"),
        },
        Continue(_) => match current_label {
            Some(l) => Continue(l),
            None => panic!("Continue outside of loop"),
        },
        While { condition, body, .. } => {
            let new_id = unique_ids::make_label("while");
            While {
                condition: label_exp(condition, &current_label),
                body: Box::new(label_statement(Some(new_id.clone()), *body)),
                id: new_id,
            }
        }
        DoWhile { body, condition, .. } => {
            let new_id = unique_ids::make_label("do_while");
            DoWhile {
                body: Box::new(label_statement(Some(new_id.clone()), *body)),
                condition: label_exp(condition, &current_label),
                id: new_id,
            }
        }
        For { init, condition, post, body, .. } => {
            let new_id = unique_ids::make_label("for");
            For {
                init: label_for_init(init, current_label.clone()),
                condition: condition.map(|e| label_exp(e, &current_label)),
                post: post.map(|e| label_exp(e, &current_label)),
                body: Box::new(label_statement(Some(new_id.clone()), *body)),
                id: new_id,
            }
        }
        If { condition, then_clause, else_clause } => {
            If {
                condition: label_exp(condition, &current_label),
                then_clause: Box::new(label_statement(current_label.clone(), *then_clause)),
                else_clause: else_clause
                    .map(|b| Box::new(label_statement(current_label.clone(), *b))),
            }
        }
        Compound(block) => Compound(label_block(current_label, block)),
        Return(e) => Return(e.map(|e| label_exp(e, &None))),
        Expression(e) => Expression(label_exp(e, &None)),
        Null => Null,
    }
}

fn label_exp(e: U::Exp, current_label: &Option<String>) -> U::Exp {
    match e {
        U::Exp::Assignment(l, r) => U::Exp::Assignment(
            Box::new(label_exp(*l, current_label)),
            Box::new(label_exp(*r, current_label)),
        ),
        U::Exp::Cast { target_type, e } => U::Exp::Cast {
            target_type,
            e: Box::new(label_exp(*e, current_label)),
        },
        U::Exp::Unary(op, e) => U::Exp::Unary(op, Box::new(label_exp(*e, current_label))),
        U::Exp::Binary(op, e1, e2) => U::Exp::Binary(
            op,
            Box::new(label_exp(*e1, current_label)),
            Box::new(label_exp(*e2, current_label)),
        ),
        U::Exp::Conditional { condition, then_result, else_result } => U::Exp::Conditional {
            condition: Box::new(label_exp(*condition, current_label)),
            then_result: Box::new(label_exp(*then_result, current_label)),
            else_result: Box::new(label_exp(*else_result, current_label)),
        },
        U::Exp::FunCall { f, args } => U::Exp::FunCall {
            f,
            args: args
                .into_iter()
                .map(|a| label_exp(a, current_label))
                .collect(),
        },
        U::Exp::Dereference(inner) =>
            U::Exp::Dereference(Box::new(label_exp(*inner, current_label))),
        U::Exp::AddrOf(inner) => U::Exp::AddrOf(Box::new(label_exp(*inner, current_label))),
        U::Exp::Subscript { ptr, index } => U::Exp::Subscript {
            ptr: Box::new(label_exp(*ptr, current_label)),
            index: Box::new(label_exp(*index, current_label)),
        },
        U::Exp::SizeOf(e) => U::Exp::SizeOf(Box::new(label_exp(*e, current_label))),
        U::Exp::Dot { strct, member } => U::Exp::Dot {
            strct: Box::new(label_exp(*strct, current_label)),
            member,
        },
        U::Exp::Arrow { strct, member } => U::Exp::Arrow {
            strct: Box::new(label_exp(*strct, current_label)),
            member,
        },
        other => other,
    }
}

fn label_for_init(init: U::ForInit, current_label: Option<String>) -> U::ForInit {
    match init {
        U::ForInit::InitDecl(vd) => U::ForInit::InitDecl(vd),
        U::ForInit::InitExp(e) =>
            U::ForInit::InitExp(e.map(|e| label_exp(e, &current_label))),
    }
}

fn label_block_item(current_label: Option<String>, item: U::BlockItem) -> U::BlockItem {
    match item {
        U::BlockItem::S(s) => U::BlockItem::S(label_statement(current_label, s)),
        decl => decl,
    }
}

fn label_block(current_label: Option<String>, U::Block(items): U::Block) -> U::Block {
    U::Block(
        items
            .into_iter()
            .map(|i| label_block_item(current_label.clone(), i))
            .collect(),
    )
}

fn label_decl(d: U::Declaration) -> U::Declaration {
    match d {
        U::Declaration::FunDecl(mut fd) => {
            fd.body = fd
                .body
                .map(|b| label_block(None, b));
            U::Declaration::FunDecl(fd)
        }
        other => other,
    }
}

pub fn label_loops(U::Program(decls): U::Program) -> U::Program {
    U::Program(decls.into_iter().map(label_decl).collect())
}
