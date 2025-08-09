use crate::ast::untyped as U;
use crate::ast::StorageClass;
use crate::types::Type;
use crate::unique_ids;
use std::collections::HashMap;

#[derive(Clone)]
struct VarEntry {
    unique_name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

#[derive(Clone)]
struct StructEntry {
    unique_tag: String,
    struct_from_current_scope: bool,
}

fn copy_identifier_map(m: &HashMap<String, VarEntry>) -> HashMap<String, VarEntry> {
    m.iter()
        .map(|(k, v)| {
            (
                k.clone(),
                VarEntry {
                    unique_name: v.unique_name.clone(),
                    from_current_scope: false,
                    has_linkage: v.has_linkage,
                },
            )
        })
        .collect()
}

fn copy_struct_map(m: &HashMap<String, StructEntry>) -> HashMap<String, StructEntry> {
    m.iter()
        .map(|(k, v)| {
            (
                k.clone(),
                StructEntry {
                    unique_tag: v.unique_tag.clone(),
                    struct_from_current_scope: false,
                },
            )
        })
        .collect()
}

fn resolve_type(struct_map: &HashMap<String, StructEntry>, t: &Type) -> Type {
    match t {
        Type::Structure(tag) => {
            if let Some(entry) = struct_map.get(tag) {
                Type::Structure(entry.unique_tag.clone())
            } else {
                panic!("specified undeclared structure type");
            }
        }
        Type::Pointer(inner) => Type::Pointer(Box::new(resolve_type(struct_map, inner))),
        Type::Array { elem_type, size } => Type::Array {
            elem_type: Box::new(resolve_type(struct_map, elem_type)),
            size: *size,
        },
        Type::FunType { param_types, ret_type } => Type::FunType {
            param_types: param_types
                .iter()
                .map(|t| resolve_type(struct_map, t))
                .collect(),
            ret_type: Box::new(resolve_type(struct_map, ret_type)),
        },
        other => other.clone(),
    }
}

fn resolve_exp(
    struct_map: &HashMap<String, StructEntry>,
    id_map: &HashMap<String, VarEntry>,
    e: U::Exp,
) -> U::Exp {
    use U::Exp::*;
    match e {
        Assignment(l, r) => Assignment(
            Box::new(resolve_exp(struct_map, id_map, *l)),
            Box::new(resolve_exp(struct_map, id_map, *r)),
        ),
        Var(v) => {
            if let Some(entry) = id_map.get(&v) {
                Var(entry.unique_name.clone())
            } else {
                panic!("Undeclared variable {}", v);
            }
        }
        Cast { target_type, e } => Cast {
            target_type: resolve_type(struct_map, &target_type),
            e: Box::new(resolve_exp(struct_map, id_map, *e)),
        },
        Unary(op, e) => Unary(op, Box::new(resolve_exp(struct_map, id_map, *e))),
        Binary(op, e1, e2) => Binary(
            op,
            Box::new(resolve_exp(struct_map, id_map, *e1)),
            Box::new(resolve_exp(struct_map, id_map, *e2)),
        ),
        Conditional { condition, then_result, else_result } => Conditional {
            condition: Box::new(resolve_exp(struct_map, id_map, *condition)),
            then_result: Box::new(resolve_exp(struct_map, id_map, *then_result)),
            else_result: Box::new(resolve_exp(struct_map, id_map, *else_result)),
        },
        FunCall { f, args } => {
            if let Some(entry) = id_map.get(&f) {
                FunCall {
                    f: entry.unique_name.clone(),
                    args: args
                        .into_iter()
                        .map(|a| resolve_exp(struct_map, id_map, a))
                        .collect(),
                }
            } else {
                panic!("Undeclared function!");
            }
        }
        Dereference(inner) => Dereference(Box::new(resolve_exp(struct_map, id_map, *inner))),
        AddrOf(inner) => AddrOf(Box::new(resolve_exp(struct_map, id_map, *inner))),
        Subscript { ptr, index } => Subscript {
            ptr: Box::new(resolve_exp(struct_map, id_map, *ptr)),
            index: Box::new(resolve_exp(struct_map, id_map, *index)),
        },
        SizeOf(e) => SizeOf(Box::new(resolve_exp(struct_map, id_map, *e))),
        SizeOfT(t) => SizeOfT(resolve_type(struct_map, &t)),
        Dot { strct, member } => Dot {
            strct: Box::new(resolve_exp(struct_map, id_map, *strct)),
            member,
        },
        Arrow { strct, member } => Arrow {
            strct: Box::new(resolve_exp(struct_map, id_map, *strct)),
            member,
        },
        other => other,
    }
}

fn resolve_optional_exp(
    struct_map: &HashMap<String, StructEntry>,
    id_map: &HashMap<String, VarEntry>,
    e: Option<U::Exp>,
) -> Option<U::Exp> {
    e.map(|e| resolve_exp(struct_map, id_map, e))
}

fn resolve_local_var_helper(
    id_map: &mut HashMap<String, VarEntry>,
    name: String,
    storage_class: Option<StorageClass>,
) -> String {
    if let Some(entry) = id_map.get(&name) {
        if entry.from_current_scope && !(entry.has_linkage && storage_class == Some(StorageClass::Extern)) {
            panic!("Duplicate variable declaration");
        }
    }
    let entry = if storage_class == Some(StorageClass::Extern) {
        VarEntry { unique_name: name.clone(), from_current_scope: true, has_linkage: true }
    } else {
        let unique_name = unique_ids::make_named_temporary(&name);
        VarEntry { unique_name: unique_name.clone(), from_current_scope: true, has_linkage: false }
    };
    let unique = entry.unique_name.clone();
    id_map.insert(name, entry);
    unique
}

fn resolve_initializer(
    struct_map: &HashMap<String, StructEntry>,
    id_map: &HashMap<String, VarEntry>,
    init: U::Initializer,
) -> U::Initializer {
    match init {
        U::Initializer::SingleInit(e) => U::Initializer::SingleInit(resolve_exp(struct_map, id_map, e)),
        U::Initializer::CompoundInit(inits) => U::Initializer::CompoundInit(
            inits
                .into_iter()
                .map(|i| resolve_initializer(struct_map, id_map, i))
                .collect(),
        ),
    }
}

fn resolve_for_init(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    init: U::ForInit,
) -> (HashMap<String, VarEntry>, U::ForInit) {
    match init {
        U::ForInit::InitDecl(vd) => {
            let (new_id_map, resolved_vd) =
                resolve_local_var_declaration(struct_map, id_map, vd);
            (new_id_map, U::ForInit::InitDecl(resolved_vd))
        }
        U::ForInit::InitExp(e) => (
            id_map.clone(),
            U::ForInit::InitExp(e.map(|e| resolve_exp(struct_map, id_map, e))),
        ),
    }
}

fn resolve_statement(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    s: U::Statement,
) -> U::Statement {
    use U::Statement::*;
    match s {
        Return(e) => Return(resolve_optional_exp(struct_map, id_map, e)),
        Expression(e) => Expression(resolve_exp(struct_map, id_map, e)),
        If { condition, then_clause, else_clause } => {
            let mut then_map = copy_identifier_map(id_map);
            let then_clause = Box::new(resolve_statement(struct_map, &mut then_map, *then_clause));
            let else_clause = else_clause.map(|b| {
                let mut else_map = copy_identifier_map(id_map);
                Box::new(resolve_statement(struct_map, &mut else_map, *b))
            });
            If {
                condition: resolve_exp(struct_map, id_map, condition),
                then_clause,
                else_clause,
            }
        }
        While { condition, body, id } => While {
            condition: resolve_exp(struct_map, id_map, condition),
            body: Box::new(resolve_statement(struct_map, id_map, *body)),
            id,
        },
        DoWhile { body, condition, id } => DoWhile {
            body: Box::new(resolve_statement(struct_map, id_map, *body)),
            condition: resolve_exp(struct_map, id_map, condition),
            id,
        },
        For { init, condition, post, body, id } => {
            let mut id_map1 = copy_identifier_map(id_map);
            let mut struct_map1 = copy_struct_map(struct_map);
            let (mut id_map2, resolved_init) =
                resolve_for_init(&mut struct_map1, &mut id_map1, init);
            let condition = resolve_optional_exp(&struct_map1, &id_map2, condition);
            let post = resolve_optional_exp(&struct_map1, &id_map2, post);
            let body = Box::new(resolve_statement(&mut struct_map1, &mut id_map2, *body));
            For {
                init: resolved_init,
                condition,
                post,
                body,
                id,
            }
        }
        Compound(block) => {
            let mut new_id_map = copy_identifier_map(id_map);
            let mut new_struct_map = copy_struct_map(struct_map);
            Compound(resolve_block(&mut new_struct_map, &mut new_id_map, block))
        }
        Break(_) | Continue(_) | Null => s,
    }
}

fn resolve_block_item(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    item: U::BlockItem,
) -> U::BlockItem {
    match item {
        U::BlockItem::S(s) => U::BlockItem::S(resolve_statement(struct_map, id_map, s)),
        U::BlockItem::D(d) => {
            let (maps, decl) = resolve_local_declaration(struct_map, id_map, d);
            *struct_map = maps.0;
            *id_map = maps.1;
            U::BlockItem::D(decl)
        }
    }
}

fn resolve_block(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    U::Block(items): U::Block,
) -> U::Block {
    let mut new_items = Vec::new();
    for item in items {
        let resolved = resolve_block_item(struct_map, id_map, item);
        new_items.push(resolved);
    }
    U::Block(new_items)
}

fn resolve_local_var_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    vd: U::VariableDeclaration,
) -> (HashMap<String, VarEntry>, U::VariableDeclaration) {
    let var_type = resolve_type(struct_map, &vd.var_type);
    let storage_class = vd.storage_class.clone();
    let unique_name = resolve_local_var_helper(id_map, vd.name.clone(), storage_class.clone());
    let init = vd.init.map(|i| resolve_initializer(struct_map, id_map, i));
    (
        id_map.clone(),
        U::VariableDeclaration {
            name: unique_name,
            var_type,
            init,
            storage_class,
        },
    )
}

fn resolve_function_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    fn_decl: U::FunctionDeclaration,
) -> (HashMap<String, VarEntry>, U::FunctionDeclaration) {
    if let Some(entry) = id_map.get(&fn_decl.name) {
        if entry.from_current_scope && !entry.has_linkage {
            panic!("Duplicate declaration");
        }
    }
    let resolved_type = resolve_type(struct_map, &fn_decl.fun_type);
    let entry = VarEntry {
        unique_name: fn_decl.name.clone(),
        from_current_scope: true,
        has_linkage: true,
    };
    id_map.insert(fn_decl.name.clone(), entry);
    let mut inner_id_map = copy_identifier_map(id_map);
    let mut params = Vec::new();
    for p in fn_decl.params.iter() {
        let unique = resolve_local_var_helper(&mut inner_id_map, p.clone(), None);
        params.push(unique);
    }
    let mut inner_struct_map = copy_struct_map(struct_map);
    let body = fn_decl
        .body
        .map(|b| resolve_block(&mut inner_struct_map, &mut inner_id_map, b));
    (
        id_map.clone(),
        U::FunctionDeclaration {
            name: fn_decl.name,
            fun_type: resolved_type,
            params,
            body,
            storage_class: fn_decl.storage_class,
        },
    )
}

fn resolve_structure_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    sd: U::StructDeclaration,
) -> (HashMap<String, StructEntry>, U::StructDeclaration) {
    let prev_entry = struct_map.get(&sd.tag).cloned();
    let (mut new_map, resolved_tag) = match prev_entry {
        Some(entry) if entry.struct_from_current_scope => (struct_map.clone(), entry.unique_tag),
        _ => {
            let unique_tag = unique_ids::make_named_temporary(&sd.tag);
            let entry = StructEntry {
                unique_tag: unique_tag.clone(),
                struct_from_current_scope: true,
            };
            let mut m = struct_map.clone();
            m.insert(sd.tag.clone(), entry);
            (m, unique_tag)
        }
    };
    let members = sd
        .members
        .into_iter()
        .map(|m| U::MemberDeclaration {
            member_name: m.member_name,
            member_type: resolve_type(&new_map, &m.member_type),
        })
        .collect();
    (
        new_map.clone(),
        U::StructDeclaration {
            tag: resolved_tag,
            members,
        },
    )
}

fn resolve_local_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    d: U::Declaration,
) -> ((HashMap<String, StructEntry>, HashMap<String, VarEntry>), U::Declaration) {
    match d {
        U::Declaration::VarDecl(vd) => {
            let (new_id_map, resolved_vd) =
                resolve_local_var_declaration(struct_map, id_map, vd);
            ((struct_map.clone(), new_id_map.clone()), U::Declaration::VarDecl(resolved_vd))
        }
        U::Declaration::FunDecl(fd) => {
            if fd.body.is_some() {
                panic!("nested function definitions are not allowed");
            }
            if fd.storage_class == Some(StorageClass::Static) {
                panic!("static keyword not allowed on local function declarations");
            }
            let (new_id_map, resolved_fd) =
                resolve_function_declaration(struct_map, id_map, fd);
            ((struct_map.clone(), new_id_map.clone()), U::Declaration::FunDecl(resolved_fd))
        }
        U::Declaration::StructDecl(sd) => {
            let (new_struct_map, resolved_sd) = resolve_structure_declaration(struct_map, sd);
            ((new_struct_map.clone(), id_map.clone()), U::Declaration::StructDecl(resolved_sd))
        }
    }
}

fn resolve_file_scope_variable_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    vd: U::VariableDeclaration,
) -> (HashMap<String, VarEntry>, U::VariableDeclaration) {
    let var_type = resolve_type(struct_map, &vd.var_type);
    id_map.insert(
        vd.name.clone(),
        VarEntry {
            unique_name: vd.name.clone(),
            from_current_scope: true,
            has_linkage: true,
        },
    );
    (
        id_map.clone(),
        U::VariableDeclaration {
            name: vd.name,
            var_type,
            init: vd.init.map(|i| resolve_initializer(struct_map, id_map, i)),
            storage_class: vd.storage_class,
        },
    )
}

fn resolve_global_declaration(
    struct_map: &mut HashMap<String, StructEntry>,
    id_map: &mut HashMap<String, VarEntry>,
    d: U::Declaration,
) -> (HashMap<String, StructEntry>, HashMap<String, VarEntry>, U::Declaration) {
    match d {
        U::Declaration::FunDecl(fd) => {
            let (id_map1, fd) = resolve_function_declaration(struct_map, id_map, fd);
            (struct_map.clone(), id_map1, U::Declaration::FunDecl(fd))
        }
        U::Declaration::VarDecl(vd) => {
            let (id_map1, vd) = resolve_file_scope_variable_declaration(struct_map, id_map, vd);
            (struct_map.clone(), id_map1, U::Declaration::VarDecl(vd))
        }
        U::Declaration::StructDecl(sd) => {
            let (struct_map1, sd) = resolve_structure_declaration(struct_map, sd);
            (struct_map1, id_map.clone(), U::Declaration::StructDecl(sd))
        }
    }
}

pub fn resolve(U::Program(decls): U::Program) -> U::Program {
    let mut struct_map: HashMap<String, StructEntry> = HashMap::new();
    let mut id_map: HashMap<String, VarEntry> = HashMap::new();
    let mut resolved = Vec::new();
    for d in decls {
        let (sm, im, rd) = resolve_global_declaration(&mut struct_map, &mut id_map, d);
        struct_map = sm;
        id_map = im;
        resolved.push(rd);
    }
    U::Program(resolved)
}
