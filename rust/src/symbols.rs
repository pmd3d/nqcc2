use crate::initializers::StaticInit;
use crate::types::Type;
use crate::unique_ids;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Mutex;

#[derive(Debug, Clone, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierAttrs {
    FunAttr { defined: bool, global: bool },
    StaticAttr { init: InitialValue, global: bool },
    ConstAttr(StaticInit),
    LocalAttr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub t: Type,
    pub attrs: IdentifierAttrs,
}

static SYMBOL_TABLE: Lazy<Mutex<HashMap<String, Entry>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub fn add_automatic_var(name: String, t: Type) {
    SYMBOL_TABLE.lock().unwrap().insert(
        name,
        Entry { t, attrs: IdentifierAttrs::LocalAttr },
    );
}

pub fn add_static_var(name: String, t: Type, global: bool, init: InitialValue) {
    SYMBOL_TABLE.lock().unwrap().insert(
        name,
        Entry { t, attrs: IdentifierAttrs::StaticAttr { init, global } },
    );
}

pub fn add_fun(name: String, t: Type, global: bool, defined: bool) {
    SYMBOL_TABLE.lock().unwrap().insert(
        name,
        Entry { t, attrs: IdentifierAttrs::FunAttr { global, defined } },
    );
}

pub fn get(name: &str) -> Entry {
    SYMBOL_TABLE
        .lock()
        .unwrap()
        .get(name)
        .cloned()
        .expect("symbol not found")
}

pub fn get_opt(name: &str) -> Option<Entry> {
    SYMBOL_TABLE.lock().unwrap().get(name).cloned()
}

pub fn add_string(s: &str) -> String {
    let str_id = unique_ids::make_named_temporary("string");
    let t = Type::Array { elem_type: Box::new(Type::Char), size: s.len() + 1 };
    SYMBOL_TABLE.lock().unwrap().insert(
        str_id.clone(),
        Entry { t, attrs: IdentifierAttrs::ConstAttr(StaticInit::StringInit(s.into(), true)) },
    );
    str_id
}

pub fn is_global(name: &str) -> bool {
    match &get(name).attrs {
        IdentifierAttrs::LocalAttr | IdentifierAttrs::ConstAttr(_) => false,
        IdentifierAttrs::StaticAttr { global, .. } => *global,
        IdentifierAttrs::FunAttr { global, .. } => *global,
    }
}

pub fn bindings() -> Vec<(String, Entry)> {
    SYMBOL_TABLE
        .lock()
        .unwrap()
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect()
}

pub fn iter<F: FnMut(&String, &Entry)>(mut f: F) {
    for (k, v) in SYMBOL_TABLE.lock().unwrap().iter() {
        f(k, v);
    }
}
