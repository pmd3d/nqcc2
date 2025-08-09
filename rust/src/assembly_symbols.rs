use crate::assembly::{AsmType, Reg};
use crate::reg_set::RegSet;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Mutex;

#[derive(Debug, Clone, PartialEq)]
pub enum Entry {
    Fun {
        defined: bool,
        bytes_required: i32,
        return_on_stack: bool,
        param_regs: Vec<Reg>,
        return_regs: Vec<Reg>,
        callee_saved_regs_used: RegSet,
    },
    Obj {
        t: AsmType,
        is_static: bool,
        constant: bool,
    },
}

static SYMBOL_TABLE: Lazy<Mutex<HashMap<String, Entry>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub fn add_fun(
    fun_name: &str,
    defined: bool,
    return_on_stack: bool,
    param_regs: Vec<Reg>,
    return_regs: Vec<Reg>,
) {
    SYMBOL_TABLE.lock().unwrap().insert(
        fun_name.into(),
        Entry::Fun {
            defined,
            bytes_required: 0,
            return_on_stack,
            param_regs,
            return_regs,
            callee_saved_regs_used: RegSet::new(),
        },
    );
}

pub fn add_var(var_name: &str, t: AsmType, is_static: bool) {
    SYMBOL_TABLE.lock().unwrap().insert(
        var_name.into(),
        Entry::Obj { t, is_static, constant: false },
    );
}

pub fn add_constant(const_name: &str, t: AsmType) {
    SYMBOL_TABLE.lock().unwrap().insert(
        const_name.into(),
        Entry::Obj { t, is_static: true, constant: true },
    );
}

pub fn set_bytes_required(fun_name: &str, bytes_required: i32) {
    let mut table = SYMBOL_TABLE.lock().unwrap();
    if let Some(Entry::Fun { bytes_required: br, .. }) = table.get_mut(fun_name) {
        *br = bytes_required;
    } else {
        panic!("Internal error: not a function");
    }
}

pub fn get_bytes_required(fun_name: &str) -> i32 {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { bytes_required, .. }) => *bytes_required,
        _ => panic!("Internal error: not a function"),
    }
}

pub fn add_callee_saved_regs_used(fun_name: &str, regs: &RegSet) {
    let mut table = SYMBOL_TABLE.lock().unwrap();
    match table.get_mut(fun_name) {
        Some(Entry::Fun { callee_saved_regs_used, .. }) => {
            callee_saved_regs_used.extend(regs.iter().cloned());
        }
        _ => panic!("Internal error: not a function"),
    }
}

pub fn get_callee_saved_regs_used(fun_name: &str) -> RegSet {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { callee_saved_regs_used, .. }) => callee_saved_regs_used.clone(),
        _ => panic!("Internal error: not a function"),
    }
}

pub fn get_type(name: &str) -> AsmType {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(name) {
        Some(Entry::Obj { t, .. }) => t.clone(),
        _ => panic!("Internal error: this is a function, not an object"),
    }
}

pub fn get_size(name: &str) -> i32 {
    match get_type(name) {
        AsmType::Byte => 1,
        AsmType::Longword => 4,
        AsmType::Quadword | AsmType::Double => 8,
        AsmType::ByteArray { size, .. } => size,
    }
}

pub fn get_alignment(name: &str) -> i32 {
    match get_type(name) {
        AsmType::Byte => 1,
        AsmType::Longword => 4,
        AsmType::Quadword | AsmType::Double => 8,
        AsmType::ByteArray { alignment, .. } => alignment,
    }
}

pub fn is_defined(fun_name: &str) -> bool {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { defined, .. }) => *defined,
        _ => panic!("Internal error: not a function"),
    }
}

pub fn is_constant(name: &str) -> bool {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(name) {
        Some(Entry::Obj { constant: true, .. }) => true,
        Some(Entry::Obj { .. }) => false,
        _ => panic!("Internal error: is_constant doesn't make sense for functions"),
    }
}

pub fn is_static(name: &str) -> bool {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(name) {
        Some(Entry::Obj { is_static, .. }) => *is_static,
        _ => panic!("Internal error: functions don't have storage duration"),
    }
}

pub fn returns_on_stack(fun_name: &str) -> bool {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { return_on_stack, .. }) => *return_on_stack,
        _ => panic!("Internal error: this is an object, not a function"),
    }
}

pub fn param_regs_used(fun_name: &str) -> Vec<Reg> {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { param_regs, .. }) => param_regs.clone(),
        _ => panic!("Internal error: not a function"),
    }
}

pub fn return_regs_used(fun_name: &str) -> Vec<Reg> {
    let table = SYMBOL_TABLE.lock().unwrap();
    match table.get(fun_name) {
        Some(Entry::Fun { return_regs, .. }) => return_regs.clone(),
        _ => panic!("Internal error: not a function"),
    }
}
