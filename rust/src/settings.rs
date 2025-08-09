use once_cell::sync::Lazy;
use std::sync::Mutex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    Lex,
    Parse,
    Validate,
    Tacky,
    Codegen,
    Assembly,
    Obj,
    Executable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Platform {
    OSX,
    Linux,
}

pub static PLATFORM: Lazy<Mutex<Platform>> = Lazy::new(|| Mutex::new(Platform::OSX));
pub static DEBUG: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Optimizations {
    pub constant_folding: bool,
    pub dead_store_elimination: bool,
    pub unreachable_code_elimination: bool,
    pub copy_propagation: bool,
}

impl Default for Optimizations {
    fn default() -> Self {
        Self {
            constant_folding: false,
            dead_store_elimination: false,
            unreachable_code_elimination: false,
            copy_propagation: false,
        }
    }
}

