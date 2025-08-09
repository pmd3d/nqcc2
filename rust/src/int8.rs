use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Int8(pub i8);

impl Int8 {
    pub const ZERO: Int8 = Int8(0);

    pub fn of_int(i: i32) -> Self {
        Int8(i as i8)
    }

    pub fn to_int(self) -> i32 {
        self.0 as i32
    }

    pub fn of_int64(i: i64) -> Self {
        Int8(i as i8)
    }

    pub fn to_int64(self) -> i64 {
        self.0 as i64
    }

    pub fn to_string(self) -> String {
        self.0.to_string()
    }
}

impl fmt::Display for Int8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn show(i: Int8) -> String {
    i.to_string()
}

pub fn pp(f: &mut fmt::Formatter<'_>, i: &Int8) -> fmt::Result {
    write!(f, "{}", i.0)
}
