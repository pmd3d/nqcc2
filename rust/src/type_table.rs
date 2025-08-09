use crate::types::Type;
use once_cell::sync::Lazy;
use std::{collections::HashMap, sync::Mutex};

#[derive(Debug, Clone)]
pub struct MemberEntry {
    pub member_type: Type,
    pub offset: usize,
}

#[derive(Debug, Clone)]
pub struct StructEntry {
    pub alignment: usize,
    pub size: usize,
    pub members: HashMap<String, MemberEntry>,
}

static TYPE_TABLE: Lazy<Mutex<HashMap<String, StructEntry>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub fn add_struct_definition(tag: String, struct_def: StructEntry) {
    TYPE_TABLE.lock().unwrap().insert(tag, struct_def);
}

pub fn mem(tag: &str) -> bool {
    TYPE_TABLE.lock().unwrap().contains_key(tag)
}

pub fn find(tag: &str) -> StructEntry {
    TYPE_TABLE
        .lock()
        .unwrap()
        .get(tag)
        .cloned()
        .expect("struct definition not found")
}

pub fn get_members(tag: &str) -> Vec<MemberEntry> {
    let struct_def = find(tag);
    let mut members: Vec<MemberEntry> = struct_def.members.values().cloned().collect();
    members.sort_by_key(|m| m.offset);
    members
}

pub fn get_member_types(tag: &str) -> Vec<Type> {
    get_members(tag)
        .into_iter()
        .map(|m| m.member_type)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn members_are_sorted() {
        let mut members = HashMap::new();
        members.insert(
            "b".into(),
            MemberEntry {
                member_type: Type::Int,
                offset: 4,
            },
        );
        members.insert(
            "a".into(),
            MemberEntry {
                member_type: Type::Char,
                offset: 0,
            },
        );
        let struct_def = StructEntry {
            alignment: 4,
            size: 8,
            members,
        };
        add_struct_definition("s".into(), struct_def);
        let members = get_members("s");
        assert_eq!(members[0].offset, 0);
        assert_eq!(members[1].offset, 4);
        let types = get_member_types("s");
        assert_eq!(types, vec![Type::Char, Type::Int]);
    }
}
