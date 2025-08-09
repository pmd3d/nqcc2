use nqcc_rust::disjoint_sets::DisjointSets;
use nqcc_rust::int8::Int8;
use nqcc_rust::rounding;
use nqcc_rust::type_utils;
use nqcc_rust::types::Type;
use nqcc_rust::unique_ids;
use nqcc_rust::utils::{list_util, string_util};

#[test]
fn test_list_utils() {
    let l = vec![3, 1, 4, 1, 5];
    assert_eq!(list_util::max_by(|a, b| a.cmp(b), &l), Some(&5));
    assert_eq!(list_util::min_by(|a, b| a.cmp(b), &l), Some(&1));
    assert_eq!(list_util::make_list(3, 7), vec![7, 7, 7]);
    assert_eq!(list_util::last(&l), Some(&5));
    assert_eq!(list_util::take(3, &l), vec![3, 1, 4]);
    assert_eq!(list_util::take_drop(3, &l), (vec![3, 1, 4], vec![1, 5]));
}

#[test]
fn test_string_utils() {
    assert_eq!(string_util::drop(2, "abcdef"), "cdef");
    assert_eq!(string_util::chop_suffix("abcdef", 2), "abcd");
    assert_eq!(string_util::of_list(&['a', 'b', 'c']), "abc");
    assert!(string_util::is_alnum('a'));
    assert!(!string_util::is_alnum('!'));
}

#[test]
fn test_int8() {
    let i8 = Int8::of_int(100);
    assert_eq!(i8.to_string(), "100");
    let i8 = Int8::of_int(128);
    assert_eq!(i8.to_string(), "-128");
    let i8 = Int8::of_int64(-110);
    assert_eq!(i8.to_string(), "-110");
    let i8 = Int8::of_int64(1239235325);
    assert_eq!(i8.to_string(), "-3");
    let twelve = Int8::of_int(268);
    let fourteen = Int8::of_int(-4082);
    assert!(twelve < fourteen);
}

#[test]
fn test_rounding() {
    assert_eq!(rounding::round_away_from_zero(4, 8), 8);
    assert_eq!(rounding::round_away_from_zero(4, 7), 8);
    assert_eq!(rounding::round_away_from_zero(4, -7), -8);
}

#[test]
fn test_unique_ids() {
    let id1 = unique_ids::make_temporary();
    let id2 = unique_ids::make_label("L");
    let id3 = unique_ids::make_named_temporary("tmp");
    assert!(id1.starts_with("tmp."));
    assert!(id2.starts_with("L."));
    assert!(id3.starts_with("tmp."));
    assert_ne!(id1, id2);
    assert_ne!(id2, id3);
}

#[test]
fn test_disjoint_sets() {
    let mut ds = DisjointSets::new();
    assert!(ds.is_empty());
    ds.union(1, 2);
    assert!(!ds.is_empty());
    ds.union(2, 3);
    assert_eq!(ds.find(&1), 3);
}

#[test]
fn test_type_utils() {
    use Type::*;
    assert_eq!(type_utils::get_size(&Int), 4);
    assert_eq!(type_utils::get_alignment(&Double), 8);
    assert!(type_utils::is_signed(&Int));
    assert!(type_utils::is_pointer(&Pointer(Box::new(Int))));
    assert!(type_utils::is_integer(&ULong));
    assert!(type_utils::is_arithmetic(&Double));
    assert!(type_utils::is_array(&Array { elem_type: Box::new(Char), size: 3 }));
    assert!(type_utils::is_character(&SChar));
    assert!(type_utils::is_scalar(&Pointer(Box::new(Int))));
    assert!(!type_utils::is_complete(&Void));
    assert!(type_utils::is_complete_pointer(&Pointer(Box::new(Int))));
}
