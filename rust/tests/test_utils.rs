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
