pub mod list_util {
    use std::cmp::Ordering;

    pub fn max_by<T, F>(cmp: F, l: &[T]) -> Option<&T>
    where
        F: Fn(&T, &T) -> Ordering,
    {
        l.iter().max_by(|a, b| cmp(a, b))
    }

    pub fn min_by<T, F>(cmp: F, l: &[T]) -> Option<&T>
    where
        F: Fn(&T, &T) -> Ordering,
    {
        l.iter().min_by(|a, b| cmp(a, b))
    }

    pub fn make_list<T: Clone>(len: usize, v: T) -> Vec<T> {
        vec![v; len]
    }

    pub fn last<T>(l: &[T]) -> Option<&T> {
        l.last()
    }

    pub fn take<T: Clone>(n: usize, l: &[T]) -> Vec<T> {
        l.iter().take(n).cloned().collect()
    }

    pub fn take_drop<T: Clone>(n: usize, l: &[T]) -> (Vec<T>, Vec<T>) {
        let split = n.min(l.len());
        let (left, right) = l.split_at(split);
        (left.to_vec(), right.to_vec())
    }
}

pub mod string_util {
    pub fn drop(n: usize, s: &str) -> String {
        s.chars().skip(n).collect()
    }

    pub fn chop_suffix(s: &str, n: usize) -> String {
        let len = s.chars().count();
        s.chars().take(len.saturating_sub(n)).collect()
    }

    pub fn of_list(l: &[char]) -> String {
        l.iter().collect()
    }

    pub fn is_alnum(c: char) -> bool {
        c.is_ascii_alphanumeric()
    }
}
