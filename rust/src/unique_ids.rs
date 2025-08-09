use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn make_temporary() -> String {
    let n = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("tmp.{}", n)
}

pub fn make_label(prefix: &str) -> String {
    let n = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("{}.{}", prefix, n)
}

pub fn make_named_temporary(prefix: &str) -> String {
    make_label(prefix)
}
