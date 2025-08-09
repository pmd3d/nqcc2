use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct DisjointSets<T: Eq + Hash + Clone> {
    parent: HashMap<T, T>,
}

impl<T: Eq + Hash + Clone> DisjointSets<T> {
    pub fn new() -> Self {
        Self { parent: HashMap::new() }
    }

    pub fn union(&mut self, x: T, y: T) {
        self.parent.insert(x, y);
    }

    pub fn find(&self, x: &T) -> T {
        match self.parent.get(x) {
            Some(mapped_to) => self.find(mapped_to),
            None => x.clone(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.parent.is_empty()
    }
}
