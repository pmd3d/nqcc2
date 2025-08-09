use crate::tokens::Token;
use std::collections::VecDeque;

pub struct TokenStream {
    queue: VecDeque<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { queue: VecDeque::from(tokens) }
    }

    pub fn take_token(&mut self) -> Option<Token> {
        self.queue.pop_front()
    }

    pub fn peek(&self) -> Option<&Token> {
        self.queue.front()
    }

    pub fn npeek(&self, n: usize) -> Vec<Token> {
        self.queue.iter().take(n).cloned().collect()
    }

    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }
}
