use std::collections::VecDeque;

#[derive(Debug)]
pub struct Tokenizer {
    input: VecDeque<char>,
}

impl From<String> for Tokenizer {
    fn from(value: String) -> Self {
        Self::from_iter(value.chars())
    }
}

impl FromIterator<char> for Tokenizer {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self {
            input: iter.into_iter().collect(),
        }
    }
}
