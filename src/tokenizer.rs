use std::collections::VecDeque;

use crate::tokens::Token;

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

impl Tokenizer {
    fn remove_whitespace(&mut self) {
        while self.input.front().is_some_and(|c| c.is_whitespace()) {
            self.input.pop_front();
        }
    }

    fn remove_comments(&mut self) {
        loop {
            self.remove_whitespace();

            let Some(c) = self.input.front() else {
                return;
            };
            assert!(!c.is_whitespace());

            let found_comment = if *c == '/' {
                self.input.get(1).is_some_and(|c| *c == '/')
            } else {
                *c == '#'
            };

            if found_comment {
                while self.input.front().is_some_and(|c| *c != '\n') {
                    self.input.pop_front();
                }
            } else {
                return;
            }
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.remove_comments();

        match self.input.pop_front()? {
            // Single character tokens
            '[' => Some(Token::LBracket),
            ']' => Some(Token::RBracket),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LCurly),
            '}' => Some(Token::RCurly),
            '|' => Some(Token::Pipe),
            ';' => Some(Token::Semi),
            c => todo!("Unknown character: {c:?}"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn all_whitespace_is_removed() {
        let mut toks = Tokenizer::from("  \t   \n   ".to_string());
        toks.remove_whitespace();
        assert!(toks.input.is_empty());
    }

    #[test]
    fn all_comments_are_removed() {
        let mut toks = Tokenizer::from("   # test\n // test 2 \n".to_string());
        toks.remove_comments();
        assert!(toks.input.is_empty());
    }

    #[test]
    fn all_symbols_can_be_parsed() {
        let toks = Tokenizer::from("  [ ] { } | ; ( ) ".to_string());
        assert_eq!(
            Vec::from_iter(toks),
            vec![
                Token::LBracket,
                Token::RBracket,
                Token::LCurly,
                Token::RCurly,
                Token::Pipe,
                Token::Semi,
                Token::LParen,
                Token::RParen
            ]
        );
    }
}
