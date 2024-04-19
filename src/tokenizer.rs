use std::collections::VecDeque;

use crate::{
    identifier_map::{Identifier, IdentifierMap},
    tokens::Token,
};

#[derive(Debug)]
pub struct Tokenizer {
    input: VecDeque<char>,
    identifiers: IdentifierMap,
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
            identifiers: Default::default(),
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

    fn read_identifier(&mut self, first: char) -> Token {
        let mut word: String = [first].into_iter().collect();

        while let Some(c) = self.input.front() {
            if is_word_character(*c) {
                word.push(self.input.pop_front().unwrap());
            } else {
                break;
            }
        }

        match word.as_str() {
            "terminal" => Token::Terminal,
            "start" => Token::Start,
            "empty" => Token::Empty,
            _ => Token::Identifier(self.identifiers.add_identifier(word)),
        }
    }

    pub fn identifier_map(self) -> IdentifierMap {
        self.identifiers
    }

    pub fn text_for(&self, i: Identifier) -> &str {
        self.identifiers.text_for(i)
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.remove_comments();

        match self.input.pop_front()? {
            c if is_word_character(c) => Some(self.read_identifier(c)),
            '%' => {
                let mut expected = "empty".chars();

                while let (Some(c), Some(expect)) = (self.input.pop_front(), expected.next()) {
                    if c != expect {
                        panic!("'%' can only start a '%empty'");
                    }
                }

                if self.input.front().is_some_and(|c| is_word_character(*c)) {
                    panic!("'%' can only start a '%empty'");
                }

                Some(Token::Empty)
            }
            // Single character tokens
            '[' => Some(Token::LBracket),
            ']' => Some(Token::RBracket),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LCurly),
            '}' => Some(Token::RCurly),
            '|' => Some(Token::Pipe),
            ';' => Some(Token::Semi),
            ':' => Some(Token::Colon),
            c => todo!("Unknown character: {c:?}"),
        }
    }
}

/// Identifiers and Keywords (collectivly "words") can only contain
/// - Letters (aka Unicode alphabetic)
/// - Underscore '_'
fn is_word_character(c: char) -> bool {
    c.is_alphabetic() || c == '_'
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
        let toks = Tokenizer::from("  [ ] { } | ; ( ) : ".to_string());
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
                Token::RParen,
                Token::Colon
            ]
        );
    }

    #[test]
    fn all_keywords_are_parsed() {
        let toks = Tokenizer::from("  terminal start empty %empty".to_string());
        assert_eq!(
            Vec::from_iter(toks),
            [Token::Terminal, Token::Start, Token::Empty, Token::Empty]
        );
    }

    #[test]
    fn identifiers_are_parsed() {
        let toks = Tokenizer::from("  hello world".to_string());
        let tokens: Vec<Token> = toks.collect();

        assert!(matches!(
            tokens.as_slice(),
            [Token::Identifier(_), Token::Identifier(_)]
        ));
    }
}
