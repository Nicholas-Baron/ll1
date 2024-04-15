use crate::{
    grammar::{Grammar, GrammarBuilder},
    tokenizer::Tokenizer,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    NoStartingId,
}

pub struct Parser {
    tokenizer: Tokenizer,
    grammar_builder: GrammarBuilder,
}

type ParserResult<T> = Result<T, ParserError>;

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            grammar_builder: GrammarBuilder::default(),
        }
    }

    pub fn parse(self) -> ParserResult<Grammar> {
        self.grammar_builder
            .build()
            .ok_or(ParserError::NoStartingId)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_grammar_errors() {
        let grammar = Parser::new("".to_string().into()).parse();
        assert_eq!(grammar, Err(ParserError::NoStartingId));
    }
}
