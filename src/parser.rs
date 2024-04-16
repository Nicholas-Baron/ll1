use crate::{
    grammar::{Grammar, GrammarBuilder},
    tokenizer::Tokenizer,
    tokens::Token,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    NoStartingId,
    ExpectedId { found: Token },
    UnexpectedEnd,
    DuplicateStarts { starts: [String; 2] },
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

    pub fn parse(mut self) -> ParserResult<Grammar> {
        while let Some(tok) = self.tokenizer.next() {
            if tok == Token::Start {
                let id = match self.tokenizer.next() {
                    None => return Err(ParserError::UnexpectedEnd),
                    Some(Token::Identifier(id)) => id,
                    Some(tok) => return Err(ParserError::ExpectedId { found: tok }),
                };

                if let Some(old_start) = self.grammar_builder.start(id.clone()) {
                    return Err(ParserError::DuplicateStarts {
                        starts: [id, old_start].map(|i| self.tokenizer.text_for(i).to_string()),
                    });
                }
            }
        }

        self.grammar_builder
            .identifier_map(self.tokenizer.identifier_map());

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

    #[test]
    fn start_token_parses() {
        let grammar = Parser::new("start s;".to_string().into()).parse();
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();
        assert_eq!(grammar.identifiers_used(), 1);
        assert_eq!(grammar.text_for(grammar.starting_id()), "s");
    }
}