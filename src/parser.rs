use crate::{
    grammar::{Grammar, GrammarBuilder},
    tokenizer::Tokenizer,
    tokens::Token,
};

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    NoStartingId,
    DuplicateStarts {
        starts: [String; 2],
    },
    UnexpectedToken {
        expected: Vec<Token>,
        could_be_id: bool,
        found: Option<Token>,
    },
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

    fn consume_expected(&mut self, expected: Option<Token>) -> ParserResult<()> {
        let found = self.tokenizer.next();
        if found != expected {
            Err(ParserError::UnexpectedToken {
                expected: expected.into_iter().collect(),
                could_be_id: false,
                found,
            })
        } else {
            Ok(())
        }
    }

    pub fn parse(mut self) -> ParserResult<Grammar> {
        while let Some(tok) = self.tokenizer.next() {
            if tok == Token::Start {
                let id = match self.tokenizer.next() {
                    Some(Token::Identifier(id)) => id,
                    tok => {
                        return Err(ParserError::UnexpectedToken {
                            found: tok,
                            could_be_id: true,
                            expected: vec![],
                        })
                    }
                };

                if let Some(old_start) = self.grammar_builder.start(id.clone()) {
                    return Err(ParserError::DuplicateStarts {
                        starts: [id, old_start].map(|i| self.tokenizer.text_for(i).to_string()),
                    });
                }

                self.consume_expected(Some(Token::Semi))?;
            } else {
                return Err(ParserError::UnexpectedToken {
                    expected: vec![Token::Start],
                    could_be_id: true,
                    found: Some(tok),
                });
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
