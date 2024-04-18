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
    EmptyTerminalList,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::NoStartingId => f.write_str("No starting id specified"),
            ParserError::EmptyTerminalList => {
                f.write_str("`terminal` needs to be followed by at least 1 identifier")
            }
            ParserError::DuplicateStarts { starts } => {
                f.write_fmt(format_args!("Found two starts: {:?}", starts))
            }
            ParserError::UnexpectedToken {
                found,
                expected,
                could_be_id,
            } => match (expected.len(), could_be_id) {
                (0, true) => f.write_fmt(format_args!("Expected an identifier; Found {:?}", found)),
                (0, false) => f.write_fmt(format_args!("Expected end of input; Found {:?}", found)),
                (1, _) => f.write_fmt(format_args!(
                    "Expected {:?}{}; Found {:?}",
                    expected[0],
                    could_be_id
                        .then_some("or an identifier")
                        .unwrap_or_default(),
                    found
                )),
                (_, _) => f.write_fmt(format_args!(
                    "Expected one of {:?}{}; Found {:?}",
                    expected,
                    could_be_id
                        .then_some("or an identifier")
                        .unwrap_or_default(),
                    found
                )),
            },
        }
    }
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
            } else if tok == Token::Terminal {
                let mut new_terminals = vec![];
                loop {
                    match self.tokenizer.next() {
                        Some(Token::Semi) => break,
                        Some(Token::Identifier(id)) => new_terminals.push(id),
                        found => {
                            return Err(ParserError::UnexpectedToken {
                                expected: vec![Token::Semi],
                                could_be_id: true,
                                found,
                            })
                        }
                    }
                }

                if new_terminals.is_empty() {
                    return Err(ParserError::EmptyTerminalList);
                }

                for id in new_terminals {
                    use crate::grammar::AddIdentifierStatus;
                    match self.grammar_builder.add_terminal(id) {
                        AddIdentifierStatus::Success => {}
                        AddIdentifierStatus::DuplicateTerminal => todo!(),
                        AddIdentifierStatus::DuplicateNonTerminal => todo!(),
                    }
                }
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
    use std::collections::HashSet;

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

    #[test]
    fn start_and_terminals_parse() {
        let grammar = Parser::new("terminal t u; start s;".to_string().into()).parse();
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();
        assert_eq!(grammar.identifiers_used(), 3);
        assert_eq!(grammar.text_for(grammar.starting_id()), "s");
        assert_eq!(
            grammar
                .terminals()
                .map(|id| grammar.text_for(id).to_string())
                .collect::<HashSet<_>>(),
            HashSet::from_iter(["t".to_string(), "u".to_string()])
        );
    }
}
