use crate::{
    grammar::{Grammar, GrammarBuilder, RuleOption},
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
    TerminalDeclaredTwice(String),
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
            ParserError::TerminalDeclaredTwice(id) => {
                f.write_fmt(format_args!("Terminal {} was declared twice", id))
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
    peeked_token: Option<Token>,
}

type ParserResult<T> = Result<T, ParserError>;

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            grammar_builder: GrammarBuilder::default(),
            peeked_token: None,
        }
    }

    fn consume_expected(&mut self, expected: Option<Token>) -> ParserResult<()> {
        let found = self.next_token();
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

    fn next_token(&mut self) -> Option<Token> {
        self.peeked_token.take().or_else(|| self.tokenizer.next())
    }

    fn peek_token(&mut self) -> Option<&Token> {
        if self.peeked_token.is_none() {
            self.peeked_token = self.next_token();
        }

        self.peeked_token.as_ref()
    }

    pub fn parse(mut self) -> ParserResult<Grammar> {
        while let Some(tok) = self.next_token() {
            if tok == Token::Start {
                let id = match self.next_token() {
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
                    match self.next_token() {
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
                    match self.grammar_builder.add_terminal(id.clone()) {
                        AddIdentifierStatus::Success => {}
                        AddIdentifierStatus::DuplicateTerminal => {
                            return Err(ParserError::TerminalDeclaredTwice(
                                self.tokenizer.text_for(id).to_string(),
                            ))
                        }
                        AddIdentifierStatus::DuplicateNonTerminal => todo!(),
                    }
                }
            } else if let Token::Identifier(lhs) = tok {
                self.consume_expected(Some(Token::Colon))?;
                let rhs = self.parse_rhs()?;

                use crate::grammar::AddIdentifierStatus;
                match self.grammar_builder.add_rule(lhs, rhs) {
                    AddIdentifierStatus::Success => {}
                    AddIdentifierStatus::DuplicateTerminal => todo!(),
                    AddIdentifierStatus::DuplicateNonTerminal => todo!(),
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

    fn parse_rhs(&mut self) -> ParserResult<RuleOption> {
        let mut current_sequence: Vec<RuleOption> = vec![];
        let mut pipe_set: Vec<Vec<_>> = vec![];

        loop {
            match self.peek_token() {
                Some(Token::Semi) => {
                    self.consume_expected(Some(Token::Semi)).unwrap();
                    break;
                }
                Some(Token::Pipe) => {
                    self.consume_expected(Some(Token::Pipe)).unwrap();
                    pipe_set.push(current_sequence);
                    current_sequence = vec![];
                }
                Some(Token::RParen | Token::RCurly | Token::RBracket) => break,
                Some(
                    Token::Empty
                    | Token::LParen
                    | Token::LCurly
                    | Token::LBracket
                    | Token::Identifier(_),
                ) => current_sequence.push(self.parse_rhs_item()?),
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        expected: vec![
                            Token::Semi,
                            Token::Pipe,
                            Token::Empty,
                            Token::LParen,
                            Token::LCurly,
                            Token::LBracket,
                        ],
                        could_be_id: true,
                        found: self.next_token(),
                    })
                }
            }
        }

        fn sequence_to_option(mut sequence: Vec<RuleOption>) -> RuleOption {
            match sequence.len() {
                0 => todo!(),
                1 => sequence.pop().expect("Pop is guarded by a length check"),
                _ => RuleOption::Sequence {
                    contents: sequence.into_boxed_slice(),
                },
            }
        }

        if pipe_set.is_empty() {
            Ok(sequence_to_option(current_sequence))
        } else {
            pipe_set.push(current_sequence);
            Ok(RuleOption::Alternates {
                contents: pipe_set.into_iter().map(sequence_to_option).collect(),
            })
        }
    }

    fn parse_rhs_item(&mut self) -> ParserResult<RuleOption> {
        match self
            .next_token()
            .ok_or_else(|| ParserError::UnexpectedToken {
                expected: vec![Token::Empty, Token::LCurly, Token::LParen, Token::LBracket],
                could_be_id: true,
                found: None,
            })? {
            Token::Semi => todo!(),
            Token::Pipe => todo!(),
            Token::Colon => todo!(),
            Token::LBracket => todo!(),
            Token::RBracket => todo!(),
            Token::LParen => todo!(),
            Token::RParen => todo!(),
            Token::LCurly => {
                let inner = self.parse_rhs()?;
                self.consume_expected(Some(Token::RCurly))?;
                Ok(RuleOption::Repetition(Box::new(inner)))
            }
            Token::RCurly => todo!(),
            Token::Start => todo!(),
            Token::Terminal => todo!(),
            Token::Empty => Ok(RuleOption::Empty),
            Token::Identifier(id) => Ok(RuleOption::Id(id)),
        }
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
                .terminal_symbols()
                .map(|id| grammar.text_for(id).to_string())
                .collect::<HashSet<_>>(),
            HashSet::from_iter(["t".to_string(), "u".to_string()])
        );
    }

    #[test]
    fn duplicate_terminals_error() {
        let grammar = Parser::new("terminal t t;".to_string().into()).parse();
        assert!(grammar.is_err());
    }

    #[test]
    fn parse_simple_rule() {
        let grammar = Parser::new("terminal t; s : t t ; start s;".to_string().into()).parse();
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(RuleOption::Sequence {
                contents: Box::new([RuleOption::Id(terminal.clone()), RuleOption::Id(terminal)])
            })
            .as_ref()
        );
    }

    #[test]
    fn parse_repetition_rule() {
        let grammar = Parser::new("terminal t; s : { t } ; start s;".to_string().into()).parse();
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(RuleOption::Repetition(Box::new(RuleOption::Id(terminal)))).as_ref()
        );
    }

    #[test]
    fn parse_pipe_rule() {
        let grammar = Parser::new("terminal t; s : t | t ; start s;".to_string().into()).parse();
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(RuleOption::Alternates {
                contents: Box::new([RuleOption::Id(terminal.clone()), RuleOption::Id(terminal)])
            })
            .as_ref()
        );
    }
}
