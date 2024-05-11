use crate::identifier_map::Identifier;
use crate::{
    grammar::{AddIdentifierStatus, Builder, Grammar, Rule},
    tokenizer::Tokenizer,
    tokens::Token,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    NoStartingId,
    DuplicateStarts {
        starts: [String; 2],
    },
    UnexpectedToken {
        expected: Box<[Token]>,
        could_be_id: bool,
        found: Option<Token>,
    },
    EmptyTerminalList,
    TerminalDeclaredTwice(String),
    NonterminalDeclaredTwice(String),
    ConflictingDeclaration(String),
    MissingEmptyMark,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NoStartingId => f.write_str("No starting id specified"),
            Error::EmptyTerminalList => {
                f.write_str("`terminal` needs to be followed by at least 1 identifier")
            }
            Error::DuplicateStarts { starts } => {
                f.write_fmt(format_args!("Found two starts: {starts:?}"))
            }
            Error::TerminalDeclaredTwice(id) => {
                f.write_fmt(format_args!("Terminal {id} was declared twice"))
            }
            Error::NonterminalDeclaredTwice(id) => {
                f.write_fmt(format_args!("Nonterminal {id} was declared twice"))
            }
            Error::ConflictingDeclaration(id) => f.write_fmt(format_args!(
                "Id {id} was given both a nonterminal and terminal definition",
            )),
            Error::UnexpectedToken {
                found,
                expected,
                could_be_id,
            } => match (expected.len(), could_be_id) {
                (0, true) => f.write_fmt(format_args!("Expected an identifier; Found {found:?}")),
                (0, false) => f.write_fmt(format_args!("Expected end of input; Found {found:?}")),
                (1, _) => f.write_fmt(format_args!(
                    "Expected {:?}{}; Found {found:?}",
                    expected[0],
                    could_be_id
                        .then_some("or an identifier")
                        .unwrap_or_default(),
                )),
                (_, _) => f.write_fmt(format_args!(
                    "Expected one of {:?}{}; Found {found:?}",
                    expected,
                    could_be_id
                        .then_some("or an identifier")
                        .unwrap_or_default(),
                )),
            },
            Error::MissingEmptyMark => f.write_str(
                "Use '%empty' to leave an explicit empty rule instead of leaving it blank",
            ),
        }
    }
}

pub struct Parser {
    tokenizer: Tokenizer,
    grammar_builder: Builder,
    peeked_token: Option<Token>,
    pseudo_rule_count: u32,
}

type ParserResult<T> = Result<T, Error>;

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            grammar_builder: Grammar::builder(),
            peeked_token: None,
            pseudo_rule_count: 0,
        }
    }

    fn add_pseudo_rule(&mut self, rule: Rule) -> Identifier {
        let pseudo_id = self
            .tokenizer
            .add_fake_id(format!("pseudo_rule_{}", self.pseudo_rule_count));
        self.pseudo_rule_count += 1;

        match self.grammar_builder.add_rule(pseudo_id.clone(), rule) {
            AddIdentifierStatus::Success => {}
            AddIdentifierStatus::DuplicateTerminal => todo!(),
            AddIdentifierStatus::DuplicateNonTerminal => todo!(),
        }

        pseudo_id
    }

    fn consume_expected(&mut self, expected: Option<Token>) -> ParserResult<()> {
        let found = self.next_token();
        if found == expected {
            Ok(())
        } else {
            Err(Error::UnexpectedToken {
                expected: expected.into_iter().collect(),
                could_be_id: false,
                found,
            })
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
                        return Err(Error::UnexpectedToken {
                            found: tok,
                            could_be_id: true,
                            expected: Box::new([]),
                        })
                    }
                };

                if let Some(old_start) = self.grammar_builder.start(id.clone()) {
                    return Err(Error::DuplicateStarts {
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
                            return Err(Error::UnexpectedToken {
                                expected: Box::new([Token::Semi]),
                                could_be_id: true,
                                found,
                            })
                        }
                    }
                }

                if new_terminals.is_empty() {
                    return Err(Error::EmptyTerminalList);
                }

                for id in new_terminals {
                    use crate::grammar::AddIdentifierStatus;
                    match self.grammar_builder.add_terminal(id.clone()) {
                        AddIdentifierStatus::Success => {}
                        AddIdentifierStatus::DuplicateTerminal => {
                            return Err(Error::TerminalDeclaredTwice(
                                self.tokenizer.text_for(id).to_string(),
                            ))
                        }
                        AddIdentifierStatus::DuplicateNonTerminal => {
                            return Err(Error::ConflictingDeclaration(
                                self.tokenizer.text_for(id).to_string(),
                            ))
                        }
                    }
                }
            } else if let Token::Identifier(lhs) = tok {
                self.consume_expected(Some(Token::Colon))?;
                let rhs = self.parse_rhs()?;

                match self.grammar_builder.add_rule(lhs.clone(), rhs) {
                    AddIdentifierStatus::Success => {}
                    AddIdentifierStatus::DuplicateTerminal => {
                        return Err(Error::ConflictingDeclaration(
                            self.tokenizer.text_for(lhs).to_string(),
                        ))
                    }
                    AddIdentifierStatus::DuplicateNonTerminal => {
                        return Err(Error::NonterminalDeclaredTwice(
                            self.tokenizer.text_for(lhs).to_string(),
                        ))
                    }
                }
            } else {
                return Err(Error::UnexpectedToken {
                    expected: Box::new([Token::Start, Token::Terminal]),
                    could_be_id: true,
                    found: Some(tok),
                });
            }
        }

        self.grammar_builder
            .build(self.tokenizer.identifier_map())
            .ok_or(Error::NoStartingId)
    }

    fn parse_rhs(&mut self) -> ParserResult<Rule> {
        let mut current_sequence: Vec<Rule> = vec![];
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
                    return Err(Error::UnexpectedToken {
                        expected: Box::new([
                            Token::Semi,
                            Token::Pipe,
                            Token::Empty,
                            Token::LParen,
                            Token::LCurly,
                            Token::LBracket,
                        ]),
                        could_be_id: true,
                        found: self.next_token(),
                    })
                }
            }
        }

        if pipe_set.is_empty() {
            sequence_to_option(current_sequence)
        } else {
            pipe_set.push(current_sequence);

            let mut contents = vec![];
            for item in pipe_set {
                contents.push(sequence_to_option(item)?);
            }

            Ok(Rule::Alternates {
                contents: contents.into(),
            })
        }
    }

    fn parse_rhs_item(&mut self) -> ParserResult<Rule> {
        match self.next_token().unwrap() {
            Token::Semi => todo!(),
            Token::Pipe => todo!(),
            Token::Colon => todo!(),
            Token::LBracket => {
                let inner = self.parse_rhs()?;
                self.consume_expected(Some(Token::RBracket))?;

                let pseudo_rule = Rule::Alternates {
                    contents: Box::new([Rule::Empty, inner]),
                };

                Ok(Rule::Id(self.add_pseudo_rule(pseudo_rule)))
            }
            Token::RBracket => todo!(),
            Token::LParen => {
                let contents = self.parse_rhs()?;
                self.consume_expected(Some(Token::RParen))?;

                Ok(contents)
            }
            Token::RParen => todo!(),
            Token::LCurly => {
                let inner = self.parse_rhs()?;
                self.consume_expected(Some(Token::RCurly))?;

                let pseudo_rule = Rule::Alternates {
                    contents: Box::new([Rule::Empty, inner.clone()]),
                };

                let pseudo_id = self.add_pseudo_rule(pseudo_rule);

                Ok(Rule::Sequence {
                    contents: vec![inner, Rule::Id(pseudo_id)].into(),
                })
            }
            Token::RCurly => todo!(),
            Token::Start => todo!(),
            Token::Terminal => todo!(),
            Token::Empty => Ok(Rule::Empty),
            Token::Identifier(id) => Ok(Rule::Id(id)),
        }
    }
}

fn sequence_to_option(mut sequence: Vec<Rule>) -> ParserResult<Rule> {
    let err = Error::MissingEmptyMark;

    match sequence.len() {
        0 => Err(err),
        1 => sequence.pop().ok_or(err),
        _ => Ok(Rule::Sequence {
            contents: sequence.into_boxed_slice(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    fn from_str(input: &str) -> ParserResult<Grammar> {
        Parser::new(input.to_string().into()).parse()
    }

    #[test]
    fn empty_grammar_errors() {
        let grammar = from_str("");
        assert_eq!(grammar, Err(Error::NoStartingId));
    }

    #[test]
    fn start_token_parses() {
        let grammar = from_str("start s;");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();
        assert_eq!(grammar.identifiers_used(), 1);
        assert_eq!(grammar.text_for(grammar.starting_id()), "s");
    }

    #[test]
    fn start_and_terminals_parse() {
        let grammar = from_str("terminal t u; start s;");
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
        let grammar = from_str("terminal t t;");
        assert!(grammar.is_err());
    }

    #[test]
    fn parse_simple_rule() {
        let grammar = from_str("terminal t; s : t t ; start s;");
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(Rule::Sequence {
                contents: Box::new([Rule::Id(terminal.clone()), Rule::Id(terminal)])
            })
            .as_ref()
        );
    }

    #[test]
    fn parse_repetition_rule() {
        let grammar = from_str("terminal t; s : { t } ; start s;");
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        let pseudo_nonterminal = grammar
            .nonterminal_symbols()
            .find(|id| grammar.text_for(id.clone()).starts_with("pseudo"))
            .unwrap();

        assert_eq!(
            grammar.starting_rule(),
            Some(Rule::Sequence {
                contents: Box::new([Rule::Id(terminal), Rule::Id(pseudo_nonterminal)])
            })
            .as_ref()
        );
    }

    #[test]
    fn parse_pipe_rule() {
        let grammar = from_str("terminal t; s : t | t ; start s;");
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let terminal = grammar.terminal_symbols().next().unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(Rule::Alternates {
                contents: Box::new([Rule::Id(terminal.clone()), Rule::Id(terminal)])
            })
            .as_ref()
        );
    }

    #[test]
    fn parse_optional_rule() {
        let grammar = from_str("terminal t; s : [ t ] ; start s;");
        eprintln!("{grammar:?}");
        assert!(grammar.is_ok());
        let grammar = grammar.unwrap();

        let pseudo_nonterminal = grammar
            .nonterminal_symbols()
            .find(|id| grammar.text_for(id.clone()).starts_with("pseudo"))
            .unwrap();
        assert_eq!(
            grammar.starting_rule(),
            Some(Rule::Id(pseudo_nonterminal)).as_ref()
        );
    }

    #[test]
    fn duplicate_nonterminals_error() {
        let grammar = Parser::new(
            "terminal t; s : [ t ] ; s : [ t ] ; start s;"
                .to_string()
                .into(),
        )
        .parse();
        assert!(grammar.is_err());
    }

    #[test]
    fn parse_parentheses() {
        let grammar_with = from_str("terminal t; s : ( t ) t ; start s;");
        let grammar_without = from_str("terminal t; s :  t  t; start s;");
        assert_eq!(grammar_with, grammar_without);
    }

    #[test]
    fn parse_empty_id() {
        let grammar = from_str("start s ; s : %empty ; ").unwrap();

        assert_eq!(grammar.starting_rule(), Some(Rule::Empty).as_ref());
    }

    #[test]
    fn already_terminal_error() {
        let grammar = from_str("terminal s ; start s; s : s ;");

        assert_eq!(grammar, Err(Error::ConflictingDeclaration("s".to_string())));
    }

    #[test]
    fn already_nonterminal_error() {
        let grammar = from_str("start s; s : s ; terminal s;");

        assert_eq!(grammar, Err(Error::ConflictingDeclaration("s".to_string())));
    }

    #[test]
    fn empty_terminal_list_error() {
        let grammar = from_str("terminal ; start s; s : s ;");

        assert_eq!(grammar, Err(Error::EmptyTerminalList));
    }

    #[test]
    fn duplicate_terminal_error() {
        let grammar = from_str("terminal s s ; start s; s : s ;");

        assert_eq!(grammar, Err(Error::TerminalDeclaredTwice("s".to_string())));
    }

    #[test]
    fn duplicate_start_error() {
        let grammar = from_str("terminal s ; start t; t : s ; start t; ");

        assert_eq!(
            grammar,
            Err(Error::DuplicateStarts {
                starts: ["t".to_string(), "t".to_string()]
            })
        );
    }
    #[test]
    fn unexpected_top_level_token_error() {
        let grammar = from_str(" | terminal s ; start s; s : s ;");

        assert_eq!(
            grammar,
            Err(Error::UnexpectedToken {
                expected: Box::new([Token::Start, Token::Terminal]),
                could_be_id: true,
                found: Some(Token::Pipe),
            })
        );
    }

    #[test]
    fn require_empty_ident() {
        let grammar = from_str("terminal t; start s ; s : | t ;");

        assert_eq!(grammar, Err(Error::MissingEmptyMark));
    }

    #[test]
    fn bad_start() {
        assert_eq!(
            from_str("start { s } ; s : %empty; "),
            Err(Error::UnexpectedToken {
                expected: Box::new([]),
                could_be_id: true,
                found: Some(Token::LCurly)
            })
        );
    }

    #[test]
    fn bad_terminal() {
        assert_eq!(
            from_str("terminal a | b ; s : a b ; start a;"),
            Err(Error::UnexpectedToken {
                expected: Box::new([Token::Semi]),
                could_be_id: true,
                found: Some(Token::Pipe),
            })
        );
    }

    #[test]
    fn bad_rhs() {
        assert_eq!(
            from_str("terminal t ; start s ; s : start ;"),
            Err(Error::UnexpectedToken {
                expected: Box::new([
                    Token::Semi,
                    Token::Pipe,
                    Token::Empty,
                    Token::LParen,
                    Token::LCurly,
                    Token::LBracket,
                ]),
                could_be_id: true,
                found: Some(Token::Start)
            })
        );
    }
}
