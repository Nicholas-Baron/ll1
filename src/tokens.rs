use crate::identifier_map::Identifier;

#[derive(PartialEq, Eq, Debug)]
pub enum Token {
    Semi,
    Pipe,
    LBracket,
    RBracket,
    LParen,
    RParen,
    LCurly,
    RCurly,
    Start,
    Terminal,
    Empty,
    Identifier(Identifier),
}
