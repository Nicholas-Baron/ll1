use std::collections::{HashMap, HashSet};

#[derive(PartialEq, Eq, Hash)]
pub struct Identifier(usize);

pub enum RuleOption {
    Empty,
    Id(Identifier),
    /// `a b c`
    Sequence {
        contents: Box<[RuleOption]>,
    },
    /// `a | b | c`
    Alternates {
        contents: Box<[RuleOption]>,
    },
    /// `[ a ]`
    Optional(Box<RuleOption>),
    /// `{ a }`
    Repetition(Box<RuleOption>),
}

pub struct Grammar {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, RuleOption>,
    starting_id: Identifier,
    string_map: HashMap<Identifier, String>,
}
