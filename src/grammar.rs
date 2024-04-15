use std::collections::{HashMap, HashSet};

use crate::identifier_map::{Identifier, IdentifierMap};

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Grammar {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, RuleOption>,
    starting_id: Identifier,
    identifier_map: IdentifierMap,
}

impl Grammar {
    fn builder() -> GrammarBuilder {
        GrammarBuilder::default()
    }
}

#[derive(Default)]
pub struct GrammarBuilder {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, RuleOption>,
    starting_id: Option<Identifier>,
    identifier_map: IdentifierMap,
}

impl GrammarBuilder {
    /// Returns `None` if there is no starting id
    pub fn build(self) -> Option<Grammar> {
        let Self {
            terminals,
            non_terminals,
            starting_id,
            identifier_map,
        } = self;

        Some(Grammar {
            terminals,
            non_terminals,
            starting_id: starting_id?,
            identifier_map,
        })
    }
}
