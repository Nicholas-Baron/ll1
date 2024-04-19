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

    pub fn starting_rule(&self) -> Option<&RuleOption> {
        self.non_terminals.get(&self.starting_id)
    }

    pub fn identifiers_used(&self) -> usize {
        self.identifier_map.len()
    }

    pub fn starting_id(&self) -> Identifier {
        self.starting_id.clone()
    }

    pub fn text_for(&self, id: Identifier) -> &str {
        self.identifier_map.text_for(id)
    }

    pub fn terminals(&self) -> impl Iterator<Item = Identifier> + '_ {
        self.terminals.iter().cloned()
    }
}

pub enum AddIdentifierStatus {
    Success,
    DuplicateTerminal,
    DuplicateNonTerminal,
}

#[derive(Default)]
pub struct GrammarBuilder {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, RuleOption>,
    starting_id: Option<Identifier>,
    identifier_map: IdentifierMap,
}

impl GrammarBuilder {
    pub fn start(&mut self, new_start: Identifier) -> Option<Identifier> {
        let old_start = self.starting_id.take();
        self.starting_id = Some(new_start);
        old_start
    }

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

    pub fn text_for(&self, i: Identifier) -> &str {
        self.identifier_map.text_for(i)
    }

    pub fn identifier_map(&mut self, identifier_map: IdentifierMap) {
        self.identifier_map = identifier_map;
    }

    pub fn add_terminal(&mut self, id: Identifier) -> AddIdentifierStatus {
        if self.non_terminals.contains_key(&id) {
            AddIdentifierStatus::DuplicateNonTerminal
        } else if self.terminals.insert(id) {
            AddIdentifierStatus::Success
        } else {
            AddIdentifierStatus::DuplicateTerminal
        }
    }

    pub fn add_rule(&mut self, lhs: Identifier, rhs: RuleOption) -> AddIdentifierStatus {
        if self.terminals.contains(&lhs) {
            AddIdentifierStatus::DuplicateTerminal
        } else {
            use std::collections::hash_map::Entry;
            match self.non_terminals.entry(lhs) {
                Entry::Occupied(mut o) => {
                    o.insert(rhs);
                    AddIdentifierStatus::DuplicateNonTerminal
                }
                Entry::Vacant(v) => {
                    v.insert(rhs);
                    AddIdentifierStatus::Success
                }
            }
        }
    }
}
