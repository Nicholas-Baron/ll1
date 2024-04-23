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

impl RuleOption {
    fn identifiers(&self) -> HashSet<Identifier> {
        match self {
            RuleOption::Empty => [].into(),
            RuleOption::Id(id) => [id.clone()].into(),
            RuleOption::Alternates { contents } | RuleOption::Sequence { contents } => contents
                .iter()
                .flat_map(|item| item.identifiers())
                .collect(),
            RuleOption::Optional(inner) | RuleOption::Repetition(inner) => inner.identifiers(),
        }
    }

    fn first_set(&self) -> HashSet<Option<Identifier>> {
        match self {
            RuleOption::Empty => [None].into(),
            RuleOption::Id(id) => [Some(id.clone())].into(),
            RuleOption::Sequence { contents } => contents[0].first_set(),
            RuleOption::Alternates { contents } => contents
                .iter()
                .map(RuleOption::first_set)
                .fold(Default::default(), |acc, item| {
                    acc.union(&item).cloned().collect()
                }),
            RuleOption::Optional(_) => todo!(),
            RuleOption::Repetition(_) => todo!(),
        }
    }

    fn local_follows(&self, nonterminal: &Identifier, current_rule_lhs: &Identifier) -> FollowSet {
        match self {
            RuleOption::Empty => [].into(),
            RuleOption::Id(id) => (id == nonterminal)
                .then_some([FollowItem::EndOfInput])
                .map(Into::into)
                .unwrap_or_default(),
            RuleOption::Sequence { contents } => {
                let mut local_follow_set = FollowSet::new();
                let mut slice = &contents[..];
                let id_rule = RuleOption::Id(nonterminal.clone());

                while let Some(idx) = slice.iter().position(|item| item == &id_rule) {
                    slice = &slice[idx + 1..];

                    if let Some(next) = slice.first() {
                        for item in next.first_set() {
                            match item {
                                Some(id) => {
                                    local_follow_set.insert(FollowItem::Id(id));
                                }
                                None => {
                                    local_follow_set.insert(FollowItem::EndOfInput);
                                }
                            }
                        }
                    } else {
                        local_follow_set.insert(FollowItem::EndOfInput);
                    }
                }

                local_follow_set
            }
            RuleOption::Alternates { contents } => contents
                .iter()
                .map(|item| item.local_follows(nonterminal, current_rule_lhs))
                .fold(FollowSet::new(), |acc, item| {
                    acc.union(&item).cloned().collect()
                }),
            RuleOption::Optional(_) => todo!(),
            RuleOption::Repetition(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod rule_tests {
    use super::Identifier;
    use super::RuleOption;

    #[test]
    fn identifiers_used() {
        assert!(RuleOption::Empty.identifiers().is_empty());
        let id = Identifier::mock_id(0);
        let id_rule = RuleOption::Id(id.clone());
        assert_eq!(id_rule.identifiers(), [id.clone()].into());

        assert_eq!(
            RuleOption::Sequence {
                contents: Box::new([RuleOption::Id(id.clone()), RuleOption::Id(id.clone())])
            }
            .identifiers(),
            id_rule.identifiers()
        );

        assert_eq!(
            RuleOption::Optional(Box::new(RuleOption::Id(id.clone()))).identifiers(),
            RuleOption::Id(id.clone()).identifiers()
        );
    }

    #[test]
    fn first_set() {
        let id1 = Identifier::mock_id(1);
        let id2 = Identifier::mock_id(2);
        assert_eq!(
            RuleOption::Alternates {
                contents: Box::new([
                    RuleOption::Empty,
                    RuleOption::Id(id1.clone()),
                    RuleOption::Id(id2.clone())
                ])
            }
            .first_set(),
            [None, Some(id1.clone()), Some(id2.clone())].into()
        );

        assert_eq!(
            RuleOption::Sequence {
                contents: Box::new([RuleOption::Id(id1.clone()), RuleOption::Id(id2.clone())])
            }
            .first_set(),
            [Some(id1)].into()
        );
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Grammar {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, RuleOption>,
    starting_id: Identifier,
    identifier_map: IdentifierMap,
}

impl Grammar {
    pub fn builder() -> GrammarBuilder {
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

    pub fn terminal_symbols(&self) -> impl Iterator<Item = Identifier> + '_ {
        self.terminals.iter().cloned()
    }

    pub fn nonterminal_symbols(&self) -> impl Iterator<Item = Identifier> + '_ {
        self.non_terminals.keys().cloned()
    }

    pub fn undeclared_symbols(&self) -> impl Iterator<Item = Identifier> + '_ {
        let all_components: HashSet<_> = self
            .non_terminals
            .values()
            .flat_map(|opt| opt.identifiers())
            .collect();

        all_components
            .into_iter()
            .filter(|id| !self.non_terminals.contains_key(id) && !self.terminals.contains(id))
    }

    pub fn reachable_symbols(&self) -> HashSet<Identifier> {
        let mut reachable_symbols: HashSet<Identifier> = [self.starting_id.clone()].into();

        loop {
            let mut extended_reachable = HashSet::new();

            for item in &reachable_symbols {
                extended_reachable.insert(item.clone());
                if let Some(rule) = self.non_terminals.get(item) {
                    extended_reachable.extend(rule.identifiers());
                }
            }

            if extended_reachable == reachable_symbols {
                break;
            } else {
                reachable_symbols = extended_reachable;
            }
        }

        reachable_symbols
    }

    pub fn first_sets(&self) -> HashMap<Identifier, HashSet<Option<Identifier>>> {
        let mut first_sets: HashMap<_, HashSet<_>> = HashMap::new();

        use std::collections::VecDeque;
        let mut to_process: VecDeque<_> = self.non_terminals.keys().cloned().collect();

        while let Some(non_term) = to_process.pop_front() {
            let mut first_set = HashSet::new();

            for symbol in self
                .non_terminals
                .get(&non_term)
                .expect("should be nonterminal")
                .first_set()
            {
                let Some(ref id) = symbol else {
                    first_set.insert(symbol);
                    continue;
                };

                if id == &non_term {
                    continue;
                }

                if to_process.contains(id) {
                    to_process.push_back(non_term.clone());
                    first_set.clear();
                    break;
                }

                if let Some(deeper_firsts) = first_sets.get(id) {
                    first_set.extend(deeper_firsts.iter().cloned());
                } else {
                    first_set.insert(symbol);
                }
            }

            // An empty set means we need to compute later
            if !first_set.is_empty() {
                first_sets.insert(non_term, first_set);
            }
        }

        first_sets
    }

    pub fn follow_sets(&self) -> HashMap<Identifier, FollowSet> {
        let mut follow_sets: HashMap<_, FollowSet> =
            [(self.starting_id.clone(), [FollowItem::EndOfInput].into())].into();
        let first_sets = self.first_sets();

        loop {
            let old_follows = follow_sets.clone();

            for nonterminal in self.nonterminal_symbols() {
                let mut local_follow_set = follow_sets.remove(&nonterminal).unwrap_or_default();

                for (current, rule) in &self.non_terminals {
                    for item in rule.local_follows(&nonterminal, current) {
                        match item {
                            FollowItem::EndOfInput => {
                                if let Some(set) = follow_sets.get(current) {
                                    local_follow_set.extend(set.iter().cloned());
                                }
                            }
                            FollowItem::Id(ref id) => {
                                if let Some(firsts) = first_sets.get(&id) {
                                    for item in firsts {
                                        match item {
                                            Some(id) => {
                                                local_follow_set.insert(FollowItem::Id(id.clone()));
                                            }
                                            None => {
                                                if let Some(set) = follow_sets.get(current) {
                                                    local_follow_set.extend(set.iter().cloned());
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    local_follow_set.insert(item);
                                }
                            }
                        }
                    }
                }

                follow_sets.insert(nonterminal, local_follow_set);
            }

            if old_follows == follow_sets {
                break;
            }
        }

        follow_sets
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum FollowItem {
    EndOfInput,
    Id(Identifier),
}

pub type FollowSet = HashSet<FollowItem>;

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
}

impl GrammarBuilder {
    pub fn start(&mut self, new_start: Identifier) -> Option<Identifier> {
        let old_start = self.starting_id.take();
        self.starting_id = Some(new_start);
        old_start
    }

    /// Returns `None` if there is no starting id
    pub fn build(self, identifier_map: IdentifierMap) -> Option<Grammar> {
        let Self {
            terminals,
            non_terminals,
            starting_id,
        } = self;

        Some(Grammar {
            terminals,
            non_terminals,
            starting_id: starting_id?,
            identifier_map,
        })
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
