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

    fn first_set(&self, resolved_sets: &HashMap<Identifier, FirstSet>) -> FirstSet {
        match self {
            RuleOption::Empty => [FirstItem::Empty].into(),
            RuleOption::Id(id) => resolved_sets
                .get(id)
                .cloned()
                .unwrap_or([FirstItem::Id(id.clone())].into()),
            RuleOption::Sequence { contents } => {
                let mut idx = 0;
                let mut first_set = contents[idx].first_set(resolved_sets);
                while first_set.contains(&FirstItem::Empty) && contents.len() > idx {
                    first_set.remove(&FirstItem::Empty);
                    first_set.extend(contents[idx].first_set(resolved_sets));
                    idx += 1;
                }
                first_set
            }
            RuleOption::Alternates { contents } => {
                contents.iter().fold(Default::default(), |acc, item| {
                    acc.union(&item.first_set(resolved_sets)).cloned().collect()
                })
            }
            RuleOption::Optional(_) => todo!(),
            RuleOption::Repetition(_) => todo!(),
        }
    }

    fn local_follows(
        &self,
        nonterminal: &Identifier,
        first_sets: &HashMap<Identifier, FirstSet>,
    ) -> FollowSet {
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
                        local_follow_set.extend(next.first_set(first_sets).into_iter().map(
                            |item| match item {
                                FirstItem::Id(id) => FollowItem::Id(id),
                                FirstItem::Empty => FollowItem::EndOfInput,
                            },
                        ));
                    } else {
                        local_follow_set.insert(FollowItem::EndOfInput);
                    }
                }

                local_follow_set
            }
            RuleOption::Alternates { contents } => contents
                .iter()
                .map(|item| item.local_follows(nonterminal, first_sets))
                .fold(FollowSet::new(), |acc, item| {
                    acc.union(&item).cloned().collect()
                }),
            RuleOption::Optional(_) => todo!(),
            RuleOption::Repetition(_) => todo!(),
        }
    }

    fn first_first_conflict_set(&self, all_first_sets: &HashMap<Identifier, FirstSet>) -> FirstSet {
        match self {
            RuleOption::Empty => Default::default(),
            RuleOption::Id(_) => Default::default(),
            RuleOption::Sequence { contents } => contents
                .first()
                .map(|item| item.first_first_conflict_set(all_first_sets))
                .unwrap_or_default(),
            RuleOption::Alternates { contents } => {
                for (idx, lhs) in contents.iter().enumerate() {
                    for rhs in contents.iter().skip(idx + 1) {
                        let conflict_set: FirstSet = lhs
                            .first_set(all_first_sets)
                            .intersection(&rhs.first_set(all_first_sets))
                            .cloned()
                            .collect();
                        if !conflict_set.is_empty() {
                            return conflict_set;
                        }
                    }
                }

                Default::default()
            }
            RuleOption::Optional(_) => todo!(),
            RuleOption::Repetition(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod rule_tests {
    use super::*;

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
            .first_set(&Default::default()),
            [
                FirstItem::Empty,
                FirstItem::Id(id1.clone()),
                FirstItem::Id(id2.clone())
            ]
            .into()
        );

        assert_eq!(
            RuleOption::Sequence {
                contents: Box::new([RuleOption::Id(id1.clone()), RuleOption::Id(id2.clone())])
            }
            .first_set(&Default::default()),
            [FirstItem::Id(id1)].into()
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

    pub fn first_sets(&self) -> HashMap<Identifier, FirstSet> {
        let mut first_sets: HashMap<_, FirstSet> = HashMap::new();

        use std::collections::VecDeque;
        let mut to_process: VecDeque<_> = self.non_terminals.keys().cloned().collect();

        while let Some(non_term) = to_process.pop_front() {
            let mut first_set = HashSet::new();

            for symbol in self
                .non_terminals
                .get(&non_term)
                .expect("should be nonterminal")
                .first_set(&first_sets)
            {
                let FirstItem::Id(ref id) = symbol else {
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

                for (current, item) in self.non_terminals.iter().flat_map(|(current, rule)| {
                    rule.local_follows(&nonterminal, &first_sets)
                        .into_iter()
                        .map(move |item| (current, item))
                }) {
                    let FollowItem::Id(ref id) = item else {
                        local_follow_set
                            .extend(follow_sets.get(current).cloned().unwrap_or_default());
                        continue;
                    };

                    let Some(firsts) = first_sets.get(id) else {
                        local_follow_set.insert(item);
                        continue;
                    };

                    local_follow_set.extend(firsts.iter().flat_map(|item| match item {
                        FirstItem::Id(id) => [FollowItem::Id(id.clone())].into(),
                        FirstItem::Empty => follow_sets.get(current).cloned().unwrap_or_default(),
                    }));
                }

                follow_sets.insert(nonterminal, local_follow_set);
            }

            if old_follows == follow_sets {
                break;
            }
        }

        follow_sets
    }

    pub fn first_first_conflicts(&self) -> HashMap<Identifier, HashSet<FirstItem>> {
        let first_sets = self.first_sets();
        self.non_terminals
            .iter()
            .filter_map(|(id, rule)| {
                let conflict_set = rule.first_first_conflict_set(&first_sets);
                (!conflict_set.is_empty()).then(|| (id.clone(), conflict_set))
            })
            .collect()
    }

    pub fn first_follow_conflicts(&self) -> HashMap<Identifier, HashSet<Identifier>> {
        let follow_sets = self.follow_sets();
        self.first_sets()
            .into_iter()
            .filter(|(_, firsts)| firsts.contains(&FirstItem::Empty))
            .filter_map(|(nonterm, firsts)| {
                let follows = follow_sets
                    .get(&nonterm)
                    .expect("Nonterminal should have a follow set");

                let first_ids: HashSet<Identifier> = firsts
                    .into_iter()
                    .filter_map(|item| match item {
                        FirstItem::Empty => None,
                        FirstItem::Id(id) => Some(id),
                    })
                    .collect();

                let follow_ids: HashSet<Identifier> = follows
                    .iter()
                    .filter_map(|item| match item {
                        FollowItem::EndOfInput => None,
                        FollowItem::Id(id) => Some(id.clone()),
                    })
                    .collect();

                let overlap: HashSet<_> = first_ids.intersection(&follow_ids).cloned().collect();

                if overlap.is_empty() {
                    None
                } else {
                    Some((nonterm, overlap))
                }
            })
            .collect()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FirstItem {
    Empty,
    Id(Identifier),
}

impl FirstItem {
    pub fn printable<'gram>(&self, grammar: &'gram Grammar) -> &'gram str {
        match self {
            FirstItem::Empty => "empty",
            FirstItem::Id(id) => grammar.text_for(id.clone()),
        }
    }
}

pub type FirstSet = HashSet<FirstItem>;

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
