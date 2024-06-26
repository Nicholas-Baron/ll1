use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::identifier_map::{Identifier, IdentifierMap};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Rule {
    Empty,
    Id(Identifier),
    /// `a b c`
    Sequence {
        contents: Box<[Rule]>,
    },
    /// `a | b | c`
    Alternates {
        contents: Box<[Rule]>,
    },
}

impl Rule {
    fn identifiers(&self) -> HashSet<Identifier> {
        match self {
            Rule::Empty => [].into(),
            Rule::Id(id) => [id.clone()].into(),
            Rule::Alternates { contents } | Rule::Sequence { contents } => {
                contents.iter().flat_map(Rule::identifiers).collect()
            }
        }
    }

    fn first_set(&self, resolved_sets: &HashMap<Identifier, FirstSet>) -> FirstSet {
        match self {
            Rule::Empty => [FirstItem::Empty].into(),
            Rule::Id(id) => resolved_sets
                .get(id)
                .cloned()
                .unwrap_or([FirstItem::Id(id.clone())].into()),
            Rule::Sequence { contents } => {
                let mut idx = 0;
                let mut first_set = contents[idx].first_set(resolved_sets);
                while first_set.contains(&FirstItem::Empty) && contents.len() > idx {
                    first_set.remove(&FirstItem::Empty);
                    first_set.extend(contents[idx].first_set(resolved_sets));
                    idx += 1;
                }
                first_set
            }
            Rule::Alternates { contents } => {
                contents.iter().fold(Default::default(), |acc, item| {
                    acc.union(&item.first_set(resolved_sets)).cloned().collect()
                })
            }
        }
    }

    fn local_follows(
        &self,
        nonterminal: &Identifier,
        first_sets: &HashMap<Identifier, FirstSet>,
    ) -> FollowSet {
        match self {
            Rule::Empty => [].into(),
            Rule::Id(id) => (id == nonterminal)
                .then_some([FollowItem::EndOfInput])
                .map(Into::into)
                .unwrap_or_default(),
            Rule::Sequence { contents } => {
                let mut local_follow_set = FollowSet::new();
                let mut slice = &contents[..];

                while let Some(idx) = slice
                    .iter()
                    .position(|item| item.identifiers().contains(nonterminal))
                {
                    slice = &slice[idx + 1..];

                    for next in slice {
                        local_follow_set.remove(&FollowItem::EndOfInput);

                        local_follow_set.extend(next.first_set(first_sets).into_iter().map(
                            |item| match item {
                                FirstItem::Id(id) => FollowItem::Id(id),
                                FirstItem::Empty => FollowItem::EndOfInput,
                            },
                        ));

                        if !local_follow_set.contains(&FollowItem::EndOfInput) {
                            break;
                        }
                    }

                    if slice.is_empty() {
                        local_follow_set.insert(FollowItem::EndOfInput);
                    }
                }

                local_follow_set
            }
            Rule::Alternates { contents } => contents
                .iter()
                .map(|item| item.local_follows(nonterminal, first_sets))
                .fold(FollowSet::new(), |acc, item| {
                    acc.union(&item).cloned().collect()
                }),
        }
    }

    fn first_first_conflict_set(&self, all_first_sets: &HashMap<Identifier, FirstSet>) -> FirstSet {
        match self {
            Rule::Empty | Rule::Id(_) => Default::default(),
            Rule::Sequence { contents } => contents
                .first()
                .map(|item| item.first_first_conflict_set(all_first_sets))
                .unwrap_or_default(),
            Rule::Alternates { contents } => {
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
        }
    }

    fn printable(&self, id_map: &IdentifierMap) -> String {
        match self {
            Rule::Empty => "%empty".to_owned(),
            Rule::Id(id) => id_map.text_for(id.clone()).to_owned(),
            Rule::Sequence { contents } => contents
                .iter()
                .map(|item| item.printable(id_map))
                .collect::<Vec<_>>()
                .join(" "),
            Rule::Alternates { contents } => {
                format!(
                    "({})",
                    contents
                        .iter()
                        .map(|item| item.printable(id_map))
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
            }
        }
    }
}

#[cfg(test)]
mod rule_tests {
    use super::*;

    #[test]
    fn identifiers_used() {
        assert!(Rule::Empty.identifiers().is_empty());
        let id = Identifier::mock_id(0);
        let id_rule = Rule::Id(id.clone());
        assert_eq!(id_rule.identifiers(), [id.clone()].into());

        assert_eq!(
            Rule::Sequence {
                contents: Box::new([Rule::Id(id.clone()), Rule::Id(id.clone())])
            }
            .identifiers(),
            id_rule.identifiers()
        );
    }

    #[test]
    fn first_set() {
        let id1 = Identifier::mock_id(1);
        let id2 = Identifier::mock_id(2);
        assert_eq!(
            Rule::Alternates {
                contents: Box::new([Rule::Empty, Rule::Id(id1.clone()), Rule::Id(id2.clone())])
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
            Rule::Sequence {
                contents: Box::new([Rule::Id(id1.clone()), Rule::Id(id2.clone())])
            }
            .first_set(&Default::default()),
            [FirstItem::Id(id1)].into()
        );
    }

    #[test]
    fn printable() {
        let mut id_map = IdentifierMap::default();
        let only_id = id_map.add_identifier("term".to_owned());

        assert_eq!(Rule::Empty.printable(&id_map), "%empty");

        assert_eq!(
            Rule::Id(only_id.clone()).printable(&id_map),
            id_map.text_for(only_id.clone())
        );

        assert_eq!(
            Rule::Sequence {
                contents: Box::new([Rule::Id(only_id.clone()), Rule::Id(only_id.clone())])
            }
            .printable(&id_map),
            "term term"
        );

        assert_eq!(
            Rule::Alternates {
                contents: Box::new([Rule::Id(only_id.clone()), Rule::Id(only_id)])
            }
            .printable(&id_map),
            "(term | term)"
        );
    }

    #[test]
    fn first_first_conflict() {
        let mut id_map = IdentifierMap::default();
        let ident = id_map.add_identifier("identifier".to_owned());
        let term = id_map.add_identifier("terminal".to_owned());

        let all_first_sets: HashMap<_, _> =
            [(ident.clone(), [FirstItem::Id(term.clone())].into())].into();

        assert_eq!(
            Rule::Empty.first_first_conflict_set(&all_first_sets),
            Default::default()
        );

        assert_eq!(
            Rule::Id(ident.clone()).first_first_conflict_set(&all_first_sets),
            Default::default()
        );

        assert_eq!(
            Rule::Sequence {
                contents: Box::new([Rule::Id(ident.clone()), Rule::Id(ident.clone())])
            }
            .first_first_conflict_set(&all_first_sets),
            Default::default()
        );

        assert_eq!(
            Rule::Alternates {
                contents: Box::new([Rule::Id(ident.clone()), Rule::Empty]),
            }
            .first_first_conflict_set(&all_first_sets),
            Default::default()
        );

        assert_eq!(
            Rule::Alternates {
                contents: Box::new([Rule::Id(ident), Rule::Id(term.clone())]),
            }
            .first_first_conflict_set(&all_first_sets),
            [FirstItem::Id(term)].into()
        );
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Grammar {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, Rule>,
    starting_id: Identifier,
    identifier_map: IdentifierMap,
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut rule_names: Vec<(Identifier, &str)> = self
            .nonterminal_symbols()
            .map(|sym| (sym.clone(), self.identifier_map.text_for(sym)))
            .collect();

        rule_names.sort_unstable_by_key(|(_, name)| name.to_owned());

        f.write_fmt(format_args!(
            "Starting rule: {}\n",
            rule_names
                .iter()
                .find_map(|(id, name)| (*id == self.starting_id).then_some(name))
                .unwrap()
        ))?;

        for (id, name) in rule_names {
            let rule = self.non_terminals.get(&id).unwrap();

            let printable_rule = rule.printable(&self.identifier_map);

            f.write_fmt(format_args!("{name} : {printable_rule}\n"))?;
        }

        Ok(())
    }
}

impl Grammar {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn starting_rule(&self) -> Option<&Rule> {
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
            .flat_map(Rule::identifiers)
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
            }
            reachable_symbols = extended_reachable;
        }

        reachable_symbols
    }

    pub fn first_sets(&self) -> HashMap<Identifier, FirstSet> {
        use std::collections::VecDeque;

        let mut first_sets: HashMap<_, FirstSet> = HashMap::new();

        let mut to_process: VecDeque<_> = self.non_terminals.keys().cloned().collect();
        to_process.make_contiguous().sort();

        let mut old_size = to_process.len();
        let mut size = to_process.len();
        let mut in_deadlock = false;

        while let Some(non_term) = to_process.pop_front() {
            let mut first_set = HashSet::new();

            size -= 1;
            if size == 0 {
                size = to_process.len();
                if size < old_size {
                    old_size = size;
                } else {
                    in_deadlock = true;
                }
            }
            if in_deadlock {
                to_process.push_back(non_term);

                let (non_term, first_set) = break_deadlock_in(
                    to_process.make_contiguous(),
                    self.starting_id.clone(),
                    &self.non_terminals,
                    &first_sets,
                );

                to_process.retain(|id| id != &non_term);
                first_sets.insert(non_term, first_set);
                in_deadlock = false;
                continue;
            }

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
                    .filter_map(FirstItem::into_identifier)
                    .collect();

                let follow_ids: HashSet<Identifier> = follows
                    .iter()
                    .filter_map(|item| item.identifier().cloned())
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

fn break_deadlock_in(
    deadlock_set: &[Identifier],
    start: Identifier,
    non_terminals: &HashMap<Identifier, Rule>,
    first_sets: &HashMap<Identifier, FirstSet>,
) -> (Identifier, FirstSet) {
    let mut visited = HashSet::new();
    let mut non_term_in_ring = start;

    // there could be a ring with tail

    loop {
        // start at the beginning
        visited.insert(non_term_in_ring.clone());

        non_term_in_ring = non_terminals
            .get(&non_term_in_ring)
            .unwrap()
            .first_set(first_sets)
            .into_iter()
            .filter_map(|item: FirstItem| {
                item.as_identifier()
                    .and_then(|id| deadlock_set.contains(id).then_some(id).cloned())
            })
            .next()
            .unwrap();

        if visited.contains(&non_term_in_ring) {
            break;
        }
    }

    // found a nonterminal in the ring
    let mut first_set: FirstSet = non_terminals
        .get(&non_term_in_ring)
        .unwrap()
        .first_set(first_sets);

    let mut found_self = false;
    while !found_self {
        let nonterms: Vec<_> = first_set
            .iter()
            .filter_map(|item| {
                item.as_identifier()
                    .and_then(|id| non_terminals.contains_key(id).then_some(id).cloned())
            })
            .collect();

        first_set.retain(|item| {
            item.as_identifier()
                .map_or(true, |id| !nonterms.contains(id))
        });

        for nonterm in nonterms {
            if nonterm == non_term_in_ring {
                found_self = true;
                continue;
            }

            first_set.extend(
                non_terminals
                    .get(&nonterm)
                    .map_or_else(FirstSet::default, |item| item.first_set(first_sets)),
            );
        }
    }

    (non_term_in_ring, first_set)
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum FirstItem {
    Empty,
    Id(Identifier),
}

impl FirstItem {
    pub fn printable<'gram>(&self, grammar: &'gram Grammar) -> &'gram str {
        self.as_identifier()
            .map_or("%empty", |id| grammar.text_for(id.clone()))
    }

    fn into_identifier(self) -> Option<Identifier> {
        match self {
            FirstItem::Empty => None,
            FirstItem::Id(id) => Some(id),
        }
    }

    fn as_identifier(&self) -> Option<&Identifier> {
        match self {
            FirstItem::Empty => None,
            FirstItem::Id(id) => Some(id),
        }
    }
}

pub type FirstSet = HashSet<FirstItem>;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum FollowItem {
    EndOfInput,
    Id(Identifier),
}

impl FollowItem {
    pub fn printable<'gram>(&self, grammar: &'gram Grammar) -> &'gram str {
        self.identifier()
            .map_or("$", |id| grammar.text_for(id.clone()))
    }

    fn identifier(&self) -> Option<&Identifier> {
        match self {
            FollowItem::EndOfInput => None,
            FollowItem::Id(id) => Some(id),
        }
    }
}

pub type FollowSet = HashSet<FollowItem>;

pub enum AddIdentifierStatus {
    Success,
    DuplicateTerminal,
    DuplicateNonTerminal,
}

#[derive(Default)]
pub struct Builder {
    terminals: HashSet<Identifier>,
    non_terminals: HashMap<Identifier, Rule>,
    starting_id: Option<Identifier>,
}

impl Builder {
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

    pub fn add_rule(&mut self, lhs: Identifier, rhs: Rule) -> AddIdentifierStatus {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unreachable_symbols() {
        let mut id_map = IdentifierMap::default();

        let reachable = id_map.add_identifier("reachable".to_owned());
        let unreachable = id_map.add_identifier("unreachable".to_owned());
        let nonterminal = id_map.add_identifier("nonterminal".to_owned());

        let mut builder = Grammar::builder();
        builder.add_terminal(reachable.clone());
        builder.add_terminal(unreachable);
        builder.add_rule(nonterminal.clone(), Rule::Id(reachable.clone()));
        builder.start(nonterminal.clone());
        let grammar = builder.build(id_map).unwrap();

        let reachables = grammar.reachable_symbols();

        assert_eq!(reachables, [reachable, nonterminal].into());
    }

    #[test]
    fn undeclared_symbols() {
        let mut id_map = IdentifierMap::default();

        let declared = id_map.add_identifier("reachable".to_owned());
        let undeclared = id_map.add_identifier("unreachable".to_owned());
        let nonterminal = id_map.add_identifier("nonterminal".to_owned());

        let mut builder = Grammar::builder();
        builder.add_terminal(declared.clone());
        builder.add_rule(
            nonterminal.clone(),
            Rule::Sequence {
                contents: Box::new([Rule::Id(declared), Rule::Id(undeclared.clone())]),
            },
        );
        builder.start(nonterminal.clone());
        let grammar = builder.build(id_map).unwrap();

        let undeclared_symbols: HashSet<_> = grammar.undeclared_symbols().collect();

        assert_eq!(undeclared_symbols, [undeclared].into());
    }

    #[test]
    fn first_sets() {
        let mut id_map = IdentifierMap::default();

        let declared = id_map.add_identifier("reachable".to_owned());
        let deeper_term = id_map.add_identifier("deeper".to_owned());
        let nonterminal = id_map.add_identifier("nonterminal".to_owned());
        let subrule = id_map.add_identifier("subrule".to_owned());

        let mut builder = Grammar::builder();
        builder.add_terminal(declared.clone());
        builder.add_rule(
            nonterminal.clone(),
            Rule::Alternates {
                contents: Box::new([
                    Rule::Id(declared.clone()),
                    Rule::Id(subrule.clone()),
                    Rule::Empty,
                ]),
            },
        );
        builder.add_terminal(deeper_term.clone());
        builder.add_rule(
            subrule.clone(),
            Rule::Alternates {
                contents: Box::new([Rule::Id(deeper_term.clone()), Rule::Empty]),
            },
        );
        builder.start(nonterminal.clone());
        let grammar = builder.build(id_map).unwrap();

        assert_eq!(
            grammar.first_sets(),
            [
                (
                    subrule.clone(),
                    HashSet::from([FirstItem::Empty, FirstItem::Id(deeper_term.clone())])
                ),
                (
                    nonterminal,
                    HashSet::from([
                        FirstItem::Empty,
                        FirstItem::Id(declared),
                        FirstItem::Id(deeper_term)
                    ])
                )
            ]
            .into()
        );
    }

    #[test]
    fn follow_sets() {
        let mut id_map = IdentifierMap::default();

        let declared = id_map.add_identifier("reachable".to_owned());
        let deeper_term = id_map.add_identifier("deeper".to_owned());
        let nonterminal = id_map.add_identifier("nonterminal".to_owned());
        let subrule = id_map.add_identifier("subrule".to_owned());
        let deeper_rule = id_map.add_identifier("deeper_rule".to_owned());

        let mut builder = Grammar::builder();
        builder.add_terminal(declared.clone());
        builder.add_rule(
            nonterminal.clone(),
            Rule::Sequence {
                contents: Box::new([
                    Rule::Id(subrule.clone()),
                    Rule::Id(deeper_rule.clone()),
                    Rule::Id(declared.clone()),
                ]),
            },
        );
        builder.add_terminal(deeper_term.clone());
        builder.add_rule(
            subrule.clone(),
            Rule::Alternates {
                contents: Box::new([
                    Rule::Id(deeper_term.clone()),
                    Rule::Empty,
                    Rule::Id(deeper_rule.clone()),
                ]),
            },
        );
        builder.add_rule(deeper_rule.clone(), Rule::Id(subrule.clone()));
        builder.start(nonterminal.clone());
        let grammar = builder.build(id_map).unwrap();

        let common_follow = HashSet::from([
            FollowItem::Id(declared.clone()),
            FollowItem::Id(deeper_term),
        ]);

        assert_eq!(
            grammar.follow_sets(),
            [
                (deeper_rule.clone(), common_follow.clone()),
                (subrule.clone(), common_follow),
                (nonterminal, HashSet::from([FollowItem::EndOfInput]))
            ]
            .into()
        );
    }

    #[test]
    fn follow_of_opt() {
        let mut id_map = IdentifierMap::default();

        let start_rule = id_map.add_identifier("start_rule".into());
        let lhs = id_map.add_identifier("lhs".into());
        let rhs = id_map.add_identifier("rhs".into());
        let follow_rule = id_map.add_identifier("follow_rule".into());
        let follow_terminal = id_map.add_identifier("follow_terminal".into());

        let mut builder = Grammar::builder();

        builder.add_rule(
            start_rule.clone(),
            Rule::Sequence {
                contents: vec![
                    Rule::Alternates {
                        contents: vec![Rule::Id(lhs.clone()), Rule::Id(rhs.clone())]
                            .into_boxed_slice(),
                    },
                    Rule::Id(follow_rule.clone()),
                ]
                .into_boxed_slice(),
            },
        );

        builder.add_rule(rhs.clone(), Rule::Empty);

        builder.add_rule(follow_rule, Rule::Id(follow_terminal.clone()));

        builder.add_terminal(follow_terminal.clone());
        builder.add_terminal(lhs);

        builder.start(start_rule);
        let grammar = builder.build(id_map).unwrap();
        let follows = grammar.follow_sets();
        assert_eq!(
            follows.get(&rhs).unwrap(),
            &HashSet::from([FollowItem::Id(follow_terminal)])
        );
    }
}
