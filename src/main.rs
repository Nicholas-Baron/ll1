#![warn(
    clippy::from_iter_instead_of_collect,
    clippy::if_not_else,
    clippy::items_after_statements,
    clippy::manual_assert,
    clippy::match_same_arms,
    clippy::module_name_repetitions,
    clippy::redundant_closure_for_method_calls,
    clippy::redundant_else,
    clippy::single_match_else,
    clippy::uninlined_format_args
)]

use std::path::PathBuf;
use std::{env, fs};

mod grammar;
use grammar::Grammar;
mod identifier_map;
mod parser;
use parser::Parser;
mod tokenizer;
mod tokens;
use tokenizer::Tokenizer;

/// Returns if anything was printed
fn print_conflicts(user_grammar: &Grammar) -> bool {
    let first_first_conflicts = user_grammar.first_first_conflicts();
    let first_follow_conflicts = user_grammar.first_follow_conflicts();

    let has_conflicts = !first_follow_conflicts.is_empty() || !first_first_conflicts.is_empty();

    if !first_first_conflicts.is_empty() {
        println!("\nFIRST-FIRST conflicts:");
    }

    for (nonterminal, conflict_set) in first_first_conflicts {
        println!(
            "First/First Conflict in {} on {{ {} }}",
            user_grammar.text_for(nonterminal),
            conflict_set
                .into_iter()
                .map(|sym| sym.printable(user_grammar))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    if !first_follow_conflicts.is_empty() {
        println!("\nFIRST-FOLLOW conflicts:");
    }

    for (nonterminal, conflict_set) in first_follow_conflicts {
        println!(
            "First/Follow Conflict in {} on {{ {} }}",
            user_grammar.text_for(nonterminal),
            conflict_set
                .into_iter()
                .map(|sym| user_grammar.text_for(sym))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    has_conflicts
}

fn print_first_and_follow_sets(user_grammar: &Grammar) {
    println!("\nFIRST and FOLLOW sets");
    let mut first_sets: Vec<(_, _, _)> = user_grammar
        .first_sets()
        .into_iter()
        .map(|(id, first_set)| (id.clone(), user_grammar.text_for(id), first_set))
        .collect();
    let follow_sets = user_grammar.follow_sets();

    first_sets.sort_by_key(|(_, name, _)| *name);

    let longest_nonterminal_name = first_sets
        .iter()
        .map(|(_, name, _)| name.len())
        .max()
        .unwrap();

    for (nonterminal, first_set, follow_set) in
        first_sets.into_iter().map(|(id, name, first_set)| {
            (
                name,
                first_set,
                follow_sets.get(&id).cloned().unwrap_or_default(),
            )
        })
    {
        let space_count = longest_nonterminal_name - nonterminal.len();
        let padding = " ".repeat(space_count + 1);

        println!(
            "FIRST ({}){}= {{ {} }}",
            nonterminal,
            padding,
            first_set
                .into_iter()
                .map(|sym| sym.printable(user_grammar))
                .collect::<Vec<_>>()
                .join(", ")
        );

        println!(
            "FOLLOW({}){}= {{ {} }}",
            nonterminal,
            padding,
            follow_set
                .into_iter()
                .map(|sym| sym.printable(user_grammar))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
}

fn print_warnings(user_grammar: &Grammar) {
    use std::collections::HashSet;

    let undeclared_symbols: HashSet<_> = user_grammar.undeclared_symbols().collect();
    let reachable_symbols = user_grammar.reachable_symbols();
    let unreachable_nonterminals: HashSet<_> = user_grammar
        .nonterminal_symbols()
        .filter(|sym| !reachable_symbols.contains(sym))
        .collect();

    let unreachable_terminals: HashSet<_> = user_grammar
        .terminal_symbols()
        .filter(|sym| !reachable_symbols.contains(sym))
        .collect();

    let warning_count =
        undeclared_symbols.len() + unreachable_terminals.len() + unreachable_nonterminals.len();

    if warning_count > 0 {
        println!("{warning_count} warnings found");
    }

    for id in undeclared_symbols {
        println!(
            "Identifier {} is not explicity a terminal or nonterminal",
            user_grammar.text_for(id)
        );
    }

    let starting_symbol_name = user_grammar.text_for(user_grammar.starting_id());
    for unreachable_nonterminal in unreachable_nonterminals {
        println!(
            "Nonterminal {} cannot be reached from starting symbol {starting_symbol_name}",
            user_grammar.text_for(unreachable_nonterminal),
        );
    }

    for unreachable_terminal in unreachable_terminals {
        println!(
            "Terminal {} cannot be reached from starting symbol {starting_symbol_name}",
            user_grammar.text_for(unreachable_terminal),
        );
    }
}

fn main() {
    let Some(filename) = env::args().nth(1).map(PathBuf::from) else {
        eprintln!("No input path specified");
        return;
    };

    if !filename.exists() {
        eprintln!("File {} does not exist", filename.display());
        return;
    }

    println!("Reading from {}", filename.display());

    let raw_input = match fs::read_to_string(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading grammar: {e}");
            return;
        }
    };

    let tokens = Tokenizer::from(raw_input);
    let parser = Parser::new(tokens);
    let user_grammar = match parser.parse() {
        Ok(g) => g,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };

    println!("\n{user_grammar}");

    print_warnings(&user_grammar);

    print_first_and_follow_sets(&user_grammar);

    let has_conflicts = print_conflicts(&user_grammar);

    println!(
        "\nProvided grammar is {}LL1",
        has_conflicts.then_some("not ").unwrap_or_default()
    );
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn firsts_are_all_nonterminal() {
        let raw_input = include_str!("../examples/simple.txt");
        let parser = Parser::new(Tokenizer::from(raw_input.to_string()));
        let grammar = parser.parse().unwrap();

        let firsts = grammar.first_sets();
        assert_eq!(firsts.len(), 2);

        for nonterminal in grammar.nonterminal_symbols() {
            assert!(firsts.contains_key(&nonterminal));
        }

        for terminal in grammar.terminal_symbols() {
            assert!(!firsts.contains_key(&terminal));
        }
    }
}
