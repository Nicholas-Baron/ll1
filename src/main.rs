#![warn(
    clippy::uninlined_format_args,
    clippy::redundant_else,
    clippy::redundant_closure_for_method_calls,
    clippy::items_after_statements,
    clippy::match_same_arms,
    clippy::single_match_else,
    clippy::manual_assert,
    clippy::from_iter_instead_of_collect,
    clippy::module_name_repetitions
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

    println!("\nFIRST-FIRST conflicts:");
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

    println!("\nFIRST-FOLLOW conflicts:");
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
    let mut first_sets: Vec<_> = user_grammar.first_sets().into_iter().collect();
    let follow_sets = user_grammar.follow_sets();

    first_sets.sort_by_key(|(id, _)| id.clone());

    let longest_nonterminal_name = first_sets
        .iter()
        .map(|(id, _)| user_grammar.text_for(id.clone()).len())
        .max()
        .unwrap();

    for (nonterminal, first_set, follow_set) in first_sets.into_iter().map(|(id, first_set)| {
        (
            user_grammar.text_for(id.clone()),
            first_set,
            follow_sets.get(&id).cloned().unwrap_or_default(),
        )
    }) {
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

    println!("\nWarnings:");

    for id in user_grammar.undeclared_symbols() {
        println!(
            "Identifier {} is not explicity a terminal or nonterminal",
            user_grammar.text_for(id)
        );
    }

    let reachable_symbols = user_grammar.reachable_symbols();
    for unreachable_nonterminal in user_grammar
        .nonterminal_symbols()
        .filter(|sym| !reachable_symbols.contains(sym))
    {
        println!(
            "Nonterminal {} cannot be reached from starting symbol {}",
            user_grammar.text_for(unreachable_nonterminal),
            user_grammar.text_for(user_grammar.starting_id())
        );
    }

    for unreachable_terminal in user_grammar
        .terminal_symbols()
        .filter(|sym| !reachable_symbols.contains(sym))
    {
        println!(
            "Terminal {} cannot be reached from starting symbol {}",
            user_grammar.text_for(unreachable_terminal),
            user_grammar.text_for(user_grammar.starting_id())
        );
    }

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
