use std::path::PathBuf;
use std::{env, fs};

mod grammar;
mod identifier_map;
mod parser;
use parser::Parser;
mod tokenizer;
mod tokens;
use tokenizer::Tokenizer;

fn main() {
    let filename: PathBuf = match env::args().nth(1) {
        Some(path) => PathBuf::from(path),
        None => {
            eprintln!("No input path specified");
            return;
        }
    };

    if !filename.exists() {
        eprintln!("File {} does not exist", filename.display());
        return;
    }

    println!("Reading from {}", filename.display());

    let raw_input = match fs::read_to_string(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading grammar: {}", e);
            return;
        }
    };

    let tokens = Tokenizer::from(raw_input);
    let parser = Parser::new(tokens);
    let user_grammar = match parser.parse() {
        Ok(g) => g,
        Err(e) => {
            eprintln!("{}", e);
            return;
        }
    };

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

    for (nonterminal, first_set) in user_grammar.first_sets() {
        println!(
            "FIRST({}) = {{ {} }}",
            user_grammar.text_for(nonterminal),
            first_set
                .into_iter()
                .map(|sym| sym.printable(&user_grammar))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    for (nonterminal, follow_set) in user_grammar.follow_sets() {
        println!(
            "FOLLOW({}) = {{ {} }}",
            user_grammar.text_for(nonterminal),
            follow_set
                .into_iter()
                .map(|sym| match sym {
                    grammar::FollowItem::EndOfInput => "$",
                    grammar::FollowItem::Id(id) => user_grammar.text_for(id),
                })
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
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
