use std::path::PathBuf;
use std::{env, fs};

mod grammar;
mod identifier_map;
mod parser;
use parser::Parser;
mod tokenizer;
mod tokens;
use tokenizer::Tokenizer;

use crate::parser::ParserError;

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
    println!("{}", raw_input);

    let tokens = Tokenizer::from(raw_input);
    let parser = Parser::new(tokens);
    let _user_grammar = match parser.parse() {
        Ok(g) => g,
        Err(e) => {
            match e {
                ParserError::NoStartingId => eprintln!("No starting id specified"),
                ParserError::ExpectedId { found } => eprintln!("Expected an Id; Found {:?}", found),
                ParserError::UnexpectedEnd => eprintln!("Unexpected End of Input"),
                ParserError::DuplicateStarts { starts } => {
                    eprintln!("Found two starts: {:?}", starts)
                }
            }
            return;
        }
    };
}
