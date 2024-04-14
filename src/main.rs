use std::path::PathBuf;
use std::{env, fs};

mod grammar;
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
    println!("{}", raw_input);

    let _tokens = Tokenizer::from(raw_input);
}
