use std::error::Error;
use std::path::PathBuf;
use std::str::FromStr;
use std::{env, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let Some(filename) = env::args()
        .nth(1)
        .as_deref()
        .map(PathBuf::from_str)
        .transpose()?
    else {
        eprintln!("Input filename not specified");
        return Ok(());
    };

    if !filename.exists() {
        eprintln!("File {} does not exist", filename.display());
        return Ok(());
    }

    println!("Reading from {}", filename.display());

    let raw_input = fs::read_to_string(filename)?;
    println!("{}", raw_input);

    Ok(())
}
