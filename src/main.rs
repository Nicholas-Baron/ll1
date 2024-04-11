use std::env;
use std::error::Error;
use std::path::PathBuf;
use std::str::FromStr;

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

    println!("Reading from {}", filename.display());
    Ok(())
}
