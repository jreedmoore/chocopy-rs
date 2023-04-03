use std::{error::Error, fs};

use front::lexer;
use front::parser::Parser;

#[test]
fn test_lexer_on_examples() -> Result<(), Box<dyn Error>> {
    for entry in fs::read_dir("data/examples/")? {
        let entry = entry?;
        let path = entry.path();

        let bytes = fs::read(path)?;
        let input = std::str::from_utf8(&bytes)?;

        assert!(lexer::lex(input).is_ok());
    }
    Ok(())
}

#[test]
fn test_lexer_with_include_str() -> Result<(), Box<dyn Error>> {
    let mut p = Parser::new(lexer::Lexer::new(include_str!("../data/examples/ex1.py")));
    p.parse()?;
    Ok(())
}
