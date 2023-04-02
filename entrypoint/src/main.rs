use std::fs;

use parser::lexer::Lexer;
use parser::parser::Parser;


fn main() {
    let filename = std::env::args().next().expect("Expected filename");
    let input = fs::read_to_string(filename).unwrap();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(prog) => println!("Parsed into {:?}", prog),
        Err(e) => println!("Failed with {:?}", e)
    }
}
