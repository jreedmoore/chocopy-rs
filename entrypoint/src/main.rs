use std::fs;

use front::lexer::Lexer;
use front::parser::Parser;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let filename = &args[1];
    let input = fs::read_to_string(filename).unwrap();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(prog) => println!("Parsed into {:?}", prog),
        Err(e) => println!("Failed with {:?}", e),
    }
}
