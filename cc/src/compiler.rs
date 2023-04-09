use front::lexer::Lexer;
use front::lower::Lower;
use front::parser::Parser;
use front::type_check::TypeChecker;
use middle::stack::FlatProgram;

pub fn produce_stack_ir(program: &str) -> FlatProgram {
    let p = Parser::new(Lexer::new(program)).parse().unwrap();
    let mut typeck = TypeChecker::new();
    let p = typeck.check_prog(&p).unwrap();
    let mut lower = Lower::new();
    let p = lower.lower_prog(&p);
    FlatProgram::from_program_clone(p)
}
