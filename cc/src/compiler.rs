use front::lexer::Lexer;
use front::lower::Lower;
use front::parser::Parser;
use front::type_check::TypeChecker;
use middle::stack::{self, FlatProgram};

pub fn produce_stack_ir(program: &str) -> anyhow::Result<stack::FlatProgram> {
    let p = Parser::new(Lexer::new(program)).parse()?;
    let p = TypeChecker::new().check_prog(&p)?;
    let mut lower = Lower::new();
    let p = lower.lower_prog(&p);
    let p = FlatProgram::from_program_clone(p);
    Ok(p)
}
