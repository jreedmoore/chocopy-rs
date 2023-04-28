use front::lexer::Lexer;
use front::lower::Lower;
use front::parser::Parser;
use front::type_check::TypeChecker;
use middle::analyze;
use middle::stack::FlatProgram;

pub fn produce_stack_ir(program: &str) -> anyhow::Result<FlatProgram> {
    let p = Parser::new(Lexer::new(program)).parse()?;
    let mut typeck = TypeChecker::new();
    let p = typeck.check_prog(&p)?;
    let mut lower = Lower::new();
    let p = lower.lower_prog(&p);
    analyze::check_exhaustive_returns(p)?;
    Ok(FlatProgram::from_program_clone(p))
}
