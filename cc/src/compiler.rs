use front::lexer::Lexer;
use front::lower::Lower;
use front::parser::Parser;
use front::type_check::TypeChecker;
use middle::stack;

pub fn produce_stack_ir(program: &str) -> anyhow::Result<stack::FlatProgram> {
    let p = Parser::new(Lexer::new(program)).parse()?;
    let a = TypeChecker::new().check_prog(&p)?;
    Ok(Lower::new().lower_prog(&a).clone())
}
