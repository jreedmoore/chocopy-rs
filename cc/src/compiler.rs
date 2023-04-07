use front::lexer::Lexer;
use front::parser::Parser;
use front::lower::Lower;
use back_wasm::wasm::WATPrint;

pub fn produce_wat(input: &str) -> anyhow::Result<String> {
    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);

    let prog = parser.parse()?;

    let mut typeck = front::type_check::TypeChecker::new();
    let ann_prog = typeck.check_prog(&prog)?;

    let mut lower = Lower::new();
    let stack = lower.lower_prog(&ann_prog);

    Ok(back_wasm::emit::prog(&stack).wat_print())
}