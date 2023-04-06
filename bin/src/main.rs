
use cc::compiler;
use cc::runtime;

use std::fs;

fn main() -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let filename = 
        if args.len() <= 1 {
            "front/data/examples/expr.py"
        } else {
            &args[1]
        };
    let input = fs::read_to_string(filename)?;

    let wat = compiler::produce_wat(&input)?;

    println!("wat: {}", wat);

    runtime::run_to_stdin_out(&wat)?;

    Ok(())
}
