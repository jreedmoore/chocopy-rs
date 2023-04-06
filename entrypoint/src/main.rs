use std::fs;

use front::lexer::Lexer;
use front::parser::Parser;
use back_wasm::wasm::WATPrint;
use wasmtime::{Engine, Module, Store, Linker, Caller};

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let filename = 
        if args.len() <= 1 {
            "front/data/examples/expr.py"
        } else {
            &args[1]
        };
    let input = fs::read_to_string(filename).unwrap();

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);

    let prog = parser.parse().unwrap();

    let typeck = front::type_check::TypeChecker::new();
    let ann_prog = typeck.check_prog(&prog).unwrap();

    let wat = back_wasm::emit::prog(&ann_prog).wat_print();
    println!("wat: {}", wat);

    let engine = Engine::default();
    let module = Module::new(&engine, wat).unwrap();

    let mut linker = Linker::new(&engine);
    linker.func_wrap("host", "print", |_caller: Caller<'_, u32>, param: i64| {
        if param == back_wasm::emit::TRUE {
            println!("True")
        } else if param == back_wasm::emit::FALSE {
            println!("False")
        } else if param == back_wasm::emit::NONE {
            println!("None")
        } else if (param & 0x03) == back_wasm::emit::OBJ_TAG {
            println!("obj")
        } else {
            println!("{}", ((param & 0x0000_0003_ffff_ffff) >> 2) as i32)
        }
    }).unwrap();

    let mut store = Store::new(&engine, 4);
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let hello = instance.get_typed_func::<(), ()>(&mut store, "entry").unwrap();

    // And finally we can call the wasm!
    hello.call(&mut store, ()).unwrap();
}
