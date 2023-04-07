use front::lower;

use wasmtime::{Caller, Engine, Linker, Module, Store};

pub fn choco_val_to_str(param: i64) -> String {
    if param == lower::TRUE {
        "True".to_owned()
    } else if param == lower::FALSE {
        "False".to_owned()
    } else if param == lower::NONE {
        "None".to_owned()
    } else if (param & 0x03) == lower::OBJ_TAG {
        "obj".to_owned()
    } else {
        format!("{}", ((param & 0x0000_0003_ffff_ffff) >> 2) as i32)
    }
}

pub fn run_to_stdin_out(wat: &str) -> anyhow::Result<()> {
    let engine = Engine::default();
    let module = Module::new(&engine, wat)?;

    let mut linker = Linker::new(&engine);

    linker.func_wrap("host", "print", |_caller: Caller<'_, u32>, param: i64| {
        println!("{}", choco_val_to_str(param))
    })?;

    let mut store = Store::new(&engine, 4);
    let instance = linker.instantiate(&mut store, &module)?;
    let entry = instance.get_typed_func::<(), ()>(&mut store, "entry")?;

    // And finally we can call the wasm!
    entry.call(&mut store, ())?;

    Ok(())
}

struct IOMock {
    input_idx: usize,
    output: Vec<String>,
}
pub fn run_with_mocked_io(wat: &str, input: &[String]) -> anyhow::Result<Vec<String>> {
    let engine = Engine::default();
    let module = Module::new(&engine, wat)?;

    let mut linker = Linker::new(&engine);

    linker.func_wrap(
        "host",
        "print",
        |mut caller: Caller<'_, IOMock>, param: i64| {
            caller.data_mut().output.push(choco_val_to_str(param));
        },
    )?;

    let mut store = Store::new(
        &engine,
        IOMock {
            input_idx: 0,
            output: vec![],
        },
    );
    let instance = linker.instantiate(&mut store, &module)?;
    let entry = instance.get_typed_func::<(), ()>(&mut store, "entry")?;

    // And finally we can call the wasm!
    entry.call(&mut store, ())?;

    Ok(store.data().output.clone())
}
