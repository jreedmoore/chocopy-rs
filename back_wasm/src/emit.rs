// Code emissions for WebAssembly

use middle::stack;
use crate::wasm::*;

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum TypeTag {
    Int = 0,
    None = 1,
    Bool = 2,
    Object = 3
}

pub const TRUE: i64 = 0x1000_0000_0000_0002;
pub const FALSE: i64 = 0x0000_0000_0000_0002;
pub const NONE: i64 = 0x0000_0000_0000_0001;
pub const OBJ_TAG: i64 = 0x0000_0000_0000_0003;

pub fn prog(p: &stack::Program) -> WASMModule {

    let mut entry_instrs = vec![];
    for instr in &p.instrs {
        match instr {
            stack::Instr::NumConst(n) => entry_instrs.push(WASMInstr::I64Const(*n)),
            stack::Instr::Add => entry_instrs.push(WASMInstr::I64Add),
            stack::Instr::BitOr => entry_instrs.push(WASMInstr::I64Or),
            stack::Instr::Call(n) => entry_instrs.push(WASMInstr::Call(n.clone())),
            stack::Instr::Drop => entry_instrs.push(WASMInstr::Drop),
            stack::Instr::Sub => entry_instrs.push(WASMInstr::I64Sub),
        } 
    }
    
    WASMModule {
        imports: vec![WASMFunImport { name: vec!["host".to_string(), "print".to_string()], params: vec![WASMType::I64], return_type: None }],
        funcs: vec![WASMFuncDef::new("entry", vec![], None, entry_instrs)]
    }
    
}
