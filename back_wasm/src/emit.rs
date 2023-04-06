// Code emissions for WebAssembly

use itertools::Itertools;
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

    let entry_instrs = p.instrs.iter().map(|instr| {
        match instr {
            stack::Instr::NumConst(n) => WASMInstr::I64Const(*n),
            stack::Instr::Add => WASMInstr::I64Add,
            stack::Instr::BitOr => WASMInstr::I64Or,
            stack::Instr::BitAnd => WASMInstr::I64And,
            stack::Instr::BitXor => WASMInstr::I64Xor,
            stack::Instr::Call(n) => WASMInstr::Call(n.clone()),
            stack::Instr::Drop => WASMInstr::Drop,
            stack::Instr::Sub => WASMInstr::I64Sub,
            stack::Instr::Mul => WASMInstr::I64Mul,
            stack::Instr::Modulo => WASMInstr::I64RemSigned,
            stack::Instr::Div => WASMInstr::I64DivSigned,
            stack::Instr::ArithShiftRight => WASMInstr::I64ShrSigned,
            stack::Instr::ShiftLeft => WASMInstr::I64ShiftLeft,
        } 
    }).collect_vec();
    
    WASMModule {
        imports: vec![WASMFunImport { name: vec!["host".to_string(), "print".to_string()], params: vec![WASMType::I64], return_type: None }],
        funcs: vec![WASMFuncDef::new("entry", vec![], None, entry_instrs)]
    }
    
}
