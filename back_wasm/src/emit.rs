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

    let entry_instrs = p.instrs.iter().flat_map(|instr| {
        match instr {
            stack::Instr::NumConst(n) => vec![WASMInstr::I64Const(*n)],
            stack::Instr::Add => vec![WASMInstr::I64Add],
            stack::Instr::BitOr => vec![WASMInstr::I64Or],
            stack::Instr::BitAnd => vec![WASMInstr::I64And],
            stack::Instr::BitXor => vec![WASMInstr::I64Xor],
            stack::Instr::Call(n) => vec![WASMInstr::Call(n.clone())],
            stack::Instr::Drop => vec![WASMInstr::Drop],
            stack::Instr::Sub => vec![WASMInstr::I64Sub],
            stack::Instr::Mul => vec![WASMInstr::I64Mul],
            stack::Instr::Modulo => vec![WASMInstr::I64RemSigned],
            stack::Instr::Div => vec![WASMInstr::I64DivSigned],
            stack::Instr::ArithShiftRight => vec![WASMInstr::I64ShrSigned],
            stack::Instr::ShiftLeft => vec![WASMInstr::I64ShiftLeft],

            // WASM comparisons produce an i32 bool as a result, so we map into i64, rotate into LSB into MSB, and mask on TAG
            stack::Instr::Eq => vec![WASMInstr::I64Eq, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
            stack::Instr::Ne => vec![WASMInstr::I64Ne, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
            stack::Instr::Lt => vec![WASMInstr::I64Lt, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
            stack::Instr::Lte => vec![WASMInstr::I64Lte, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
            stack::Instr::Gt => vec![WASMInstr::I64Gt, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
            stack::Instr::Gte => vec![WASMInstr::I64Gte, WASMInstr::I64ExtendI32, WASMInstr::I64Const(1), WASMInstr::I64RotateRight, WASMInstr::I64Const(FALSE), WASMInstr::I64Or],
        } 
    }).collect_vec();
    
    WASMModule {
        imports: vec![WASMFunImport { name: vec!["host".to_string(), "print".to_string()], params: vec![WASMType::I64], return_type: None }],
        funcs: vec![WASMFuncDef::new("entry", vec![], None, entry_instrs)]
    }
    
}
