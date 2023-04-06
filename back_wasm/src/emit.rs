// Code emissions for WebAssembly

use front::{ast, annotated_ast};
use front::type_check::ChocoType;
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

pub fn expression(e: &annotated_ast::Expression) -> Vec<WASMInstr> {
    match e {
        annotated_ast::Expression::Lit{ l: ast::Literal::True } => vec![WASMInstr::I64Const(TRUE)],
        annotated_ast::Expression::Lit{ l: ast::Literal::False } => vec![WASMInstr::I64Const(FALSE)],
        annotated_ast::Expression::Lit{ l: ast::Literal::None } => vec![WASMInstr::I64Const(NONE)],
        annotated_ast::Expression::Lit{ l: ast::Literal::Integer(i) } => vec![WASMInstr::I64Const((*i as i64) << 2)],
        annotated_ast::Expression::Call { f, params, choco_type } => {
            let mut instrs = params.iter().flat_map(|p| expression(p)).collect::<Vec<_>>();
            instrs.push(WASMInstr::Call(f.name.clone()));
            instrs
        }
        annotated_ast::Expression::Lit{ l: _ } => todo!(),

        annotated_ast::Expression::Binary { op, l, r, choco_type } => todo!(),
    }
}

pub fn prog(p: &annotated_ast::Program) -> WASMModule {

    let mut entry_instrs = vec![];
    for stmt in &p.stmts {
        match stmt {
            annotated_ast::Statement::Expr(e) => entry_instrs.append(&mut expression(e)),
        }
    }
    WASMModule {
        imports: vec![WASMFunImport { name: vec!["host".to_string(), "print".to_string()], params: vec![WASMType::I64], return_type: None }],
        funcs: vec![WASMFuncDef::new("entry", vec![], None, entry_instrs)]
    }
    
}
