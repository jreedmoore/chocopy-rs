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

pub fn expression(e: &ast::Expression, _ctype: ChocoType) -> Vec<WASMInstr> {
    match e {
        ast::Expression::Lit(ast::Literal::True) => vec![WASMInstr::I64Const(TRUE), WASMInstr::Call("host_print".to_string())],
        ast::Expression::Lit(ast::Literal::False) => vec![WASMInstr::I64Const(FALSE), WASMInstr::Call("host_print".to_string())],
        ast::Expression::Lit(ast::Literal::None) => vec![WASMInstr::I64Const(NONE), WASMInstr::Call("host_print".to_string())],
        ast::Expression::Lit(ast::Literal::Integer(i)) => vec![WASMInstr::I64Const((*i as i64) << 2), WASMInstr::Call("host_print".to_string())],
        ast::Expression::Lit(_) => todo!(),
        //
        ast::Expression::Ternary { e, if_expr, else_expr } => todo!(),
        ast::Expression::Id(_) => todo!(),
        ast::Expression::Not(_) => todo!(),
        ast::Expression::ListLiteral(_) => todo!(),
        ast::Expression::Member(_) => todo!(),
        ast::Expression::Index(_) => todo!(),
        ast::Expression::MemberCall(_, _) => todo!(),
        ast::Expression::Call(_, _) => todo!(),
        ast::Expression::BinaryOp(_, _, _) => todo!(),
        ast::Expression::UnaryMinus(_) => todo!(),
    }
}

pub fn prog(e: &ast::Program) -> WASMModule {
    if let Some(ast::Statement::Expr(e)) = e.stmts.first() {
        WASMModule {
            imports: vec![WASMFunImport { name: vec!["host".to_string(), "print".to_string()], params: vec![WASMType::I64], return_type: None }],
            funcs: vec![WASMFuncDef::new("entry", vec![], None, expression(e, ChocoType::Bool))]
        }
    } else {
        panic!("Not a single expression program")
    }
    
}
