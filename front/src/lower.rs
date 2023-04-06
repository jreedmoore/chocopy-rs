// Lower from Annotated AST to Stack IR

use crate::{annotated_ast::{*, self}, ast, type_check::ChocoType};
use middle::stack::{self, Instr};

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum TypeTag {
    Int = 0,
    None = 1,
    Bool = 2,
    Object = 3
}

pub const MSB: i64 = (0x8000_0000_0000_0000 as u64) as i64;
pub const TRUE: i64 = (0x8000_0000_0000_0002 as u64) as i64;
pub const FALSE: i64 = 0x0000_0000_0000_0002;
pub const NONE: i64 = 0x0000_0000_0000_0001;
pub const OBJ_TAG: i64 = 0x0000_0000_0000_0003;

pub const TAG_BITS: usize = 2;

pub struct Lower {
    lowered: stack::Program
}
impl Lower {
    pub fn new() -> Lower {
        Lower { lowered: stack::Program::new() }
    }


    fn push_instr(&mut self, instr: stack::Instr) {
        self.lowered.instrs.push(instr);
    }

    pub fn lower_prog(&mut self, prog: &annotated_ast::Program) -> &stack::Program {
        for stmt in &prog.stmts {
            self.lower_statement(&stmt);
        }
        &self.lowered
    }

    fn lower_statement(&mut self, stmt: &annotated_ast::Statement) {
        match stmt {
            annotated_ast::Statement::Expr(e) => {
                self.lower_expr(e);
                if e.choco_type() != ChocoType::None {
                    self.lowered.instrs.push(stack::Instr::Drop)
                }
            }
        } 
    }

    fn lower_expr(&mut self, e: &Expression) {
        match e {
            Expression::Lit { l: ast::Literal::True } => self.push_instr(Instr::NumConst(TRUE)),
            Expression::Lit { l: ast::Literal::False } => self.push_instr(Instr::NumConst(FALSE)),
            Expression::Lit { l: ast::Literal::Integer(i) } => self.push_instr(Instr::NumConst((*i as i64) << TAG_BITS)),
            Expression::Lit { l: ast::Literal::None } => self.push_instr(Instr::NumConst(NONE)),
            Expression::Lit { l: _ } => todo!(), // strings
            Expression::Unary { op: annotated_ast::UnaryOp::LogicalNot, e, .. } => {
                self.lower_expr(e);
                self.push_instr(Instr::NumConst(MSB));
                self.push_instr(Instr::BitXor);
            }
            Expression::Binary { op, l, r, .. } => {
                self.lower_expr(l);
                self.lower_expr(r);
                self.push_instr(match op {
                    crate::ast::BinOp::And => Instr::BitAnd,
                    crate::ast::BinOp::Or => Instr::BitOr,
                    crate::ast::BinOp::Plus => Instr::Add,
                    crate::ast::BinOp::Minus => Instr::Sub,
                    crate::ast::BinOp::Multiply => Instr::Mul,
                    crate::ast::BinOp::IntegerDiv => Instr::Div,
                    crate::ast::BinOp::Modulo => Instr::Modulo,

                    crate::ast::BinOp::Equals => todo!(),
                    crate::ast::BinOp::NotEquals => todo!(),
                    crate::ast::BinOp::LessThan => todo!(),
                    crate::ast::BinOp::LessThanEqual => todo!(),
                    crate::ast::BinOp::GreaterThan => todo!(),
                    crate::ast::BinOp::GreaterThanEqual => todo!(),
                    crate::ast::BinOp::Is => todo!(),
                });
                if *op == crate::ast::BinOp::Multiply {
                    self.push_instr(Instr::NumConst(TAG_BITS as i64));
                    self.push_instr(Instr::ArithShiftRight);
                }
                if *op == crate::ast::BinOp::IntegerDiv {
                    self.push_instr(Instr::NumConst(TAG_BITS as i64));
                    self.push_instr(Instr::ShiftLeft);
                }
            }
            Expression::Call { f, params, .. } => {
                params.iter().for_each(|p| self.lower_expr(p));
                self.push_instr(Instr::Call(f.name.clone()))
            }
        }
    }
}
