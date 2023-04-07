// Lower from Annotated AST to Stack IR

use std::collections::HashMap;

use crate::{
    annotated_ast::{self, *},
    ast,
    type_check::ChocoType,
};
use middle::stack::{self, Instr};

#[derive(Debug, PartialEq)]
#[repr(u8)]
pub enum TypeTag {
    Int = 0,
    None = 1,
    Bool = 2,
    Object = 3,
}

pub const MSB: i64 = (0x8000_0000_0000_0000 as u64) as i64;
pub const TRUE: i64 = (0x8000_0000_0000_0002 as u64) as i64;
pub const FALSE: i64 = 0x0000_0000_0000_0002;
pub const NONE: i64 = 0x0000_0000_0000_0001;
pub const OBJ_TAG: i64 = 0x0000_0000_0000_0003;

pub const TAG_BITS: usize = 2;

pub struct Locals {
    max_used: usize,
    bindings: HashMap<String, usize>,
}
impl Locals {
    pub fn new() -> Locals {
        Locals {
            max_used: 0,
            bindings: HashMap::new(),
        }
    }
}
pub struct Lower {
    lowered: stack::Program,
    locals: Locals,
}
impl Lower {
    pub fn new() -> Lower {
        Lower {
            lowered: stack::Program::new(),
            locals: Locals::new(),
        }
    }

    fn push_instr(&mut self, instr: stack::Instr) {
        self.lowered.instrs.push(instr);
    }

    pub fn lower_prog(&mut self, prog: &annotated_ast::Program) -> &stack::Program {
        for stmt in &prog.stmts {
            self.lower_statement(&stmt);
        }
        self.lowered.locals = self.locals.max_used;
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
            Statement::Assign(Var::Local { name, .. }, e) => {
                let index = self.upsert_local(name);
                self.lower_expr(e);
                if e.choco_type() == ChocoType::None {
                    self.push_instr(Instr::NumConst(NONE))
                }
                self.push_instr(Instr::StoreLocal(index))
            }
            Statement::If { cond, then, els } => {
                self.lower_expr(cond);
                self.push_instr(Instr::If(false));
                then.iter().for_each(|s| self.lower_statement(s));
                if !els.is_empty() {
                    self.push_instr(Instr::Else);
                    els.iter().for_each(|s| self.lower_statement(s));
                }
                self.push_instr(Instr::EndIf);
            }
        }
    }

    fn lower_expr(&mut self, e: &Expression) {
        match e {
            Expression::Lit {
                l: ast::Literal::True,
            } => self.push_instr(Instr::NumConst(TRUE)),
            Expression::Lit {
                l: ast::Literal::False,
            } => self.push_instr(Instr::NumConst(FALSE)),
            Expression::Lit {
                l: ast::Literal::Integer(i),
            } => self.push_instr(Instr::NumConst((*i as i64) << TAG_BITS)),
            Expression::Lit {
                l: ast::Literal::None,
            } => self.push_instr(Instr::NumConst(NONE)),
            Expression::Lit { l: _ } => todo!(), // strings
            Expression::Unary {
                op: annotated_ast::UnaryOp::LogicalNot,
                e,
                ..
            } => {
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

                    crate::ast::BinOp::Equals => Instr::Eq,
                    crate::ast::BinOp::NotEquals => Instr::Ne,
                    crate::ast::BinOp::LessThan => Instr::Lt,
                    crate::ast::BinOp::LessThanEqual => Instr::Lte,
                    crate::ast::BinOp::GreaterThan => Instr::Gt,
                    crate::ast::BinOp::GreaterThanEqual => Instr::Gte,

                    // object id comparison, only makes sense with allocation implemented
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
            Expression::Ternary {
                cond, then, els, ..
            } => {
                self.lower_expr(cond);
                self.push_instr(Instr::If(true));
                self.lower_expr(then);
                self.push_instr(Instr::Else);
                self.lower_expr(els);
                self.push_instr(Instr::EndIf);
            }
            Expression::Load {
                v: Var::Local { name, .. },
            } => self.push_instr(Instr::LoadLocal(self.get_local(&name))),
        }
    }

    fn upsert_local(&mut self, name: &str) -> usize {
        *self
            .locals
            .bindings
            .entry(name.to_owned())
            .or_insert_with(|| {
                let id = self.locals.max_used;
                self.locals.max_used += 1;
                id
            })
    }

    fn get_local(&self, name: &str) -> usize {
        self.locals.bindings[name]
    }
}
