// Lower from Annotated AST to Stack IR

use std::collections::HashMap;

use crate::{
    annotated_ast::{self, *},
    ast,
    type_check::ChocoType,
};
use middle::stack::{self, BlockLocation, Instr};

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

    fn clear(&mut self) {
        self.bindings.clear();
        self.max_used = 0;
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

    pub fn lower_prog(&mut self, prog: &annotated_ast::Program) -> &stack::Program {
        for class in &prog.classes {
            let dispatch_name = class.name.to_owned() + &"$$dispatch";
            let mut dispatch = stack::Block::named(dispatch_name.clone());
            for (_, funid) in &class.methods {
                dispatch.instrs.push(Instr::Jump(BlockLocation::Named(funid.name.to_owned())))
            }
            self.lowered.blocks.push(dispatch);

            self.lowered.start_function(class.name.to_owned() + &"$$new", true);
            self.push_instr(Instr::ClassAlloc(class.vars.len(), BlockLocation::Named(dispatch_name)));
            for (idx, (_, init_val)) in class.vars.iter().enumerate() {
                self.lower_literal(init_val);
                self.push_instr(Instr::Duplicate(1));
                self.push_instr(Instr::ClassMemberStore(idx))
            }
            self.push_instr(Instr::Jump(BlockLocation::Named(class.name.to_owned() + &"$__init__")))
        }
        for fun in &prog.funs {
            self.locals.clear();
            for p in &fun.params {
                self.upsert_local(&p.name);
            }
            self.lowered.start_function(fun.name.clone(), fun.return_type != ChocoType::None);
            for stmt in &fun.body {
                self.lower_statement(&stmt);
            }
        }
        self.lowered.insert_nop();
        &self.lowered
    }

    fn lower_statement(&mut self, stmt: &annotated_ast::Statement) {
        match stmt {
            annotated_ast::Statement::Expr(e) => {
                self.lower_expr(e);
            }
            Statement::Assign(lhs, e) => {
                self.lower_expr(e);
                for lhs in lhs {
                    self.push_instr(Instr::Duplicate(0));
                    match lhs {
                        Lhs::Var(Var::Local { name, .. }) => {
                            let index = self.upsert_local(name);
                            self.push_instr(Instr::StoreLocal(index))
                        }
                        Lhs::Index { list, index, .. } => {
                            self.lower_expr(index);
                            self.lower_expr(list);
                            self.push_instr(Instr::ListAssign)
                        }
                        Lhs::Member { expr, offset, .. } => {
                            self.lower_expr(expr);
                            self.push_instr(Instr::ClassMemberStore(*offset));
                        }
                    }
                }
                self.push_instr(Instr::Drop);
            }
            Statement::Declare(Var::Local { name, .. }, e) => {
                self.upsert_local(name);
                self.lower_expr(e);
                if e.choco_type() == ChocoType::None {
                    self.push_instr(Instr::NoneConst)
                }
            }
            Statement::If { cond, then, els } => {
                self.lower_expr(cond);
                if !els.is_empty() {
                    self.push_instr(Instr::IfJump(BlockLocation::BlockOffset(2)));
                    self.start_block();
                    els.iter().for_each(|s| self.lower_statement(s));
                    self.push_instr(Instr::Jump(BlockLocation::BlockOffset(2)));
                    self.start_block();
                    then.iter().for_each(|s| self.lower_statement(s));
                    self.start_block();
                } else {
                    self.push_instr(Instr::UnaryNot);
                    self.push_instr(Instr::IfJump(BlockLocation::BlockOffset(2)));
                    self.start_block();
                    then.iter().for_each(|s| self.lower_statement(s));
                    self.start_block();
                }
            }
            Statement::While { cond, stmts } => {
                self.start_block();
                self.lower_expr(cond);
                self.push_instr(Instr::UnaryNot);
                self.push_instr(Instr::IfJump(BlockLocation::BlockOffset(1)));
                stmts.iter().for_each(|s| self.lower_statement(s));
                self.push_instr(Instr::Jump(BlockLocation::BlockOffset(0)));
                self.start_block();
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    self.lower_expr(e);
                }
                self.push_instr(Instr::Return);
            }
        }
    }

    fn lower_literal(&mut self, l: &TyLiteral) {
        match l {
            TyLiteral {
                l: Literal::True,
                ..
            } => self.push_instr(Instr::BoolConst(true)),
            TyLiteral {
                l: Literal::False,
                ..
            } => self.push_instr(Instr::BoolConst(false)),
            TyLiteral {
                l: Literal::Integer(i),
                ..
            } => self.push_instr(Instr::NumConst(*i)),
            TyLiteral {
                l: Literal::None,
                ..
            } => self.push_instr(Instr::NoneConst),
            TyLiteral {
                l: Literal::Str(s),
                ..
            } => {
                let idx = self.push_constant(s);
                self.push_instr(Instr::LoadConstant(idx));
            }
            TyLiteral {
                l: Literal::List(exprs),
                ..
            } => {
                self.push_instr(Instr::ListAlloc(exprs.len()));
                for (idx, expr) in exprs.into_iter().enumerate() {
                    self.lower_expr(expr);
                    self.push_instr(Instr::NumConst(idx as i32));
                    self.push_instr(Instr::Duplicate(2));
                    self.push_instr(Instr::ListAssign);
                }
            }
        }
    }

    fn lower_expr(&mut self, e: &Expression) {
        match e {
            Expression::Lit { l } => self.lower_literal(l),
            Expression::Unary {
                op: annotated_ast::UnaryOp::LogicalNot,
                e,
                ..
            } => {
                self.lower_expr(e);
                self.push_instr(Instr::UnaryNot);
            }
            Expression::Binary {
                op,
                l,
                r,
                choco_type: ChocoType::None,
            } => {
                self.lower_expr(l);
                self.lower_expr(r);
                self.push_instr(match op {
                    ast::BinOp::Equals => Instr::Eq,
                    ast::BinOp::NotEquals => Instr::Ne,
                    ast::BinOp::Is => Instr::Is,
                    _ => panic!("Unsupported op for None"),
                })
            }
            Expression::Binary {
                op,
                l,
                r,
                choco_type: ChocoType::Str,
            } => {
                self.lower_expr(l);
                self.lower_expr(r);
                self.push_instr(match op {
                    ast::BinOp::Plus => Instr::StrConcat,
                    ast::BinOp::Equals => Instr::Eq,
                    ast::BinOp::NotEquals => Instr::Ne,
                    ast::BinOp::Is => Instr::Is,
                    _ => panic!("Unsupported op for strings"),
                })
            }
            Expression::Binary {
                op,
                l,
                r,
                choco_type: ChocoType::List(_),
            } => {
                self.lower_expr(l);
                self.lower_expr(r);
                self.push_instr(match op {
                    ast::BinOp::Plus => Instr::ListConcat,
                    ast::BinOp::Equals => Instr::Eq,
                    ast::BinOp::NotEquals => Instr::Ne,
                    ast::BinOp::Is => Instr::Is,
                    _ => panic!("Unsupported op for strings"),
                })
            }
            Expression::Binary {
                op,
                l,
                r,
                choco_type: ChocoType::Int | ChocoType::Bool,
            } => {
                self.lower_expr(l);
                self.lower_expr(r);
                self.push_instr(match op {
                    crate::ast::BinOp::And => Instr::LogicalAnd,
                    crate::ast::BinOp::Or => Instr::LogicalOr,
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
                    crate::ast::BinOp::Is => Instr::Is,
                });
            }
            Expression::Binary { .. } => panic!("Unsupported binary ops"),
            Expression::Call {
                f, params, native, ..
            } => {
                params.iter().for_each(|p| self.lower_expr(p));
                if *native {
                    self.push_instr(Instr::CallNative(f.name.clone()))
                } else {
                    self.push_instr(Instr::Call {
                        loc: BlockLocation::Named(f.name.clone()),
                        arity: params.len(),
                    })
                }
            }
            Expression::Ternary {
                cond, then, els, ..
            } => {
                self.lower_expr(cond);
                self.push_instr(Instr::IfJump(BlockLocation::BlockOffset(2)));
                self.start_block();
                self.lower_expr(els);
                self.push_instr(Instr::Jump(BlockLocation::BlockOffset(2)));
                self.start_block();
                self.lower_expr(then);
                self.start_block();
            }
            Expression::Load {
                v: Var::Local { name, .. },
            } => self.push_instr(Instr::LoadLocal(self.get_local(&name))),
            Expression::Index { expr, index } => match expr.choco_type() {
                ChocoType::Str => {
                    self.lower_expr(expr);
                    self.lower_expr(index);
                    self.push_instr(Instr::StrIndex);
                }
                ChocoType::List(_) => {
                    self.lower_expr(expr);
                    self.lower_expr(index);
                    self.push_instr(Instr::ListIndex);
                }
                t => panic!("Unsupported type for index {:?}", t),
            },
            Expression::MemberCall { target, params, .. } => {
                for p in params {
                    self.lower_expr(p)
                }
                self.lower_expr(&target.expr);
                self.push_instr(stack::Instr::ClassMethodCall {offset: target.offset, arity: params.len() });
            }
            Expression::MemberAccess { target, .. } => {
                self.lower_expr(&target.expr);
                self.push_instr(stack::Instr::ClassMemberLoad(target.offset));
            }
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

    fn push_instr(&mut self, instr: stack::Instr<BlockLocation>) {
        self.lowered.push_instr(instr);
    }

    fn start_block(&mut self) {
        self.lowered.start_block()
    }

    fn push_constant(&mut self, s: &str) -> usize {
        let idx = self.lowered.consts.len();
        self.lowered.consts.push(stack::ConstVal::Str(s.to_owned()));
        idx
    }
}
