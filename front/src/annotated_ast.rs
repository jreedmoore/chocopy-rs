use crate::{ast, type_check::ChocoType};

pub struct Program {
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Assign(Var, Expression),
    Declare(Var, Expression),
    If {
        cond: Expression,
        then: Vec<Statement>,
        els: Vec<Statement>,
    },
    While {
        cond: Expression,
        stmts: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        op: ast::BinOp,
        l: Box<Expression>,
        r: Box<Expression>,
        choco_type: ChocoType,
    },
    Call {
        f: FunId,
        params: Vec<Expression>,
        choco_type: ChocoType,
    },
    Lit {
        l: ast::Literal,
    },
    Unary {
        op: UnaryOp,
        e: Box<Expression>,
        choco_type: ChocoType,
    },
    Ternary {
        cond: Box<Expression>,
        then: Box<Expression>,
        els: Box<Expression>,
        choco_type: ChocoType,
    },
    Load {
        v: Var,
    },
}
impl ChocoTyped for Expression {
    fn choco_type(&self) -> ChocoType {
        match self {
            Expression::Binary { choco_type, .. } => *choco_type,
            Expression::Call { choco_type, .. } => *choco_type,
            Expression::Lit { l } => l.choco_type(),
            Expression::Unary { choco_type, .. } => *choco_type,
            Expression::Ternary { choco_type, .. } => *choco_type,
            Expression::Load { v } => v.choco_type(),
        }
    }
}
impl ChocoTyped for ast::Literal {
    fn choco_type(&self) -> ChocoType {
        match self {
            ast::Literal::True | ast::Literal::False => ChocoType::Bool,
            ast::Literal::Integer(_) => ChocoType::Int,
            ast::Literal::None => ChocoType::None,
            ast::Literal::Str(_) | ast::Literal::IdStr(_) => ChocoType::Str,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    LogicalNot,
}

#[derive(Debug, Clone)]
pub struct FunId {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Var {
    Local { name: String, choco_type: ChocoType },
}
impl ChocoTyped for Var {
    fn choco_type(&self) -> ChocoType {
        match self {
            Var::Local { choco_type, .. } => *choco_type,
        }
    }
}

pub(crate) trait ChocoTyped {
    fn choco_type(&self) -> ChocoType;
}
