use crate::{type_check::ChocoType, ast};

pub struct Program {
    pub stmts: Vec<Statement>
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary { op: ast::BinOp, l: Box<Expression>, r: Box<Expression>, choco_type: ChocoType },
    Call { f: FunId, params: Vec<Expression>, choco_type: ChocoType },
    Lit { l: ast::Literal },
    Unary { op: UnaryOp, e: Box<Expression>, choco_type: ChocoType },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    LogicalNot
}

#[derive(Debug, Clone)]
pub struct FunId {
    pub name: String
}

pub(crate) trait ChocoTyped {
    fn choco_type(&self) -> ChocoType;
}

impl ChocoTyped for Expression {
    fn choco_type(&self) -> ChocoType {
        match self {
            Expression::Binary { choco_type, .. } => *choco_type,
            Expression::Call { choco_type, .. } => *choco_type,

            Expression::Lit { l: ast::Literal::True | ast::Literal::False } => ChocoType::Bool,
            Expression::Lit { l: ast::Literal::Integer(_) } => ChocoType::Int,
            Expression::Lit { l: ast::Literal::None } => ChocoType::None,
            Expression::Lit { l: ast::Literal::Str(_) | ast::Literal::IdStr(_) } => ChocoType::Str,
            Expression::Unary { choco_type, .. } => *choco_type,
        }
    }
}
