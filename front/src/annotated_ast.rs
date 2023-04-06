use crate::{type_check::ChocoType, ast};

pub struct Program {
    pub stmts: Vec<Statement>
}

pub enum Statement {
    Expr(Expression)
}

pub enum Expression {
    Binary { op: ast::BinOp, l: Box<Expression>, r: Box<Expression>, choco_type: ChocoType },
    Call { f: Box<Expression>, params: Vec<Expression>, choco_type: ChocoType },
    Lit { l: ast::Literal },
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
        }
    }
}
