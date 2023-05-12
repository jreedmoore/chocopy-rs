use crate::{
    ast,
    type_check::ChocoType,
};

pub struct Program {
    pub funs: Vec<Function>,
    pub classes: Vec<Class>
}
impl Program {
    pub(crate) fn new() -> Program {
        Program { funs: vec![], classes: vec![] }
    }
}

pub struct Class {
    pub name: String,
    pub vars: Vec<(String, TyLiteral)>,
    pub methods: Vec<(String, FunId)>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    Assign(Vec<Lhs>, Expression),
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
    Return(Option<Box<Expression>>),
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
        native: bool,
        choco_type: ChocoType,
    },
    Lit {
        l: TyLiteral,
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
    Index {
        expr: Box<Expression>,
        index: Box<Expression>,
    },
    MemberCall {
        target: MemberExpression,
        params: Vec<Expression>,
        choco_type: ChocoType,
    },
    MemberAccess { target: MemberExpression, choco_type: ChocoType },
}

#[derive(Debug, Clone)]
pub struct MemberExpression {
    pub expr: Box<Expression>,
    pub offset: usize,
}
impl ChocoTyped for Expression {
    fn choco_type(&self) -> ChocoType {
        match self {
            Expression::Binary { choco_type, .. } => choco_type.clone(),
            Expression::Call { choco_type, .. } => choco_type.clone(),
            Expression::Lit { l } => l.choco_type(),
            Expression::Unary { choco_type, .. } => choco_type.clone(),
            Expression::Ternary { choco_type, .. } => choco_type.clone(),
            Expression::Load { v } => v.choco_type(),
            Expression::Index { expr, .. } => expr.choco_type(),
            Expression::MemberCall { choco_type, .. } => choco_type.clone(),
            Expression::MemberAccess { choco_type, .. } => choco_type.clone(),
        }
    }
}
#[derive(Debug, Clone)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i32),
    Str(String),
    List(Vec<Expression>)
}

#[derive(Debug, Clone)]
pub struct TyLiteral {
    pub l: Literal,
    pub choco_type: ChocoType
}
impl ChocoTyped for TyLiteral {
    fn choco_type(&self) -> ChocoType {
        self.choco_type.clone()
    }
}
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: ChocoType,
    pub body: Vec<Statement>,
}
#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub choco_type: ChocoType,
}
impl Function {
    pub(crate) fn new(name: String, params: Vec<Param>, return_type: ChocoType) -> Function {
        Function {
            name,
            params,
            return_type,
            body: vec![],
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
            Var::Local { choco_type, .. } => choco_type.clone()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Lhs {
    Var(Var),
    Index { list: Box<Expression>, index: Box<Expression>, choco_type: ChocoType },
    Member { expr: Box<Expression>, offset: usize, choco_type: ChocoType },
}
impl ChocoTyped for Lhs {
    fn choco_type(&self) -> ChocoType {
        match self {
            Lhs::Var(v) => v.choco_type(),
            Lhs::Index { choco_type, .. } => choco_type.clone(),
            Lhs::Member { choco_type, .. } => choco_type.clone(),
        }
    }
}

pub(crate) trait ChocoTyped {
    fn choco_type(&self) -> ChocoType;
}
