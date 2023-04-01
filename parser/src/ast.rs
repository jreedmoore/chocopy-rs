pub struct Program {
    pub defs: Vec<Definition>,
    pub stmts: Vec<Statement>,
}

pub enum Definition {
    Var(VariableDef),
    Func(FunctionDef),
    Class(ClassDef),
}

pub struct ClassDef {
    pub id: Identifier,
    pub parent: Identifier,
    pub vars: Vec<VariableDef>,
    pub funcs: Vec<FunctionDef>,
}
pub struct FunctionDef {
    pub id: Identifier,
    pub params: Vec<TypedVar>,
    pub return_type: Option<Type>,
    pub decls: Vec<Declaration>,
    pub vars: Vec<VariableDef>,
    pub funcs: Vec<FunctionDef>,
    pub stmts: Vec<Statement>,
}

pub enum Declaration {
    NonLocal(Identifier),
    Global(Identifier),
}
pub struct TypedVar {
    pub id: Identifier,
    pub typ: Type,
}
pub enum Type {
    Id(Identifier),
    List(Box<Type>),
}
pub struct VariableDef {
    pub var: TypedVar,
    pub literal: Literal,
}

pub struct Identifier {
    pub name: String,
}

pub struct ConditionalBlock {
    pub condition: Expression,
    pub then: Vec<Statement>,
}
pub enum Statement {
    Pass,
    Expr(Expression),
    Return(Option<Expression>),
    Assign {
        targets: Vec<Target>,
        expr: Expression,
    },
    If {
        main: ConditionalBlock,
        elifs: Vec<ConditionalBlock>,
        otherwise: Option<Vec<Statement>>,
    },
    While(ConditionalBlock),
    For {
        id: Identifier,
        in_expr: Expression,
        block: Vec<Statement>,
    },
}

pub enum Literal {
    None,
    True,
    False,
    Integer(i32),
    Str(String),
    IdStr(Identifier),
}

pub enum Expression {
    C(CExpression),
    Not(Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Ternary {
        e: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
}
pub struct MemberExpression {
    pub expr: Box<CExpression>,
    pub id: Identifier,
}
pub struct IndexExpression {
    pub expr: Box<CExpression>,
    pub index: Box<Expression>,
}
pub enum CExpression {
    Id(Identifier),
    Lit(Literal),
    ListLiteral(Vec<Expression>),
    Member(MemberExpression),
    Index(IndexExpression),
    MemberCall(MemberExpression, Option<Vec<Expression>>),
    Call(Identifier, Option<Vec<Expression>>),
    BinaryOp(BinOp, Box<CExpression>, Box<CExpression>),
    Negate(Box<CExpression>),
}

pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    IntegerDiv,
    Modulo,
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Is,
}

pub enum Target {
    Id(Identifier),
    Member(MemberExpression),
    Index(IndexExpression),
}
