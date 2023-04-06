#[derive(Debug)]
pub struct Program {
    pub defs: Vec<Definition>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Definition {
    Var(VariableDef),
    Func(FunctionDef),
    Class(ClassDef),
}

#[derive(Debug)]
pub struct ClassDef {
    pub id: Identifier,
    pub parent: Identifier,
    pub vars: Vec<VariableDef>,
    pub funcs: Vec<FunctionDef>,
}
#[derive(Debug)]
pub struct FunctionDef {
    pub id: Identifier,
    pub params: Vec<TypedVar>,
    pub return_type: Option<Type>,
    pub decls: Vec<Declaration>,
    pub vars: Vec<VariableDef>,
    pub funcs: Vec<FunctionDef>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug)]
pub enum Declaration {
    NonLocal(Identifier),
    Global(Identifier),
}
#[derive(Debug)]
pub struct TypedVar {
    pub id: Identifier,
    pub typ: Type,
}
#[derive(Debug)]
pub enum Type {
    Id(Identifier),
    List(Box<Type>),
}
#[derive(Debug)]
pub struct VariableDef {
    pub var: TypedVar,
    pub literal: Literal,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct ConditionalBlock {
    pub condition: Expression,
    pub then: Vec<Statement>,
}
#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i32),
    Str(String),
    IdStr(Identifier),
}

#[derive(Debug)]
pub enum Expression {
    Not(Box<Expression>),
    Ternary {
        e: Box<Expression>,
        if_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    // cexpression
    // feels little bit bad that we're throwing away structural guarantees from the grammar
    // but it's kind of annoying to deal with the CExpression breakdown
    Id(Identifier),
    Lit(Literal),
    ListLiteral(Vec<Expression>),
    Member(MemberExpression),
    Index(IndexExpression),
    MemberCall(MemberExpression, Vec<Expression>),
    Call(Identifier, Vec<Expression>),
    BinaryOp(BinOp, Box<Expression>, Box<Expression>),
    UnaryMinus(Box<Expression>),
}

#[derive(Debug)]
pub struct MemberExpression {
    pub expr: Box<Expression>,
    pub id: Identifier,
}
#[derive(Debug)]
pub struct IndexExpression {
    pub expr: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    And,
    Or,
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

#[derive(Debug)]
pub enum Target {
    Id(Identifier),
    Member(MemberExpression),
    Index(IndexExpression),
}
