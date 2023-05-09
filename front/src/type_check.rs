use std::collections::HashMap;

use crate::annotated_ast::{ChocoTyped, FunId, Function, Param};
use crate::ast::Type;
use crate::{annotated_ast, ast};

#[derive(Debug, PartialEq, Clone)]
pub enum ChocoType {
    Int,
    Bool,
    Str,
    /// the unmentionable type
    None,
    Empty,
    List(Box<ChocoType>), 
}
impl ChocoType {
    fn is_ref(&self) -> bool {
        match self {
            ChocoType::Str => true,
            ChocoType::None => true,
            ChocoType::List(_) => true,
            _ => false,
        }
    }

    fn is_list(&self) -> bool {
        match self {
            ChocoType::List(_) => true,
            _ => false
        }
    }

    fn list_inner_type(&self) -> Result<ChocoType, TypeError> {
        match self {
            ChocoType::List(t) => Ok(*t.clone()),
            _ => Err(TypeError::NotAList)
        }
    }
}
impl ChocoTyped for ChocoType {
    fn choco_type(&self) -> ChocoType {
        self.clone()
    }
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch {
        expected: ChocoType,
        actual: ChocoType,
    },
    Todo,
    NotBound(String),
    EmptyTargets,
    CannotRebindLocal(String),
    ExpectedRef,
    CannotTreatFunsAsLocals,
    NotBoundAsVar,
    ReturnOutsideOfFunction,
    WrongNumberOfArugments,
    CannotIndexIntoNonList,
    CannotIterateOverNonList,
    NotAList,
}
impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for TypeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

struct TypeEnvironment {
    bindings: HashMap<String, TypeBinding>,
}
impl TypeEnvironment {
    pub fn new() -> TypeEnvironment {
        TypeEnvironment {
            bindings: HashMap::new(),
        }
    }
}

pub enum TypeBinding {
    V(annotated_ast::Var),
    Fun {
        params: Vec<ChocoType>,
        result: ChocoType,
    },
}
impl TypeBinding {
    fn as_var(&self) -> Result<&annotated_ast::Var, TypeError> {
        match self {
            TypeBinding::V(v) => Ok(v),
            _ => Err(TypeError::NotBoundAsVar),
        }
    }
}

pub struct TypeChecker {
    environments: Vec<TypeEnvironment>,
    funs: Vec<Function>,
    program: annotated_ast::Program,
}
impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            environments: vec![TypeEnvironment::new()],
            funs: vec![],
            program: annotated_ast::Program::new(),
        }
    }

    fn match_type<C: ChocoTyped>(expected: ChocoType, actual: C) -> Result<C, TypeError> {
        if expected == actual.choco_type() {
            Ok(actual)
        } else {
            Err(TypeError::TypeMismatch {
                expected,
                actual: actual.choco_type(),
            })
        }
    }

    pub fn check_expression(
        &self,
        e: &ast::Expression,
    ) -> Result<annotated_ast::Expression, TypeError> {
        match e {
            ast::Expression::Not(b) => {
                let b = TypeChecker::match_type(ChocoType::Bool, self.check_expression(b)?)?;
                Ok(annotated_ast::Expression::Unary {
                    op: annotated_ast::UnaryOp::LogicalNot,
                    e: Box::new(b),
                    choco_type: ChocoType::Bool,
                })
            }
            ast::Expression::BinaryOp(ast::BinOp::And, l, r) => {
                let al = TypeChecker::match_type(ChocoType::Bool, self.check_expression(l)?)?;
                let ar = TypeChecker::match_type(ChocoType::Bool, self.check_expression(r)?)?;
                Ok(annotated_ast::Expression::Ternary {
                    cond: Box::new(annotated_ast::Expression::Unary { op: annotated_ast::UnaryOp::LogicalNot, e: Box::new(al), choco_type: ChocoType::Bool }),
                    then: Box::new(annotated_ast::Expression::Lit { l: annotated_ast::Literal::False, choco_type: ChocoType::Bool }),
                    els: Box::new(ar),
                    choco_type: ChocoType::Bool,
                })
            }
            ast::Expression::BinaryOp(ast::BinOp::Or, l, r) => {
                let al = TypeChecker::match_type(ChocoType::Bool, self.check_expression(l)?)?;
                let ar = TypeChecker::match_type(ChocoType::Bool, self.check_expression(r)?)?;
                Ok(annotated_ast::Expression::Ternary {
                    cond: Box::new(al),
                    then: Box::new(annotated_ast::Expression::Lit { l: annotated_ast::Literal::True, choco_type: ChocoType::Bool }),
                    els: Box::new(ar),
                    choco_type: ChocoType::Bool,
                })
            }
            ast::Expression::Ternary {
                e,
                if_expr,
                else_expr,
            } => {
                let cond =
                    TypeChecker::match_type(ChocoType::Bool, self.check_expression(if_expr)?)?;
                let al = self.check_expression(e)?;
                let ar = self.check_expression(else_expr)?;

                // todo: this should actually be finding the "join" of l and r types.
                let res_type = al.choco_type();
                if al.choco_type() == ar.choco_type() {
                    Ok(annotated_ast::Expression::Ternary {
                        cond: Box::new(cond),
                        then: Box::new(al),
                        els: Box::new(ar),
                        choco_type: res_type,
                    })
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: al.choco_type(),
                        actual: ar.choco_type(),
                    })
                }
            }

            ast::Expression::Lit(lit) => {
                let (l, choco_type) = match lit {
                    ast::Literal::None => (annotated_ast::Literal::None, ChocoType::None),
                    ast::Literal::True => (annotated_ast::Literal::True, ChocoType::Bool),
                    ast::Literal::False => (annotated_ast::Literal::False, ChocoType::Bool),
                    ast::Literal::Integer(i) => (annotated_ast::Literal::Integer(*i), ChocoType::Int),
                    ast::Literal::Str(s) => (annotated_ast::Literal::Str(s.to_owned()), ChocoType::Str),
                    ast::Literal::IdStr(s) => (annotated_ast::Literal::Str(s.name.to_owned()), ChocoType::Str),
                    ast::Literal::List(es) if es.is_empty() => (annotated_ast::Literal::List(vec![]), ChocoType::Empty),
                    ast::Literal::List(es) => {
                        let es = self.check_exprs(es)?;
                        //TODO: compute join of types
                        let ty = es[0].choco_type();
                        (annotated_ast::Literal::List(es), ChocoType::List(Box::new(ty)))
                    }
                };
                Ok(annotated_ast::Expression::Lit { l, choco_type })
            }
            ast::Expression::BinaryOp(op, l, r) => {
                let al = self.check_expression(l)?;
                let ar = self.check_expression(r)?;
                if al.choco_type() != ar.choco_type() {
                    return Err(TypeError::TypeMismatch {
                        expected: al.choco_type(),
                        actual: ar.choco_type(),
                    });
                };
                let rel_type = al.choco_type();
                let res_type = match op {
                    ast::BinOp::Plus => match rel_type {
                        ChocoType::Int | ChocoType::Str | ChocoType::List(_) => Ok(rel_type),
                        _ => Err(TypeError::TypeMismatch {
                            expected: ChocoType::Int,
                            actual: rel_type,
                        }),
                    },
                    ast::BinOp::Minus
                    | ast::BinOp::Multiply
                    | ast::BinOp::IntegerDiv
                    | ast::BinOp::Modulo => {
                        if rel_type != ChocoType::Int {
                            return Err(TypeError::TypeMismatch {
                                expected: ChocoType::Int,
                                actual: rel_type,
                            });
                        }
                        Ok(ChocoType::Int)
                    }
                    ast::BinOp::Equals | ast::BinOp::NotEquals => Ok(ChocoType::Bool),
                    ast::BinOp::LessThan
                    | ast::BinOp::LessThanEqual
                    | ast::BinOp::GreaterThan
                    | ast::BinOp::GreaterThanEqual => {
                        if rel_type != ChocoType::Int {
                            return Err(TypeError::TypeMismatch {
                                expected: ChocoType::Int,
                                actual: rel_type,
                            });
                        }
                        Ok(ChocoType::Bool)
                    }

                    ast::BinOp::Is => {
                        if rel_type.is_ref() {
                            Ok(ChocoType::Bool)
                        } else {
                            Err(TypeError::ExpectedRef)
                        }
                    }
                    ast::BinOp::And | ast::BinOp::Or => unreachable!(),
                }?;
                Ok(annotated_ast::Expression::Binary {
                    op: *op,
                    l: Box::new(al),
                    r: Box::new(ar),
                    choco_type: res_type,
                })
            }
            ast::Expression::UnaryMinus(n) => {
                let n = TypeChecker::match_type(ChocoType::Int, self.check_expression(n)?)?;
                Ok(annotated_ast::Expression::Binary {
                    op: ast::BinOp::Minus,
                    l: Box::new(annotated_ast::Expression::Lit {
                        l: annotated_ast::Literal::Integer(0),
                        choco_type: ChocoType::Int
                    }),
                    r: Box::new(n),
                    choco_type: ChocoType::Int,
                })
            }
            // hack in support for print
            ast::Expression::Call(id, es) if id.name == "print" => {
                let es = self.check_exprs(es)?;
                let e = TypeChecker::exactly_one_param(es)?;
                Ok(annotated_ast::Expression::Call {
                    f: annotated_ast::FunId {
                        name: "host_print".to_owned(),
                    },
                    params: vec![e],
                    native: true,
                    choco_type: ChocoType::None,
                })
            }
            ast::Expression::Call(id, es) if id.name == "len" => {
                let es = self.check_exprs(es)?;
                let e = TypeChecker::exactly_one_param(es)?;
                Ok(annotated_ast::Expression::Call {
                    f: annotated_ast::FunId {
                        name: match e.choco_type() {
                            ChocoType::Str => "len_str".to_owned(),
                            ChocoType::List(_) => "len_list".to_owned(),
                            t => return Err(TypeError::TypeMismatch { expected: ChocoType::Str, actual: t })
                        }
                    },
                    params: vec![e],
                    native: true,
                    choco_type: ChocoType::None,
                })
            }
            ast::Expression::Call(id, es) => {
                let fun = self.get_fun(&id.name)?;

                let es = self.check_exprs(es)?;
                for (e, p) in es.iter().zip(fun.params.iter()) {
                    if e.choco_type() != p.choco_type {
                        return Err(TypeError::TypeMismatch {
                            expected: p.choco_type.clone(),
                            actual: e.choco_type(),
                        });
                    }
                }

                Ok(annotated_ast::Expression::Call {
                    f: FunId {
                        name: fun.name.clone(),
                    },
                    params: es,
                    native: false,
                    choco_type: fun.return_type.clone(),
                })
            }
            // lists
            ast::Expression::ListLiteral(_) => todo!(),

            // require environment
            ast::Expression::Id(id) => self
                .get_local(&id.name)
                .map(|v| annotated_ast::Expression::Load { v: v.clone() }),
            ast::Expression::Member(_) => todo!(),
            ast::Expression::Index(ast::IndexExpression { expr, index }) => {
                let access = self.check_expression(expr)?;
                let index = TypeChecker::match_type(ChocoType::Int, self.check_expression(index)?)?;
                match access.choco_type() {
                    ChocoType::Str => Ok(annotated_ast::Expression::Index {
                        expr: Box::new(access),
                        index: Box::new(index),
                    }),
                    ChocoType::List(_) => Ok(annotated_ast::Expression::Index {
                        expr: Box::new(access),
                        index: Box::new(index),
                    }),
                    t => Err(TypeError::TypeMismatch {
                        expected: ChocoType::Str,
                        actual: t,
                    }),
                }
            }
            ast::Expression::MemberCall(_, _) => todo!(),
        }
    }

    pub fn check_exprs(
        &self,
        es: &[ast::Expression],
    ) -> Result<Vec<annotated_ast::Expression>, TypeError> {
        let mut ann_es = vec![];
        for e in es {
            ann_es.push(self.check_expression(e)?)
        }
        Ok(ann_es)
    }

    pub fn resolve_target(&self, t: &ast::Target) -> Result<annotated_ast::Lhs, TypeError> {
        match t {
            ast::Target::Id(name) => Ok(annotated_ast::Lhs::Var(self.get_local(&name.name)?.clone())),
            ast::Target::Member(_) => Err(TypeError::Todo),
            ast::Target::Index(access) => {
                let list = self.check_expression(&access.expr)?;
                let inner_type = list.choco_type().list_inner_type()?;
                let index = TypeChecker::match_type(ChocoType::Int, self.check_expression(&access.index)?)?;
                Ok(annotated_ast::Lhs::Index{ list: Box::new(list), index: Box::new(index), choco_type: inner_type })
            }
        }
    }

    pub fn check_stmt(
        &mut self,
        p: &ast::Statement,
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        match p {
            ast::Statement::Expr(e) => Ok(vec![annotated_ast::Statement::Expr(
                self.check_expression(e)?,
            )]),
            ast::Statement::Pass => Ok(vec![]),
            ast::Statement::Return(e) => {
                let expected_type = self.current_fun().ok_or(TypeError::ReturnOutsideOfFunction)?.return_type.clone();

                if let Some(e) = e {
                    let e = self.check_expression(e)?;
                    TypeChecker::match_type(expected_type, e.choco_type())?;
                    Ok(vec![annotated_ast::Statement::Return(Some(Box::new(e)))])
                } else {
                    TypeChecker::match_type(expected_type, ChocoType::None)?;
                    Ok(vec![annotated_ast::Statement::Return(Some(Box::new(annotated_ast::Expression::Lit { l: annotated_ast::Literal::None, choco_type: ChocoType::None })))])
                }
            }
            ast::Statement::Assign { targets, expr } => {
                let e = self.check_expression(expr)?;
                let mut lhs = vec![];
                if targets.is_empty() {
                    return Err(TypeError::EmptyTargets)
                }
                for target in targets {
                    let tlhs = self.resolve_target(target)?.clone();
                    self.is_assignable(tlhs.choco_type(), e.choco_type())?;
                    lhs.push(tlhs);
                }
                Ok(vec![annotated_ast::Statement::Assign(lhs, e)])
            }
            ast::Statement::If {
                main,
                elifs,
                otherwise,
            } => {
                let cond = self.check_expression(&main.condition)?;
                if cond.choco_type() != ChocoType::Bool {
                    return Err(TypeError::TypeMismatch {
                        expected: ChocoType::Bool,
                        actual: cond.choco_type(),
                    });
                }
                let then = self.check_stmts(&main.then)?;
                let mut els = if let Some(stmts) = otherwise {
                    self.check_stmts(&stmts)?
                } else {
                    vec![]
                };
                for cond_block in elifs.iter().rev() {
                    let cond = self.check_expression(&cond_block.condition)?;
                    if cond.choco_type() != ChocoType::Bool {
                        return Err(TypeError::TypeMismatch {
                            expected: ChocoType::Bool,
                            actual: cond.choco_type(),
                        });
                    }
                    let then = self.check_stmts(&cond_block.then)?;
                    let nested = vec![annotated_ast::Statement::If { cond, then, els }];
                    els = nested;
                }

                Ok(vec![annotated_ast::Statement::If { cond, then, els }])
            }
            ast::Statement::While(cond_block) => {
                let cond = self.check_expression(&cond_block.condition)?;
                if cond.choco_type() != ChocoType::Bool {
                    return Err(TypeError::TypeMismatch {
                        expected: ChocoType::Bool,
                        actual: cond.choco_type(),
                    });
                }
                let stmts = self.check_stmts(&cond_block.then)?;
                Ok(vec![annotated_ast::Statement::While { cond, stmts }])
            }
            ast::Statement::For { id, in_expr, block } => {
                let list = self.check_expression(in_expr)?;
                let inner_type = list.choco_type().list_inner_type()?;

                let idx = "$idx_".to_owned() + id.name.as_str();
                let idx_local = self.add_local(idx, ChocoType::Int)?.clone();

                let in_name = "$in_".to_owned() + id.name.as_str();
                let in_local = self.add_local(in_name, list.choco_type())?.clone();

                let len = "$len_".to_owned() + id.name.as_str();
                let len_local = self.add_local(len, ChocoType::Int)?.clone();

                let val_local = self.add_local(id.name.clone(), inner_type)?.clone();

                let mut desugar_stmts = vec![
                    annotated_ast::Statement::Assign(
                        vec![annotated_ast::Lhs::Var(val_local.clone())],
                        annotated_ast::Expression::Index {
                            expr: Box::new(annotated_ast::Expression::Load { v: in_local.clone() }),
                            index: Box::new(annotated_ast::Expression::Load { v: idx_local.clone() })
                        }
                    )
                ];
                for stmt in block {
                    let mut stmt = self.check_stmt(stmt)?;
                    desugar_stmts.append(&mut stmt);
                }
                desugar_stmts.push(annotated_ast::Statement::Assign(
                    vec![annotated_ast::Lhs::Var(idx_local.clone())],
                    annotated_ast::Expression::Binary {
                        op: ast::BinOp::Plus,
                        l: Box::new(annotated_ast::Expression::Load { v: idx_local.clone() }),
                        r: Box::new(annotated_ast::Expression::Lit { l: annotated_ast::Literal::Integer(1), choco_type: ChocoType::Int}),
                        choco_type: ChocoType::Int
                    }
                ));
                Ok(vec![
                    annotated_ast::Statement::Declare(
                        idx_local.clone(),
                        annotated_ast::Expression::Lit { l: annotated_ast::Literal::Integer(0), choco_type: ChocoType::Int },
                    ),
                    annotated_ast::Statement::Declare(
                        in_local.clone(),
                        list
                    ),
                    annotated_ast::Statement::Declare(
                        len_local.clone(),
                        annotated_ast::Expression::Call { f: FunId { name: "len_list".to_owned() }, params: vec![annotated_ast::Expression::Load { v: in_local }], choco_type: ChocoType::Int, native: true }
                    ),
                    annotated_ast::Statement::Declare(
                        val_local.clone(),
                        annotated_ast::Expression::Lit { l: annotated_ast::Literal::None, choco_type: ChocoType::None }
                    ),
                    annotated_ast::Statement::While {
                        cond: annotated_ast::Expression::Binary { 
                            op: ast::BinOp::LessThan, 
                            l: Box::new(annotated_ast::Expression::Load { v: idx_local }),
                            r: Box::new(annotated_ast::Expression::Load { v: len_local }),
                            choco_type: ChocoType::Bool,
                        },
                        stmts: desugar_stmts
                    }
                ])
            }
        }
    }

    pub fn check_stmts(
        &mut self,
        s: &[ast::Statement],
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        let mut stmts = vec![];
        for stmt in s {
            stmts.append(&mut self.check_stmt(stmt)?);
        }
        Ok(stmts)
    }

    fn resolve_type(&self, typ: &ast::Type) -> Result<ChocoType, TypeError> {
        match typ {
            Type::Id(id) => match id.name.as_str() {
                "bool" => Ok(ChocoType::Bool),
                "int" => Ok(ChocoType::Int),
                "str" => Ok(ChocoType::Str),
                _ => Err(TypeError::Todo),
            },
            Type::List(ty) => Ok(ChocoType::List(Box::new(self.resolve_type(ty)?)))
        }
    }

    fn check_var_def(
        &mut self,
        def: &ast::VariableDef,
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        let bound_type = self.resolve_type(&def.var.typ)?;
        let init_lit = self.check_expression(&ast::Expression::Lit(def.literal.clone()))?;
        let init = init_lit.choco_type();
        if bound_type != init {
            return Err(TypeError::TypeMismatch {
                expected: bound_type,
                actual: init,
            });
        }
        let v = self.add_local(def.var.id.name.clone(), bound_type)?.clone();
        Ok(vec![annotated_ast::Statement::Declare(
            v,
            init_lit,
        )])
    }

    fn check_def(
        &mut self,
        def: &ast::Definition,
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        match def {
            ast::Definition::Var(def) => self.check_var_def(&def),
            ast::Definition::Func(fun) => {
                let mut params = vec![];
                for p in &fun.params {
                    params.push(Param {
                        name: p.id.name.clone(),
                        choco_type: self.resolve_type(&p.typ)?,
                    })
                }
                self.start_fun(
                    fun.id.name.clone(),
                    params.clone(),
                    fun.return_type
                        .clone()
                        .map(|t| self.resolve_type(&t))
                        .unwrap_or(Ok(ChocoType::None))?,
                )?;
                self.push_environment();
                for param in &params {
                    self.add_local(param.name.clone(), param.choco_type.clone())?;
                }
                // todo: &fun.decls
                for var in &fun.vars {
                    self.check_var_def(var)?;
                }

                for stmt in &fun.stmts {
                    let mut stmts = self.check_stmt(stmt)?;
                    self.append_statements(&mut stmts)
                }

                self.pop_environment();
                self.finish_fun();

                // do I need a CFG to determine if this function returns?
                Ok(vec![])
            }
            ast::Definition::Class(_) => todo!(),
        }
    }

    pub fn check_prog(&mut self, p: &ast::Program) -> Result<&annotated_ast::Program, TypeError> {
        self.start_fun("entry".to_owned(), vec![], ChocoType::None)?;
        for def in &p.defs {
            let stmt = &mut self.check_def(def)?;
            self.append_statements(stmt);
        }
        for stmt in &p.stmts {
            let stmt = &mut self.check_stmt(&stmt)?;
            self.append_statements(stmt);
        }
        self.finish_fun();
        Ok(&self.program)
    }

    fn get_local(&self, name: &str) -> Result<&annotated_ast::Var, TypeError> {
        self.environments
            .last()
            .unwrap()
            .bindings
            .get(name)
            .ok_or(TypeError::NotBound(name.to_owned()))
            .and_then(|tb| match tb {
                TypeBinding::V(v) => Ok(v),
                TypeBinding::Fun { .. } => Err(TypeError::CannotTreatFunsAsLocals),
            })
    }

    fn add_local(
        &mut self,
        name: String,
        choco_type: ChocoType,
    ) -> Result<&annotated_ast::Var, TypeError> {
        if self
            .environments
            .last()
            .unwrap()
            .bindings
            .contains_key(&name)
        {
            Err(TypeError::CannotRebindLocal(name.clone()))
        } else {
            let v = annotated_ast::Var::Local {
                name: name.clone(),
                choco_type,
            };
            self.environments
                .last_mut()
                .unwrap()
                .bindings
                .insert(name.clone(), TypeBinding::V(v));
            self.environments.last().unwrap().bindings[&name].as_var()
        }
    }

    fn push_environment(&mut self) {
        self.environments.push(TypeEnvironment::new())
    }

    fn pop_environment(&mut self) {
        self.environments.pop();
    }

    fn start_fun(
        &mut self,
        name: String,
        params: Vec<annotated_ast::Param>,
        result_type: ChocoType,
    ) -> Result<(), TypeError> {
        // todo: should fail on name collision
        Ok(self.funs.push(Function::new(name, params, result_type)))
    }

    fn finish_fun(&mut self) {
        let mut f = self.funs.pop().expect("function stack empty");
        if f.return_type == ChocoType::None && f.name != "entry" {
            f.body.push(annotated_ast::Statement::Return(Some(Box::new(annotated_ast::Expression::Lit { l: annotated_ast::Literal::None, choco_type: ChocoType::None }))));
        }
        self.program.funs.push(f);
    }

    fn current_fun(&self) -> Option<&annotated_ast::Function> {
        self.funs.last().filter(|f| f.name != "entry")
    }

    fn append_statements(&mut self, stmts: &mut Vec<annotated_ast::Statement>) {
        self.funs.last_mut().unwrap().body.append(stmts)
    }

    fn get_fun(&self, name: &str) -> Result<&annotated_ast::Function, TypeError> {
        let f = { |fun: &&annotated_ast::Function| fun.name == name };
        self.funs
            .iter()
            .find(f)
            .or_else(|| self.program.funs.iter().find(f))
            .map(|f| Ok(f))
            .unwrap_or(Err(TypeError::NotBound(name.to_owned())))
    }

    fn exactly_one_param(es: Vec<annotated_ast::Expression>) -> Result<annotated_ast::Expression, TypeError> {
        if es.len() == 1 {
            Ok(es[0].clone())
        } else {
            Err(TypeError::WrongNumberOfArugments)
        }
    }

    fn is_assignable(&self, to: ChocoType, from: ChocoType) -> Result<(), TypeError> {
        if to != from {
            Err(TypeError::TypeMismatch { expected: to, actual: from })
        } else {
            Ok(())
        }
    }
        
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        annotated_ast::{self, ChocoTyped},
        lexer::Lexer,
        parser::Parser,
    };

    use super::{ChocoType, TypeChecker};

    fn parse_and_type_check(input: &str) -> Result<ChocoType, Box<dyn Error>> {
        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse()?;

        let mut typeck = TypeChecker::new();
        let ann_prog = typeck.check_prog(&prog)?;
        let entry = ann_prog.funs.iter().find(|f| f.name == "entry");
        if let Some(annotated_ast::Statement::Expr(e)) = entry.unwrap().body.last()
        {
            Ok(e.choco_type())
        } else {
            Ok(ChocoType::None)
        }
    }

    fn assert_fails_type_check(input: &str) {
        match parse_and_type_check(input) {
            Ok(_) => panic!("Expected type check failure for: {}", input),
            Err(_) => (),
        }
    }

    #[test]
    fn test_type_checker_literal_expressions() {
        assert_eq!(parse_and_type_check("1 + 2").unwrap(), ChocoType::Int);
        assert_eq!(parse_and_type_check("1 + 2").unwrap(), ChocoType::Int);
        assert_eq!(parse_and_type_check("1 + 2 * 3").unwrap(), ChocoType::Int);
        assert_eq!(parse_and_type_check("-1").unwrap(), ChocoType::Int);
        assert_eq!(parse_and_type_check("not True").unwrap(), ChocoType::Bool);
        assert_eq!(
            parse_and_type_check("True and True").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("True or True").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("True and True or False").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(parse_and_type_check("1 > 2").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("1 == 2").unwrap(), ChocoType::Bool);
        assert_eq!(
            parse_and_type_check("True == False").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("1 if True else 2").unwrap(),
            ChocoType::Int
        );
        assert_eq!(
            parse_and_type_check("False if True else True").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("\"foo\" if True else \"bar\"").unwrap(),
            ChocoType::Str
        );
        //todo: assignability assert_eq!(parse_and_type_check("1 if True else None").unwrap(), ChocoType::Int);
        assert_fails_type_check("1 if True else False");
        assert_fails_type_check("1 if 2 else 3");
        assert_fails_type_check("True + True");
        assert_fails_type_check("True is True");
        assert_fails_type_check("1 and 2");
        assert_fails_type_check("(1 + 2) and 3");
    }

    #[test]
    fn test_locals() {
        assert_eq!(
            parse_and_type_check("x: bool = False\nx").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("x: bool = False\nx and True").unwrap(),
            ChocoType::Bool
        );
        assert_eq!(
            parse_and_type_check("x: int = 0\nx + 0").unwrap(),
            ChocoType::Int
        );
        assert_fails_type_check("x: bool = True\nx + 0");
        assert_fails_type_check("x: bool = 0\nx and True");
    }

    #[test]
    fn test_control_flow() {
        parse_and_type_check("if True:\n  print(1)\nelse:\n  print(2)").unwrap();
        parse_and_type_check("if True:\n  1\nelif False:\n  2\nelse:\n  3").unwrap();
        assert_fails_type_check("if 0:\n  print(1)");
    }

    #[test]
    fn test_functions() {
        assert_eq!(
            parse_and_type_check("def f(x: int) -> int:\n  return x\nf(1)").unwrap(),
            ChocoType::Int
        );
        assert_fails_type_check("return");
        assert_fails_type_check("def f() -> int\n  return False");
    }

    #[test]
    fn test_assignment_with_none() {
        assert_fails_type_check("x: str = None");
        assert_fails_type_check("x: int = None");
        assert_fails_type_check("x: bool = None");
    }

    #[test]
    fn test_list_assignment() {
        parse_and_type_check("l: [int] = [1,2,3]\nl[0] = 0").unwrap();
    }

    #[test]
    fn test_assignability_analysis() {
        assert_fails_type_check("x: [int] = []"); //TODO: this should succeed!
    }
}
