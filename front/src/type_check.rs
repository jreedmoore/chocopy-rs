use std::collections::HashMap;

use crate::annotated_ast::{ChocoTyped, Expression};
use crate::ast::{Identifier, Type};
use crate::{annotated_ast, ast};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ChocoType {
    Int,
    Bool,
    Str,
    None, // the unmentionable type!
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
    bindings: HashMap<String, annotated_ast::Var>,
}
impl TypeEnvironment {
    pub fn new() -> TypeEnvironment {
        TypeEnvironment {
            bindings: HashMap::new(),
        }
    }
}

pub struct TypeChecker {
    environments: Vec<TypeEnvironment>,
}
impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            environments: vec![TypeEnvironment::new()],
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
            ast::Expression::BinaryOp(op @ (ast::BinOp::And | ast::BinOp::Or), l, r) => {
                let al = TypeChecker::match_type(ChocoType::Bool, self.check_expression(l)?)?;
                let ar = TypeChecker::match_type(ChocoType::Bool, self.check_expression(r)?)?;
                Ok(annotated_ast::Expression::Binary {
                    op: *op,
                    l: Box::new(al),
                    r: Box::new(ar),
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

            ast::Expression::Lit(lit) => Ok(annotated_ast::Expression::Lit { l: lit.clone() }),
            // what about lists?
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
                        ChocoType::Int | ChocoType::Str => Ok(rel_type),
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

                    ast::BinOp::Is => Ok(rel_type),
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
                        l: ast::Literal::Integer(0),
                    }),
                    r: Box::new(n),
                    choco_type: ChocoType::Int,
                })
            }
            // hack in support for print
            ast::Expression::Call(id, es) if id.name == "print" => {
                let es = self.check_exprs(es)?;
                let e = es[0].clone();
                Ok(annotated_ast::Expression::Call {
                    f: annotated_ast::FunId {
                        name: "host_print".to_string(),
                    },
                    params: vec![e],
                    choco_type: ChocoType::None,
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
                    t => Err(TypeError::TypeMismatch {
                        expected: ChocoType::Str,
                        actual: t,
                    }),
                }
            }
            ast::Expression::MemberCall(_, _) => todo!(),
            ast::Expression::Call(_, _) => todo!(),
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

    pub fn resolve_target(&self, t: &ast::Target) -> Result<&annotated_ast::Var, TypeError> {
        match t {
            ast::Target::Id(name) => self.get_local(&name.name),
            ast::Target::Member(_) => Err(TypeError::Todo),
            ast::Target::Index(_) => Err(TypeError::Todo),
        }
    }

    pub fn check_stmt(
        &self,
        p: &ast::Statement,
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        match p {
            ast::Statement::Expr(e) => Ok(vec![annotated_ast::Statement::Expr(
                self.check_expression(e)?,
            )]),
            ast::Statement::Pass => todo!(),
            ast::Statement::Return(_) => todo!(),
            ast::Statement::Assign { targets, expr } => {
                let e = self.check_expression(expr)?;
                let mut stmts = vec![];
                if let Some(fst) = targets.first() {
                    let var = self.resolve_target(fst)?;
                    stmts.push(annotated_ast::Statement::Assign(var.clone(), e));
                    for target in targets.iter().skip(1) {
                        let next_var = self.resolve_target(target)?;
                        stmts.push(annotated_ast::Statement::Assign(
                            next_var.clone(),
                            annotated_ast::Expression::Load { v: var.clone() },
                        ))
                    }
                    Ok(stmts)
                } else {
                    Err(TypeError::EmptyTargets)
                }
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
            ast::Statement::For { id, in_expr, block } => todo!(),
        }
    }

    pub fn check_stmts(
        &self,
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
            Type::List(_) => todo!(),
        }
    }

    fn check_def(
        &mut self,
        def: &ast::Definition,
    ) -> Result<Vec<annotated_ast::Statement>, TypeError> {
        match def {
            ast::Definition::Var(def) => {
                let bound_type = self.resolve_type(&def.var.typ)?;
                let init = def.literal.choco_type();
                if bound_type != init {
                    return Err(TypeError::TypeMismatch {
                        expected: bound_type,
                        actual: init,
                    });
                }
                let v = self.add_local(def.var.id.name.clone(), bound_type)?.clone();
                Ok(vec![annotated_ast::Statement::Declare(
                    v,
                    annotated_ast::Expression::Lit {
                        l: def.literal.clone(),
                    },
                )])
            }
            ast::Definition::Func(_) => todo!(),
            ast::Definition::Class(_) => todo!(),
        }
    }

    pub fn check_prog(&mut self, p: &ast::Program) -> Result<annotated_ast::Program, TypeError> {
        let mut stmts = vec![];
        for def in &p.defs {
            stmts.append(&mut self.check_def(def)?);
        }
        for stmt in &p.stmts {
            stmts.append(&mut self.check_stmt(&stmt)?)
        }
        Ok(annotated_ast::Program { stmts })
    }

    fn get_local(&self, name: &str) -> Result<&annotated_ast::Var, TypeError> {
        self.environments
            .last()
            .unwrap()
            .bindings
            .get(name)
            .ok_or(TypeError::NotBound(name.to_owned()))
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
                .insert(name.clone(), v);
            Ok(self
                .environments
                .last()
                .unwrap()
                .bindings
                .get(&name)
                .unwrap())
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
        if let Some(annotated_ast::Statement::Expr(e)) = ann_prog.stmts.last() {
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
}
