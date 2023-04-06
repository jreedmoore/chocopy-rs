use crate::{ast, annotated_ast};
use crate::annotated_ast::{ChocoTyped, Expression};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ChocoType {
    Int,
    Bool,
    Str,
    None // the unmentionable type!
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    TypeMismatch { expected: ChocoType, actual: ChocoType }
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

pub struct TypeChecker {

}
impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {  }
    }

    fn match_type<C: ChocoTyped>(expected: ChocoType, actual: C) -> Result<C, TypeError> {
        if expected == actual.choco_type() {
            Ok(actual)
        } else {
            Err(TypeError::TypeMismatch { expected, actual: actual.choco_type() })
        }
    }

    pub fn check_expression(&self, e: &ast::Expression) -> Result<annotated_ast::Expression, TypeError> {
        match e {
            ast::Expression::Not(b) => {
                let b = TypeChecker::match_type(ChocoType::Bool, self.check_expression(b)?)?;
                Ok(annotated_ast::Expression::Unary { op: annotated_ast::UnaryOp::LogicalNot, e: Box::new(b), choco_type: ChocoType::Bool })
            }
            ast::Expression::BinaryOp(op @ (ast::BinOp::And | ast::BinOp::Or), l, r) => {
                let al = TypeChecker::match_type(ChocoType::Bool, self.check_expression(l)?)?;
                let ar = TypeChecker::match_type(ChocoType::Bool, self.check_expression(r)?)?;
                Ok(annotated_ast::Expression::Binary { op: *op, l: Box::new(al), r: Box::new(ar), choco_type: ChocoType::Bool })
            }
            ast::Expression::Ternary { e, if_expr, else_expr } => {
                let cond = TypeChecker::match_type(ChocoType::Bool, self.check_expression(if_expr)?)?;
                let al = self.check_expression(e)?;
                let ar = self.check_expression(else_expr)?;

                // todo: this should actually be finding the "join" of l and r types.
                let res_type = al.choco_type();
                if al.choco_type() == ar.choco_type() {
                    Ok(annotated_ast::Expression::Ternary { cond: Box::new(cond), then: Box::new(al), els: Box::new(ar), choco_type: res_type })
                } else {
                    Err(TypeError::TypeMismatch { expected: al.choco_type(), actual: ar.choco_type() })
                }
            }

            ast::Expression::Lit(lit) => Ok(annotated_ast::Expression::Lit { l: lit.clone() }),
            // what about lists?
            ast::Expression::BinaryOp(op, l, r) => {
                let al = self.check_expression(l)?;
                let ar = self.check_expression(r)?;
                if al.choco_type() != ar.choco_type() {
                    return Err(TypeError::TypeMismatch { expected: al.choco_type(), actual: ar.choco_type() })
                };
                let rel_type = al.choco_type();
                let res_type = match op {
                    ast::BinOp::Plus 
                    | ast::BinOp::Minus
                    | ast::BinOp::Multiply
                    | ast::BinOp::IntegerDiv
                    | ast::BinOp::Modulo => {
                        if rel_type != ChocoType::Int {
                            return Err(TypeError::TypeMismatch { expected: ChocoType::Int, actual: rel_type })
                        }
                        ChocoType::Int
                    },
                    ast::BinOp::Equals
                    | ast::BinOp::NotEquals => ChocoType::Bool,
                    ast::BinOp::LessThan
                    | ast::BinOp::LessThanEqual
                    | ast::BinOp::GreaterThan
                    | ast::BinOp::GreaterThanEqual => {
                        if rel_type != ChocoType::Int {
                            return Err(TypeError::TypeMismatch { expected: ChocoType::Int, actual: rel_type })
                        }
                        ChocoType::Bool
                    }

                    ast::BinOp::Is => todo!(),
                    ast::BinOp::And | ast::BinOp::Or => unreachable!(),
                };
                Ok(annotated_ast::Expression::Binary { op: *op, l: Box::new(al), r: Box::new(ar), choco_type: res_type })
            }
            ast::Expression::UnaryMinus(n) => {
                let n = TypeChecker::match_type(ChocoType::Int, self.check_expression(n)?)?;
                Ok(annotated_ast::Expression::Binary { op: ast::BinOp::Minus, l: Box::new(annotated_ast::Expression::Lit { l: ast::Literal::Integer(0)}), r: Box::new(n), choco_type: ChocoType::Int })
            }
            // hack in support for print
            ast::Expression::Call(id, es) if id.name == "print" => {
                let es = self.check_exprs(es)?;
                let e = es[0].clone();
                Ok(annotated_ast::Expression::Call { f: annotated_ast::FunId { name: "host_print".to_string() }, params: vec![e], choco_type: ChocoType::None })
            }
            // lists
            ast::Expression::ListLiteral(_) => todo!(),

            // require environment
            ast::Expression::Id(_) => todo!(),
            ast::Expression::Member(_) => todo!(),
            ast::Expression::Index(_) => todo!(),
            ast::Expression::MemberCall(_, _) => todo!(),
            ast::Expression::Call(_, _) => todo!(),
        }
    }

    pub fn check_exprs(&self, es: &[ast::Expression]) -> Result<Vec<annotated_ast::Expression>, TypeError> {
        let mut ann_es = vec![];
        for e in es {
            ann_es.push(self.check_expression(e)?)
        } 
        Ok(ann_es)
    }

    pub fn check_stmt(&self, p: &ast::Statement) -> Result<annotated_ast::Statement, TypeError> {
        match p {
            ast::Statement::Expr(e) => Ok(annotated_ast::Statement::Expr(self.check_expression(e)?)),
            ast::Statement::Pass => todo!(),
            ast::Statement::Return(_) => todo!(),
            ast::Statement::Assign { targets, expr } => todo!(),
            ast::Statement::If { main, elifs, otherwise } => todo!(),
            ast::Statement::While(_) => todo!(),
            ast::Statement::For { id, in_expr, block } => todo!(),
        }
    }

    pub fn check_prog(&self, p: &ast::Program) -> Result<annotated_ast::Program, TypeError> {
        let mut stmts = vec![];
        for stmt in &p.stmts {
            stmts.push(self.check_stmt(&stmt)?)
        }
        Ok(annotated_ast::Program { stmts })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{parser::Parser, lexer::Lexer, ast, annotated_ast::{ChocoTyped, self}};

    use super::{TypeChecker, ChocoType};

    fn parse_and_type_check(input: &str) -> Result<ChocoType, Box<dyn Error>> {
        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse()?;

        let typeck = TypeChecker::new();
        let ann_prog = typeck.check_prog(&prog)?;
        if let Some(annotated_ast::Statement::Expr(e)) = ann_prog.stmts.first() {
            Ok(e.choco_type())
        } else {
            panic!("Not an expression program")
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
        assert_eq!(parse_and_type_check("True and True").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("True or True").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("True and True or False").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("1 > 2").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("1 == 2").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("True == False").unwrap(), ChocoType::Bool);
        // todo: assert_eq!(parse_and_type_check("1 if True else 2").unwrap(), ChocoType::Int);
        // todo: assert_eq!(parse_and_type_check("False if True else True").unwrap(), ChocoType::Bool);
        // todo: assert_eq!(parse_and_type_check("\"foo\" if True else \"bar\"").unwrap(), ChocoType::Str);
        // todo: assert_eq!(parse_and_type_check("1 if True else None").unwrap(), ChocoType::Int);
        assert_fails_type_check("1 if True else False");
        assert_fails_type_check("1 if 2 else 3");
        assert_fails_type_check("True + True");
        assert_fails_type_check("1 and 2");
        assert_fails_type_check("(1 + 2) and 3");
    }
}