use std::rc;

use crate::ast;

#[derive(Debug, PartialEq)]
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

    fn match_type(expected: ChocoType, actual: ChocoType) -> Result<ChocoType, TypeError> {
        if expected == actual {
            Ok(expected)
        } else {
            Err(TypeError::TypeMismatch { expected, actual })
        }
    }

    pub fn check_expression(&self, e: &ast::Expression) -> Result<ChocoType, TypeError> {
        match e {
            ast::Expression::Not(b) => TypeChecker::match_type(ChocoType::Bool, self.check_expression(b)?),
            ast::Expression::LogicalBinaryOp(_, l, r) => {
                TypeChecker::match_type(ChocoType::Bool, self.check_expression(l)?)?;
                TypeChecker::match_type(ChocoType::Bool, self.check_expression(r)?)?;
                Ok(ChocoType::Bool)
            }
            ast::Expression::Ternary { e, if_expr, else_expr } => {
                TypeChecker::match_type(ChocoType::Bool, self.check_expression(if_expr)?)?;
                let lctype = self.check_expression(e)?;
                let rctype = self.check_expression(else_expr)?;

                // todo: this should actually be finding the "join" of l and r types.
                if lctype == rctype {
                    Ok(lctype)
                } else {
                    Err(TypeError::TypeMismatch { expected: lctype, actual: rctype })
                }
            }

            ast::Expression::Lit(ast::Literal::False | ast::Literal::True) => Ok(ChocoType::Bool),
            ast::Expression::Lit(ast::Literal::Integer(_)) => Ok(ChocoType::Int),
            ast::Expression::Lit(ast::Literal::IdStr(_) | ast::Literal::Str(_)) => Ok(ChocoType::Str),
            ast::Expression::Lit(ast::Literal::None) => Ok(ChocoType::None),

            ast::Expression::BinaryOp(_, l, r) => {
                TypeChecker::match_type(ChocoType::Int, self.check_expression(l)?)?;
                TypeChecker::match_type(ChocoType::Int, self.check_expression(r)?)?;
                Ok(ChocoType::Int)
            }
            ast::Expression::UnaryMinus(n) => TypeChecker::match_type(ChocoType::Int, self.check_expression(n)?),

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
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{parser::Parser, lexer::Lexer, ast};

    use super::{TypeChecker, ChocoType};

    fn parse_and_type_check(input: &str) -> Result<ChocoType, Box<dyn Error>> {
        let mut p = Parser::new(Lexer::new(input));
        let prog = p.parse()?;

        if let Some(ast::Statement::Expr(e)) = prog.stmts.first() {
            let typeck = TypeChecker::new();
            let ctype = typeck.check_expression(e)?;
            Ok(ctype)
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
        assert_eq!(parse_and_type_check("1 if True else 2").unwrap(), ChocoType::Int);
        assert_eq!(parse_and_type_check("False if True else True").unwrap(), ChocoType::Bool);
        assert_eq!(parse_and_type_check("\"foo\" if True else \"bar\"").unwrap(), ChocoType::Str);
        // todo: assert_eq!(parse_and_type_check("1 if True else None").unwrap(), ChocoType::Int);
        assert_fails_type_check("1 if True else False");
        assert_fails_type_check("1 if 2 else 3");
        assert_fails_type_check("True + True");
        assert_fails_type_check("1 and 2");
        assert_fails_type_check("(1 + 2) and 3");
    }
}