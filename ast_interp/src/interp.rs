use front::ast::{self, Expression, Literal, BinOp};

fn interp_exp(e: &Expression) -> ChocoVal {
    match e {
        Expression::Lit(Literal::Integer(i)) => ChocoVal::Int(*i),
        Expression::Lit(Literal::True) => ChocoVal::Bool(true),
        Expression::Lit(Literal::False) => ChocoVal::Bool(false),
        Expression::BinaryOp(BinOp::Plus, l, r) => ChocoVal::Int(eval_num_binop(l, r, |l,r| l + r)),
        Expression::BinaryOp(BinOp::Multiply, l, r) => ChocoVal::Int(eval_num_binop(l, r, |l,r| l * r)),
        Expression::BinaryOp(BinOp::IntegerDiv, l, r) => ChocoVal::Int(eval_num_binop(l, r, |l,r| l / r)),
        Expression::BinaryOp(BinOp::Modulo, l, r) => ChocoVal::Int(eval_num_binop(l, r, |l,r| l % r)),
        Expression::BinaryOp(BinOp::GreaterThan, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l > r)),
        Expression::BinaryOp(BinOp::LessThan, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l < r)),
        Expression::BinaryOp(BinOp::GreaterThanEqual, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l >= r)),
        Expression::BinaryOp(BinOp::LessThanEqual, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l <= r)),
        Expression::BinaryOp(BinOp::Equals, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l == r)),
        Expression::BinaryOp(BinOp::NotEquals, l, r) => ChocoVal::Bool(eval_num_compare(l, r, |l,r| l != r)),
        Expression::LogicalBinaryOp(ast::LogicalBinOp::And, l, r) => ChocoVal::Bool(expect_bool(interp_exp(l)) && expect_bool(interp_exp(r))),
        Expression::LogicalBinaryOp(ast::LogicalBinOp::Or, l, r) => ChocoVal::Bool(expect_bool(interp_exp(l)) || expect_bool(interp_exp(r))),
        Expression::Not(e) => ChocoVal::Bool(!expect_bool(interp_exp(e))),
        Expression::Ternary { e, if_expr, else_expr } => 
            if expect_bool(interp_exp(if_expr)) {
                interp_exp(e)
            } else {
                interp_exp(else_expr)
            }
        _ => todo!()
    }
}

fn eval_num_binop<F>(l: &Expression, r: &Expression, f: F) -> i32 
    where F: Fn(i32, i32) -> i32
{
    f(expect_int(interp_exp(l)), expect_int(interp_exp(r)))
}

fn eval_num_compare<F>(l: &Expression, r: &Expression, f: F) -> bool 
    where F: Fn(i32, i32) -> bool
{
    f(expect_int(interp_exp(l)), expect_int(interp_exp(r)))
}

fn expect_int(v: ChocoVal) -> i32 {
    match v {
        ChocoVal::Int(i) => i,
        _ => panic!("wrong type, expected Int")
    }
}

fn expect_bool(v: ChocoVal) -> bool {
    match v {
        ChocoVal::Bool(b) => b,
        _ => panic!("wrong type, expected Bool")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChocoVal {
    Int(i32),
    Bool(bool)
}

#[cfg(test)]
mod tests {
    use front::{lexer, parser, ast};

    use super::{interp_exp, ChocoVal};

    fn parse_and_interp(input: &str) -> ChocoVal {
        let prog = parser::Parser::new(lexer::Lexer::new(input)).parse().expect("valid program");
        if let Some(ast::Statement::Expr(e)) = prog.stmts.first() {
            interp_exp(e)
        } else {
            panic!("Program didn't consist of a single expression")
        }
    }

    #[test]
    fn test_num_exp() {
        assert_eq!(parse_and_interp("1 + 2"), ChocoVal::Int(3));
        assert_eq!(parse_and_interp("1 + 2 * 3"), ChocoVal::Int(7));
        assert_eq!(parse_and_interp("1 + 2 * 3 // 2"), ChocoVal::Int(4));
        assert_eq!(parse_and_interp("1 + 2 * 7 % 4"), ChocoVal::Int(3));
    }

    #[test]
    fn test_bool_exp() {
        assert_eq!(parse_and_interp("True and False"), ChocoVal::Bool(false));
        assert_eq!(parse_and_interp("True or False"), ChocoVal::Bool(true));
        assert_eq!(parse_and_interp("not True"), ChocoVal::Bool(false));
    }

    #[test]
    fn test_comparison_exp() {
        assert_eq!(parse_and_interp("1 > 2"), ChocoVal::Bool(false));
        assert_eq!(parse_and_interp("1 >= 1"), ChocoVal::Bool(true));
        assert_eq!(parse_and_interp("1 < 2"), ChocoVal::Bool(true));
        assert_eq!(parse_and_interp("1 <= 1"), ChocoVal::Bool(true));
        assert_eq!(parse_and_interp("1 == 2"), ChocoVal::Bool(false));
        assert_eq!(parse_and_interp("1 != 2"), ChocoVal::Bool(true));
    }

    #[test]
    fn test_ternary_exp() {
        assert_eq!(parse_and_interp("10 if 1 > 2 else 11"), ChocoVal::Int(11));
    }
}
