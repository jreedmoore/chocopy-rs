// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html

use std::collections::VecDeque;

// https://chocopy.org/chocopy_language_reference.pdf
use crate::ast;
use crate::lexer::{self, Lexer, Span, Token};

/// Recursive descent parser with 2 lookahead tokens
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    eof: bool,
    current: Option<Span>,
    errors: Vec<ParseError>,
    peek: VecDeque<Span>,
    exprs: VecDeque<ast::Expression>
}
impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            eof: false,
            peek: VecDeque::new(),
            current: None,
            errors: vec![],
            exprs: VecDeque::new(),
        }
    }

    /// Main entry point
    pub fn parse(&mut self) -> Result<ast::Program, ParseError> {
        let prog = self.program();
        if self.errors.is_empty() {
            Ok(prog)
        } else {
            Err(self.errors.pop().expect("not empty"))
        }
    }

    fn error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    fn consume(&mut self, expected: Token) -> Option<()> {
        if let Some(token) = self.advance() {
            if token == expected {
                return Some(());
            }
            self.error(ParseError::UnexpectedToken(token))
        }
        None
    }

    // Get next valid Span, ignoring errors
    fn get_next(&mut self) -> Option<Span> {
        loop {
            match self.lexer.next() {
                Some(Ok(span)) => return Some(span),
                None => {
                    self.eof = true;
                    return None;
                }
                Some(Err(e)) => {
                    self.error(ParseError::LexError(e));
                }
            }
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if let Some(span) = self.peek.pop_front() {
            self.current = Some(span.clone());
            Some(span.token)
        } else {
            if let Some(span) = self.get_next() {
                self.current = Some(span.clone());
                Some(span.token)
            } else {
                self.error(ParseError::UnexpectedEof);
                None
            }
        }
    }

    fn fill_peek(&mut self, to: usize) -> bool {
        while self.peek.len() < to {
            if let Some(span) = self.get_next() {
                self.peek.push_back(span)
            } else {
                return false;
            }
        }
        true
    }

    fn peek_n(&mut self, idx: usize) -> Option<&Token> {
        if self.fill_peek(idx + 1) {
            Some(&self.peek[idx].token)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.peek_n(0)
    }

    fn check(&mut self, expected: Token) -> bool {
        if !self.fill_peek(1) {
            return false;
        }
        self.peek[0].token == expected
    }

    fn check2(&mut self, expected: Token) -> bool {
        if !self.fill_peek(2) {
            return false;
        }
        self.peek[1].token == expected
    }

    fn check_p<F>(&mut self, p: F) -> bool
    where
        F: Fn(&Token) -> bool,
    {
        if !self.fill_peek(1) {
            return false;
        }
        p(&self.peek[0].token)
    }

    fn eof(&mut self) -> bool {
        self.eof
    }

    // Beginning of grammar rules
    fn program(&mut self) -> ast::Program {
        let mut defs = vec![];
        let mut stmts = vec![];
        loop {
            if self.eof() {
                break;
            }
            if self.is_typed_var() {
                if let Some(v) = self.variable_def() {
                    defs.push(ast::Definition::Var(v));
                }
                continue;
            }
            if self.check(Token::Def) {
                if let Some(f) = self.function_def() {
                    defs.push(ast::Definition::Func(f))
                }
                continue;
            }
            if self.check(Token::Class) {
                if let Some(c) = self.class_def() {
                    defs.push(ast::Definition::Class(c))
                }
                continue;
            }
            break;
        }
        while !self.eof() {
            if let Some(stmt) = self.statement() {
                stmts.push(stmt)
            }
        }
        ast::Program { defs, stmts }
    }

    fn class_def(&mut self) -> Option<ast::ClassDef> {
        self.consume(Token::Class)?;
        let id = self.identifier()?;
        self.consume(Token::OpenParen);
        let parent = self.identifier()?;
        self.consume(Token::CloseParen)?;
        self.consume(Token::Colon)?;
        self.consume(Token::Newline)?;
        self.consume(Token::Indent)?;
        if self.check(Token::Pass) {
            self.consume(Token::Pass)?;
            self.consume(Token::Newline)?;
            Some(ast::ClassDef {
                id,
                parent,
                vars: vec![],
                funcs: vec![],
            })
        } else {
            let mut vars = vec![];
            let mut funcs = vec![];
            // first Dedent (not in a function def) ends the class
            while !self.check(Token::Dedent) {
                self.advance()?;
                // if we hit a def token we're defining a function
                if self.check(Token::Def) {
                    funcs.push(self.function_def()?);
                }
                // otherwise must be a variable
                // TODO: not sure how ? early escape error recovery will work
                vars.push(self.variable_def()?);
            }
            self.consume(Token::Dedent)?;
            Some(ast::ClassDef {
                id,
                parent,
                vars,
                funcs,
            })
        }
    }

    fn function_def(&mut self) -> Option<ast::FunctionDef> {
        self.consume(Token::Def)?;
        let id = self.identifier()?;
        self.consume(Token::OpenParen)?;
        let mut params = vec![];
        loop {
            if self.is_identifier() {
                params.push(self.typed_var()?)
            }
            if self.check(Token::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        self.consume(Token::CloseParen);
        let mut return_type = None;
        if self.check(Token::Arrow) {
            self.advance()?;
            return_type = Some(self.type_rule()?);
        }
        self.consume(Token::Colon);
        self.consume(Token::Newline);
        self.consume(Token::Indent);
        let mut decls = vec![];
        let mut vars = vec![];
        let mut funcs = vec![];
        loop {
            if self.is_typed_var() {
                vars.push(self.variable_def()?);
            }
            if self.check(Token::Def) {
                funcs.push(self.function_def()?);
            }
            if self.check(Token::Global) {
                self.advance();
                decls.push(ast::Declaration::Global(self.identifier()?))
            }
            if self.check(Token::NonLocal) {
                self.advance();
                decls.push(ast::Declaration::NonLocal(self.identifier()?))
            }
            break;
        }
        let mut stmts = vec![];
        loop {
            if self.check(Token::Dedent) {
                break;
            }
            stmts.push(self.statement()?);
        }
        Some(ast::FunctionDef {
            id,
            params,
            return_type,
            decls,
            vars,
            funcs,
            stmts,
        })
    }

    fn variable_def(&mut self) -> Option<ast::VariableDef> {
        let var = self.typed_var()?;
        self.consume(Token::Colon)?;
        let literal = self.literal()?;
        Some(ast::VariableDef { var, literal })
    }

    fn statement(&mut self) -> Option<ast::Statement> {
        match self.peek()? {
            Token::Pass => {
                self.advance()?;
                self.consume(Token::Newline)?;
                Some(ast::Statement::Pass)
            }
            Token::Return => {
                self.advance()?;
                if self.check(Token::Newline) {
                    Some(ast::Statement::Return(None))
                } else {
                    Some(ast::Statement::Return(Some(self.expression()?)))
                }
            }
            Token::If => {
                self.advance()?;
                let main = self.cond_block()?;
                let mut elifs = vec![];
                while self.check(Token::Elif) {
                    self.advance()?;
                    elifs.push(self.cond_block()?);
                }
                let mut otherwise = None;
                if self.check(Token::Else) {
                    self.advance()?;
                    otherwise = Some(self.block()?);
                }
                Some(ast::Statement::If {
                    main,
                    elifs,
                    otherwise,
                })
            }
            Token::While => {
                self.advance()?;
                Some(ast::Statement::While(self.cond_block()?))
            }
            Token::For => {
                self.advance()?;
                let id = self.identifier()?;
                self.consume(Token::In)?;
                let in_expr = self.expression()?;
                self.consume(Token::Colon);
                let block = self.block()?;
                Some(ast::Statement::For { id, in_expr, block })
            }
            _ => Some(ast::Statement::Expr(self.expression()?)),
        }
    }

    fn cond_block(&mut self) -> Option<ast::ConditionalBlock> {
        let condition = self.expression()?;
        self.consume(Token::Colon);
        let then = self.block()?;
        Some(ast::ConditionalBlock { condition, then })
    }
    fn block(&mut self) -> Option<Vec<ast::Statement>> {
        self.consume(Token::Newline)?;
        self.consume(Token::Indent)?;
        let mut stmts = vec![];
        while !self.check(Token::Dedent) {
            stmts.push(self.statement()?)
        }
        self.consume(Token::Dedent)?;
        if stmts.is_empty() {
            self.error(ParseError::EmptyBlock);
            None
        } else {
            Some(stmts)
        }
    }

    fn is_identifier(&mut self) -> bool {
        self.check_p(|t| t.is_identifier())
    }

    fn identifier(&mut self) -> Option<ast::Identifier> {
        if self.is_identifier() {
            if let Some(Token::Identifier(id)) = self.advance() {
                return Some(ast::Identifier { name: id });
            }
        }
        None
    }

    fn is_typed_var(&mut self) -> bool {
        self.is_identifier() && self.check2(Token::Colon)
    }

    fn typed_var(&mut self) -> Option<ast::TypedVar> {
        let id = self.identifier()?;
        self.consume(Token::Colon)?;
        let type_name = self.type_rule()?;
        Some(ast::TypedVar { id, typ: type_name })
    }

    // renaming of production rule type to avoid reserved keyword
    fn type_rule(&mut self) -> Option<ast::Type> {
        let r = match self.peek()? {
            Token::Identifier(name) => Some(ast::Type::Id(ast::Identifier {
                name: name.to_string(),
            })),
            Token::IdString(name) => Some(ast::Type::Id(ast::Identifier {
                name: name.to_string(),
            })),
            Token::OpenBracket => Some(ast::Type::List(Box::new(self.type_rule()?))),
            _ => return None,
        };
        self.advance();
        r
    }

    fn literal(&mut self) -> Option<ast::Literal> {
        match self.advance()? {
            Token::None => Some(ast::Literal::None),
            Token::True => Some(ast::Literal::True),
            Token::False => Some(ast::Literal::False),
            Token::Integer(i) => Some(ast::Literal::Integer(i)),
            Token::String(s) => Some(ast::Literal::Str(s.clone())),
            Token::IdString(s) => Some(ast::Literal::IdStr(ast::Identifier { name: s.clone() })),
            _ => None,
        }
    }

    fn expression(&mut self) -> Option<ast::Expression> {
        self.expression_bp(0)
    }

    // main idea is that we parse a prefix, peek to decide if we're parsing a bin/3-op
    // use binding power to determine when to stop (on rhs)
    fn expression_bp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let prefix_token = self.peek()?.clone();
        let prefix_rule = Parser::parse_table(&prefix_token);
        if let Some(f) = prefix_rule.prefix {
            let prefix = f(self, prefix_rule.power.right_bp())?;
            self.exprs.push_back(prefix);
        } else {
            self.error(ParseError::UnexpectedToken(prefix_token));
            return None
        }

        let rule = self.peek().map(Parser::parse_table);
        if let Some(rule) = rule {
            if rule.infix.is_none() || rule.power.left_bp() < min_bp {
                return self.exprs.pop_back();
            }

            return rule.infix.unwrap()(self, rule.power.right_bp());
        } else {
            return self.exprs.pop_back();
        }
    }

    fn logical_infix(&mut self, min_bp: usize) -> Option<ast::Expression> {
        match self.advance()? {
            op @ (Token::And | Token::Or) => {
                let bin_op = match op {
                    Token::And => ast::LogicalBinOp::And,
                    Token::Or => ast::LogicalBinOp::Or,
                    _ => unimplemented!()
                };
                let rhs = self.expression_bp(min_bp)?;
                Some(ast::Expression::LogicalBinaryOp(bin_op, Box::new(self.exprs.pop_front()?), Box::new(rhs)))
            },
            Token::If => {
                let mhs = self.expression_bp(0)?;
                self.consume(Token::Else)?;
                let rhs = self.expression_bp(min_bp)?;
                let lhs = self.exprs.pop_front()?;
                Some(ast::Expression::Ternary { e: Box::new(lhs), if_expr: Box::new(mhs), else_expr: Box::new(rhs) })
            }
            t => {
                self.error(ParseError::UnexpectedToken(t));
                return None
            }
        }
    }

    fn logical_prefix(&mut self, min_bp: usize) -> Option<ast::Expression> {
        match self.advance()? {
            Token::Not => {
                Some(ast::Expression::Not(Box::new(self.expression_bp(min_bp)?)))
            }
            t => {
                self.error(ParseError::UnexpectedToken(t));
                None
            }
        }
    }

    fn parse_table<'b>(token: &Token) -> ParseRule<'a, 'b> {
        match token {
            Token::Or => ParseRule::infix(Parser::logical_infix, 1, 2),
            Token::And => ParseRule::infix(Parser::logical_infix, 3, 4),
            Token::If => ParseRule::infix(Parser::logical_infix, 3, 4),
            Token::Not => ParseRule::prefix(Parser::logical_prefix, 7),
            _ => ParseRule::default(Parser::cexpression_bp)
        }
    }

    fn cexpression_bp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let prefix = {
            // I can't figure out function pointers to a struct with lifetimes
            // so I'm just jamming the equivalent of the table directly here.
            match self.peek()? {
                Token::None
                | Token::True
                | Token::False
                | Token::Integer(_)
                | Token::String(_)
                | Token::IdString(_) => {
                    return Some(ast::Expression::Lit(self.literal()?))
                }
                Token::OpenParen => {
                    self.advance();
                    let e = self.expression_bp(0);
                    self.consume(Token::CloseParen);
                    // TODO: is this correct?
                    return e;
                }
                Token::Minus => {
                    self.advance();
                    return Some(ast::Expression::UnaryMinus(Box::new(
                        self.cexpression_bp(0)?,
                    )));
                }
                Token::OpenBracket => {
                    self.advance();
                    let mut es = vec![];
                    loop {
                        es.push(self.expression_bp(0)?);
                        if self.check(Token::Comma) {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.consume(Token::CloseBracket)?;
                    return Some(ast::Expression::ListLiteral(es));
                }
                t => {
                    let t = t.clone();
                    self.advance()?;
                    self.error(ParseError::UnexpectedToken(t));
                    return None;
                }
            }
        };
        todo!()
    }
}

type PF<'a,'b> = fn(&'b mut Parser<'a>, usize) -> Option<ast::Expression>;
struct ParseRule<'a,'b> {
    prefix: Option<PF<'a, 'b>>,
    infix: Option<PF<'a,'b>>,
    power: BindingPower
}
impl<'a, 'b> ParseRule<'a, 'b> {
    fn prefix(f: PF<'a,'b>, bp: usize) -> ParseRule<'a,'b> {
        ParseRule { prefix: Some(f), infix: None, power: BindingPower::Prefix(bp) }
    }

    fn infix(f: PF<'a,'b>, lbp: usize, rbp: usize) -> ParseRule<'a,'b> {
        ParseRule { prefix: None, infix: Some(f), power: BindingPower::Infix(lbp, rbp) }
    }

    fn postfix(f: PF<'a,'b>, bp: usize) -> ParseRule<'a,'b> {
        ParseRule { prefix: Some(f), infix: None, power: BindingPower::Postfix(bp) }
    }

    fn default(f: PF<'a,'b>) -> ParseRule<'a,'b> {
        ParseRule::prefix(f, 0)
    }
}

enum BindingPower {
    Prefix(usize),
    Infix(usize, usize),
    Postfix(usize),
}
impl BindingPower {
    fn left_bp(&self) -> usize {
        match self {
            BindingPower::Prefix(bp) => *bp,
            BindingPower::Infix(bp, _) => *bp,
            BindingPower::Postfix(_) => usize::MAX,
        }
    }

    fn right_bp(&self) -> usize {
        match self {
            BindingPower::Prefix(_) => usize::MAX,
            BindingPower::Infix(_, bp) => *bp,
            BindingPower::Postfix(bp) => *bp,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    LexError(lexer::LexError),
    UnexpectedEof,
    UnexpectedToken(Token),
    EmptyBlock,
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::lexer::Lexer;

    fn assert_parses(input: &str) {
        println!("Parses: {}", input);
        let r = Parser::new(Lexer::new(input)).parse();
        match r {
            Err(e) => panic!("Failure in example {}: {:?}", input, e),
            Ok(p) if p.defs.is_empty() && p.stmts.is_empty() => panic!("Empty parse in example {}", input),
            Ok(a) => println!("{:?}", a),
        }
    }

    fn assert_fails(input: &str) {
        println!("Fails: {}", input);
        let r = Parser::new(Lexer::new(input)).parse();
        assert!(r.is_err(), "Expected failure for {}", input);
    }

    #[test]
    fn test_one() {
        assert_parses("True == not False")
    }

    #[test]
    fn test_parser() {
        assert_parses("1");
        assert_parses("1234");
        assert_parses("True");
        assert_parses("False");
        assert_parses("None");
        assert_parses("[1, None]");
        assert_parses("(1)");
        assert_fails("((1)");
        // hangs
        assert_fails("(1))");
        assert_fails("True == not False");
        assert_parses("True == (not False)");
    }
}
