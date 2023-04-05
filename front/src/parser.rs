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
    errors: Vec<AnnotatedParseError>,
    peek: VecDeque<Span>,
    exprs: Vec<ast::Expression>,
}
impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            eof: false,
            peek: VecDeque::new(),
            current: None,
            errors: vec![],
            exprs: vec![],
        }
    }

    /// Main entry point
    pub fn parse(&mut self) -> Result<ast::Program, AnnotatedParseError> {
        let prog = self.program();
        if self.errors.is_empty() {
            Ok(prog)
        } else {
            Err(self.errors.first().cloned().expect("not empty"))
        }
    }

    fn error(&mut self, error: ParseError) {
        let ann = if let Some(span) = &self.current {
            AnnotatedParseError {
                begin: span.start,
                end: span.end,
                error: error,
            }
        } else {
            AnnotatedParseError {
                begin: lexer::Location::default(),
                end: lexer::Location::default(),
                error: error,
            }
        };
        println!("error {:?}", ann);
        self.errors.push(ann);
    }

    fn consume(&mut self, expected: Token, during: &'static str) -> Option<()> {
        if let Some(token) = self.advance(during) {
            if token == expected {
                return Some(());
            }
            self.error(ParseError::UnexpectedToken {
                token,
                expected: Some(expected),
                during,
            })
        }
        None
    }

    fn consume_or_eof(&mut self, expected: Token, during: &'static str) -> Option<()> {
        if let Some(_) = self.peek() {
            let token = self.advance(during)?;
            if token == expected {
                return Some(());
            } else {
                self.error(ParseError::UnexpectedToken {
                    token,
                    expected: Some(expected),
                    during,
                });
                None
            }
        } else {
            Some(())
        }
    }

    fn eol_or_eof(&mut self, during: &'static str) -> Option<()> {
        if let Some(_) = self.peek() {
            let token = self.advance(during)?;
            if token == Token::Newline {
                Some(())
            } else {
                self.error(ParseError::UnexpectedToken {
                    token: token,
                    expected: Some(Token::Newline),
                    during,
                });
                None
            }
        } else {
            Some(())
        }
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

    fn advance(&mut self, during: &'static str) -> Option<Token> {
        if let Some(span) = self.peek.pop_front() {
            self.current = Some(span.clone());
            println!("advance {:?}: {}", span.token, during);
            Some(span.token)
        } else {
            if let Some(span) = self.get_next() {
                self.current = Some(span.clone());
                println!("advance {:?}: {}", span.token, during);
                Some(span.token)
            } else {
                self.error(ParseError::UnexpectedEof(during));
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
        self.consume(Token::Class, "class def")?;
        let id = self.identifier()?;
        self.consume(Token::OpenParen, "class parent");
        let parent = self.identifier()?;
        self.consume(Token::CloseParen, "class parent")?;
        self.consume(Token::Colon, "class header")?;
        self.consume(Token::Newline, "class header")?;
        self.consume(Token::Indent, "class body")?;
        if self.check(Token::Pass) {
            self.consume(Token::Pass, "class body pass")?;
            self.eol_or_eof("class body pass")?;
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
            while !self.check(Token::Dedent) && !self.eof {
                //self.advance("class defs")?;
                // if we hit a def token we're defining a function
                if self.check(Token::Def) {
                    funcs.push(self.function_def()?);
                } else {
                    vars.push(self.variable_def()?);
                }
            }
            self.consume_or_eof(Token::Dedent, "class body")?;
            Some(ast::ClassDef {
                id,
                parent,
                vars,
                funcs,
            })
        }
    }

    fn function_def(&mut self) -> Option<ast::FunctionDef> {
        self.consume(Token::Def, "fun def")?;
        let id = self.identifier()?;
        self.consume(Token::OpenParen, "fun def param")?;
        let mut params = vec![];
        loop {
            if self.is_identifier() {
                params.push(self.typed_var()?)
            }
            if self.check(Token::Comma) {
                self.advance("fun params")?;
            } else {
                break;
            }
        }
        self.consume(Token::CloseParen, "fun def param");
        let mut return_type = None;
        if self.check(Token::Arrow) {
            self.advance("fun return type")?;
            return_type = Some(self.type_rule()?);
        }
        self.consume(Token::Colon, "fun header");
        self.consume(Token::Newline, "fun header");
        self.consume(Token::Indent, "fun body");
        let mut decls = vec![];
        let mut vars = vec![];
        let mut funcs = vec![];
        loop {
            if self.is_typed_var() {
                vars.push(self.variable_def()?);
                continue;
            }
            if self.check(Token::Def) {
                funcs.push(self.function_def()?);
                continue;
            }
            if self.check(Token::Global) {
                self.advance("global");
                decls.push(ast::Declaration::Global(self.identifier()?));
                self.consume(Token::Newline, "global")?;
                continue;
            }
            if self.check(Token::NonLocal) {
                self.advance("nonlocal");
                decls.push(ast::Declaration::NonLocal(self.identifier()?));
                self.consume(Token::Newline, "nonlocal")?;
                continue;
            }
            break;
        }
        let mut stmts = vec![];
        while !self.check(Token::Dedent) {
            stmts.push(self.statement()?);
        }
        self.consume(Token::Dedent, "fun body")?;
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
        self.consume(Token::Assign, "variable def")?;
        let literal = self.literal()?;
        self.eol_or_eof("variable def")?;
        Some(ast::VariableDef { var, literal })
    }

    fn statement(&mut self) -> Option<ast::Statement> {
        match self.peek()? {
            Token::Pass => {
                self.advance("pass")?;
                self.eol_or_eof("pass stmt")?;
                Some(ast::Statement::Pass)
            }
            Token::Return => {
                self.advance("return")?;
                if self.check(Token::Newline) || self.eof() {
                    self.advance("empty return")?;
                    Some(ast::Statement::Return(None))
                } else {
                    let e = self.expression()?;
                    self.eol_or_eof("return")?;
                    Some(ast::Statement::Return(Some(e)))
                }
            }
            Token::If => {
                self.advance("if")?;
                let main = self.cond_block()?;
                let mut elifs = vec![];
                while self.check(Token::Elif) {
                    self.advance("elif")?;
                    elifs.push(self.cond_block()?);
                }
                let mut otherwise = None;
                if self.check(Token::Else) {
                    self.advance("else")?;
                    self.consume(Token::Colon, "else")?;
                    otherwise = Some(self.block()?);
                }
                Some(ast::Statement::If {
                    main,
                    elifs,
                    otherwise,
                })
            }
            Token::While => {
                self.advance("while")?;
                Some(ast::Statement::While(self.cond_block()?))
            }
            Token::For => {
                self.advance("for")?;
                let id = self.identifier()?;
                self.consume(Token::In, "for in")?;
                let in_expr = self.expression()?;
                self.consume(Token::Colon, "for header");
                let block = self.block()?;
                Some(ast::Statement::For { id, in_expr, block })
            }
            _ => {
                let e = self.expression()?;
                if !self.check(Token::Assign) {
                    self.eol_or_eof("expr stmt")?;
                    Some(ast::Statement::Expr(e))
                } else {
                    let mut exprs = vec![e];
                    loop {
                        if !self.check(Token::Assign) {
                            break;
                        }
                        self.consume(Token::Assign, "assign stmt")?;
                        let e = self.expression()?;
                        exprs.push(e);
                    }
                    let rhs = exprs.pop()?;
                    self.eol_or_eof("assign stmt")?;
                    Some(ast::Statement::Assign {
                        targets: self.expr_to_target(exprs)?,
                        expr: rhs,
                    })
                }
            }
        }
    }

    fn cond_block(&mut self) -> Option<ast::ConditionalBlock> {
        let condition = self.expression()?;
        self.consume(Token::Colon, "cond block");
        let then = self.block()?;
        Some(ast::ConditionalBlock { condition, then })
    }
    fn block(&mut self) -> Option<Vec<ast::Statement>> {
        self.consume(Token::Newline, "block")?;
        self.consume(Token::Indent, "block")?;
        let mut stmts = vec![];
        while !self.check(Token::Dedent) && !self.eof() {
            stmts.push(self.statement()?)
        }
        self.consume_or_eof(Token::Dedent, "block")?;
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
            if let Some(Token::Identifier(id)) = self.advance("identifier") {
                return Some(ast::Identifier { name: id });
            }
        }
        None
    }

    fn is_typed_var(&mut self) -> bool {
        let b = self.is_identifier() && self.check2(Token::Colon);
        if b {
            println!("is_typed_var")
        }
        b
    }

    fn typed_var(&mut self) -> Option<ast::TypedVar> {
        let id = self.identifier()?;
        self.consume(Token::Colon, "typed_var")?;
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
            Token::OpenBracket => {
                self.advance("list type");
                Some(ast::Type::List(Box::new(self.type_rule()?)))
            }
            _ => return None,
        };
        self.advance("type rule");
        r
    }

    fn literal(&mut self) -> Option<ast::Literal> {
        match self.advance("literal")? {
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

    fn expression_bp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let prefix_token = self.peek()?.clone();
        let prefix_rule = Parser::parse_table(&prefix_token);
        if let Some(pf) = prefix_rule.prefix {
            let prefix = (pf.f)(self, pf.power.right_bp())?;
            self.exprs.push(prefix);
        } else {
            self.advance("expr prefix");
            self.error(ParseError::unexpected_token(prefix_token, "expr prefix"));
            return None;
        }

        loop {
            if self.eof() {
                break;
            }

            let infix_rule = self
                .peek()
                .map(Parser::parse_table)
                .map(|r| r.infix)
                .flatten();

            if let Some(pf) = infix_rule {
                if pf.power.left_bp() < min_bp {
                    break;
                } else {
                    let e = (pf.f)(self, pf.power.right_bp())?;
                    self.exprs.push(e);
                }
            } else {
                break;
            }
        }

        return self.exprs.pop();
    }

    fn logical_infix(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let bin_op = match self.advance("infix")? {
            Token::And => ast::LogicalBinOp::And,
            Token::Or => ast::LogicalBinOp::Or,
            t => {
                self.error(ParseError::unexpected_token(t, "logical infix"));
                return None;
            }
        };

        let rhs = self.expression_bp(min_bp)?;
        Some(ast::Expression::LogicalBinaryOp(
            bin_op,
            Box::new(self.exprs.pop()?),
            Box::new(rhs),
        ))
    }

    fn logical_ternary(&mut self, min_bp: usize) -> Option<ast::Expression> {
        self.consume(Token::If, "ternary")?;
        let mhs = self.expression_bp(0)?;
        self.consume(Token::Else, "ternary")?;
        let rhs = self.expression_bp(min_bp)?;
        let lhs = self.exprs.pop()?;
        Some(ast::Expression::Ternary {
            e: Box::new(lhs),
            if_expr: Box::new(mhs),
            else_expr: Box::new(rhs),
        })
    }

    fn logical_prefix(&mut self, min_bp: usize) -> Option<ast::Expression> {
        match self.advance("logical prefix")? {
            Token::Not => Some(ast::Expression::Not(Box::new(self.expression_bp(min_bp)?))),
            token => {
                self.error(ParseError::unexpected_token(token, "logical prefix"));
                None
            }
        }
    }

    fn parse_table<'b>(token: &Token) -> ParseRule<'a, 'b> {
        match token {
            Token::Or => ParseRule::infix(Parser::logical_infix, 1, 2),
            Token::And => ParseRule::infix(Parser::logical_infix, 3, 4),
            Token::If => ParseRule::infix(Parser::logical_ternary, 6, 5),
            Token::Not => ParseRule::prefix(Parser::logical_prefix, 7),
            Token::None
            | Token::True
            | Token::False
            | Token::Integer(_)
            | Token::String(_)
            | Token::IdString(_) => ParseRule::prefix(Parser::literal_exp, 0),
            Token::OpenParen => ParseRule::both(Parser::grouping, 0, Parser::call_exp, 20, 21),
            Token::OpenBracket => {
                ParseRule::both(Parser::list_literal_exp, 0, Parser::access_exp, 22, 23)
            }
            Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan
            | Token::LessThanEqual
            | Token::GreaterThanEqual
            | Token::Is => ParseRule::infix(Parser::infix_exp, 9, 10),
            Token::Plus => ParseRule::infix(Parser::infix_exp, 11, 12),
            Token::Minus => ParseRule::both(Parser::unary_exp, 17, Parser::infix_exp, 9, 10), //todo, needs different! precedence for unary and binary
            Token::Multiply | Token::IntegerDiv | Token::Modulo => {
                ParseRule::infix(Parser::infix_exp, 13, 14)
            }
            Token::Dot => ParseRule::infix(Parser::access_exp, 15, 16),
            Token::Identifier(_) => ParseRule::prefix(Parser::identifier_exp, 0),
            _ => ParseRule::default(Parser::exp_error),
        }
    }

    fn literal_exp(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        Some(ast::Expression::Lit(self.literal()?))
    }

    fn identifier_exp(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        Some(ast::Expression::Id(self.identifier()?))
    }

    fn grouping(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        self.advance("grouping");
        let e = self.expression_bp(0)?;
        self.consume(Token::CloseParen, "grouping");
        Some(e)
    }

    fn list_literal_exp(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        self.advance("list literal");
        let mut es = vec![];
        loop {
            es.push(self.expression_bp(0)?);
            if self.check(Token::Comma) {
                self.advance("list comma");
            } else {
                break;
            }
        }
        self.consume(Token::CloseBracket, "list literal")?;
        return Some(ast::Expression::ListLiteral(es));
    }

    fn exp_error(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        let token = self.advance("exp error")?;
        self.error(ParseError::unexpected_token(token, "exp error"));
        None
    }

    fn infix_exp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let bin_op = match self.advance("infix exp")? {
            Token::Equal => ast::BinOp::Equals,
            Token::NotEqual => ast::BinOp::NotEquals,
            Token::LessThan => ast::BinOp::LessThan,
            Token::LessThanEqual => ast::BinOp::LessThanEqual,
            Token::GreaterThan => ast::BinOp::GreaterThan,
            Token::GreaterThanEqual => ast::BinOp::GreaterThanEqual,
            Token::Is => ast::BinOp::Is,
            Token::Plus => ast::BinOp::Plus,
            Token::Minus => ast::BinOp::Minus,
            Token::Multiply => ast::BinOp::Multiply,
            Token::IntegerDiv => ast::BinOp::IntegerDiv,
            Token::Modulo => ast::BinOp::Modulo,
            _ => unreachable!(),
        };
        let lhs = self.exprs.pop()?;
        let rhs = self.expression_bp(min_bp)?;

        Some(ast::Expression::BinaryOp(
            bin_op,
            Box::new(lhs),
            Box::new(rhs),
        ))
    }

    fn access_exp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        let token = self.advance("access exp")?;
        match token {
            Token::OpenBracket => {
                let rhs = self.expression_bp(0)?;
                let lhs = self.exprs.pop()?;
                self.consume(Token::CloseBracket, "index")?;
                Some(ast::Expression::Index(ast::IndexExpression {
                    expr: Box::new(lhs),
                    index: Box::new(rhs),
                }))
            }
            Token::Dot => {
                let id = self.identifier()?;
                let lhs = self.exprs.pop()?;
                Some(ast::Expression::Member(ast::MemberExpression {
                    expr: Box::new(lhs),
                    id,
                }))
            }
            _ => unreachable!(),
        }
    }

    fn call_exp(&mut self, _min_bp: usize) -> Option<ast::Expression> {
        self.consume(Token::OpenParen, "call")?;
        let lhs = self.exprs.pop()?;
        let mut args = vec![];
        if !self.check(Token::CloseParen) {
            loop {
                args.push(self.expression_bp(0)?);
                if self.check(Token::Comma) {
                    self.advance("call exp");
                } else {
                    break;
                }
            }
        }
        self.consume(Token::CloseParen, "call")?;
        Some(match lhs {
            ast::Expression::Member(m) => ast::Expression::MemberCall(m, args),
            ast::Expression::Id(id) => ast::Expression::Call(id, args),
            t => panic!("Unexpected lhs in call {:?}", t),
        })
    }

    fn unary_exp(&mut self, min_bp: usize) -> Option<ast::Expression> {
        self.consume(Token::Minus, "unary minus")?;
        let e = self.expression_bp(min_bp)?;
        Some(ast::Expression::UnaryMinus(Box::new(e)))
    }

    fn expr_to_target(&mut self, exprs: Vec<ast::Expression>) -> Option<Vec<ast::Target>> {
        let mut targets = vec![];
        for e in exprs {
            match e {
                ast::Expression::Id(i) => targets.push(ast::Target::Id(i)),
                ast::Expression::Member(m) => targets.push(ast::Target::Member(m)),
                ast::Expression::Index(idx) => targets.push(ast::Target::Index(idx)),
                t => {
                    self.error(ParseError::UnexpectedExprInTargetPosition);
                    return None;
                }
            }
        }
        Some(targets)
    }
}

type PF<'a, 'b> = fn(&'b mut Parser<'a>, usize) -> Option<ast::Expression>;
struct ParseFun<'a, 'b> {
    f: PF<'a, 'b>,
    power: BindingPower,
}
struct ParseRule<'a, 'b> {
    prefix: Option<ParseFun<'a, 'b>>,
    infix: Option<ParseFun<'a, 'b>>,
}
impl<'a, 'b> ParseRule<'a, 'b> {
    fn prefix(f: PF<'a, 'b>, bp: usize) -> ParseRule<'a, 'b> {
        ParseRule {
            prefix: Some(ParseFun {
                f,
                power: BindingPower::Prefix(bp),
            }),
            infix: None,
        }
    }

    fn infix(f: PF<'a, 'b>, lbp: usize, rbp: usize) -> ParseRule<'a, 'b> {
        ParseRule {
            prefix: None,
            infix: Some(ParseFun {
                f,
                power: BindingPower::Infix(lbp, rbp),
            }),
        }
    }

    fn both(p: PF<'a, 'b>, pbp: usize, i: PF<'a, 'b>, lbp: usize, rbp: usize) -> ParseRule<'a, 'b> {
        ParseRule {
            prefix: Some(ParseFun {
                f: p,
                power: BindingPower::Prefix(pbp),
            }),
            infix: Some(ParseFun {
                f: i,
                power: BindingPower::Infix(lbp, rbp),
            }),
        }
    }

    fn default(f: PF<'a, 'b>) -> ParseRule<'a, 'b> {
        ParseRule::prefix(f, 0)
    }
}

enum BindingPower {
    Prefix(usize),
    Infix(usize, usize),
}
impl BindingPower {
    fn left_bp(&self) -> usize {
        match self {
            BindingPower::Prefix(bp) => *bp,
            BindingPower::Infix(bp, _) => *bp,
        }
    }

    fn right_bp(&self) -> usize {
        match self {
            BindingPower::Prefix(_) => usize::MAX,
            BindingPower::Infix(_, bp) => *bp,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError(lexer::LexError),
    UnexpectedEof(&'static str),
    UnexpectedToken {
        token: Token,
        expected: Option<Token>,
        during: &'static str,
    },
    EmptyBlock,
    UnexpectedExprInTargetPosition,
}
impl ParseError {
    fn unexpected_token(token: Token, during: &'static str) -> ParseError {
        ParseError::UnexpectedToken {
            token,
            expected: None,
            during,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnnotatedParseError {
    begin: lexer::Location,
    end: lexer::Location,
    error: ParseError,
}
impl std::fmt::Display for AnnotatedParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at [{:?}, {:?}]", self.error, self.begin, self.end)
    }
}
impl std::error::Error for AnnotatedParseError {
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
        let r = Parser::new(Lexer::new(input)).parse();
        match r {
            Err(e) => panic!("Failure in example {}: {:?}", input, e),
            Ok(p) if p.defs.is_empty() && p.stmts.is_empty() => {
                panic!("Empty parse in example {}", input)
            }
            Ok(_) => (),
        }
    }

    fn assert_fails(input: &str) {
        let r = Parser::new(Lexer::new(input)).parse();
        assert!(r.is_err(), "Expected failure for {}", input);
    }

    #[test]
    fn test_one() {
        //assert_parses("1")
        assert_parses("class Foo(object):\n  x:int=0\n  y:int=None");
        //assert_parses("a < len(b)");
        //assert_parses("1 + 2 * 3 + 4");
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
        assert_parses("True if True else False");
        assert_parses("True and False");
        assert_parses("True or False");
        assert_parses("True and False or True");
        assert_parses("True or False and True");
        assert_parses("1 + 2 * 3 + 4");
        assert_parses("a.b");
        assert_parses("a[1]");
        assert_parses("a(1)");
        assert_parses("a(1, 2)");
        assert_parses("a.b(1, 2)");
        assert_parses("a < len(b)");
        assert_fails("((1)");
        assert_fails("(1))");
        //assert_fails("True == not False");
        assert_parses("True == (not False)");
        assert_parses("a = 1");
        assert_parses("a:int = 1");
        assert_parses("if True:\n  True\nelse:\n  False");
        assert_parses("a()");
        assert_parses("return f(y)\n");
    }
}
