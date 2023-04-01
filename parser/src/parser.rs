// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
// https://rust-analyzer.github.io/blog/2020/09/16/challeging-LR-parsing.html

use std::collections::VecDeque;

// https://chocopy.org/chocopy_language_reference.pdf
use crate::ast::{self};
use crate::lexer::{self, Lexer, Span, Token};
struct Parser<'a> {
    lexer: Lexer<'a>,
    eof: bool,
    current: Option<Span>,
    errors: Vec<ParseError>,
    peek: VecDeque<Span>,
}
impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer,
            eof: false,
            peek: VecDeque::new(),
            current: None,
            errors: vec![],
        }
    }

    /// Main entry point
    pub fn parse(&mut self) -> Result<ast::Program, ParseError> {
        Ok(self.program())
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

    fn statement(&mut self) -> Option<ast::Statement> {
        todo!()
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
        todo!()
    }

    // renaming of production rule type to avoid reserved keyword
    fn type_rule(&mut self) -> Option<ast::Type> {
        todo!()
    }

    fn literal(&mut self) -> Option<ast::Literal> {
        todo!()
    }
}

enum FunOrVar {
    Fun(ast::FunctionDef),
    Var(ast::VariableDef),
}

pub enum ParseError {
    LexError(lexer::LexError),
    UnexpectedEof,
    UnexpectedToken(Token),
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::lexer::Lexer;

    #[test]
    fn test_parser() {
        assert!(Parser::new(Lexer::new("")).parse().is_ok())
    }
}
