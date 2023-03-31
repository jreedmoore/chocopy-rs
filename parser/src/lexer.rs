use std::{str::Chars, iter::Peekable};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Token<'a> {
    Newline, Indent, Dedent, // refer to Python 3 documentation
    Identifier(&'a str),

    String(&'a str),
    Integer(i32),

    // operators
    Plus,
    Minus,
    Multiply,
    IntegerDiv,
    Modulo,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
    Assign,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Comma,
    Colon,
    Dot,
    Arrow,

    // keywords
    False,
    None,
    True,
    And,
    As,
    Assert,
    Async,
    Await,
    Break,
    Class,
    Continue,
    Def,
    Del,
    Elif,
    Else,
    Except,
    Finally,
    For,
    From,
    Global,
    If,
    Import,
    In,
    Is,
    Lambda,
    NonLocal,
    Not,
    Or,
    Pass,
    Raise,
    Return,
    Try,
    While,
    With,
    Yield,

    /// end of file
    Eof
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Location {
    offset: usize
}
impl Location {
    fn default() -> Location {
        Location { offset: 0 }
    }

    fn inc(&mut self) {
        self.offset += 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Span<'a> {
    start: Location,
    token: Token<'a>,
    end: Location,
}
struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    start: Location,
    current: Location,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum LexError {
    UnexpectedCharacter(char),
}

// https://craftinginterpreters.com/scanning.html
impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { chars: input.chars().peekable(), start: Location::default(), current: Location::default() }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.current.inc();
            Some(c)
        } else {
            None
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some(c) = self.chars.peek() {
            if *c == expected {
                self.chars.next();
                self.current.inc();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub(crate) fn scan(&mut self) -> Result<Span<'a>, LexError> {
        loop {
            if let Some(c) = self.advance() {
                match c {
                    '(' => return self.span(Token::OpenParen),
                    ')' => return self.span(Token::CloseParen),
                    '[' => return self.span(Token::OpenBracket),
                    ']' => return self.span(Token::CloseBracket),
                    ',' => return self.span(Token::Comma),
                    ':' => return self.span(Token::Colon),
                    '.' => return self.span(Token::Dot),
                    '+' => return self.span(Token::Plus),
                    '-' => 
                        if self.match_next('>') {
                            return self.span(Token::Arrow)
                        } else {
                            return self.span(Token::Minus)
                        }
                    '*' => return self.span(Token::Multiply),
                    '/' =>
                        if self.match_next('/') {
                            return self.span(Token::IntegerDiv)
                        } else {
                            return self.report_error(LexError::UnexpectedCharacter(c))
                        },
                    '%' => return self.span(Token::Modulo),
                    '<' => 
                        if self.match_next('=') {
                            return self.span(Token::LessThanEqual)
                        } else {
                            return self.span(Token::LessThan)
                        },
                    '>' => 
                        if self.match_next('=') {
                            return self.span(Token::GreaterThanEqual)
                        } else {
                            return self.span(Token::GreaterThan)
                        }
                    '=' =>
                        if self.match_next('=') {
                            return self.span(Token::Equal)
                        } else {
                            return self.span(Token::Assign)
                        }
                    '!' =>
                        if self.match_next('=') {
                            return self.span(Token::NotEqual)
                        } else {
                            // todo
                            return self.report_error(LexError::UnexpectedCharacter(' '))
                        }
                    '#' => loop {
                        if let Some(c) = self.peek() {
                            if *c != '\n' { // this probably needs some clever thinking to deal with windows style EOL etc.
                                self.advance();
                            }
                        } else {
                            break;
                        }
                    }
                    t => return self.report_error(LexError::UnexpectedCharacter(t)),
                }
            } else {
                break;
            }
        }
        self.span(Token::Eof)
    }

    fn span(&mut self, token: Token<'a>) -> Result<Span<'a>, LexError> {
        let s = Span { start: self.start, token: token, end: self.current };
        self.start = self.current;
        Ok(s)
    }

    fn report_error(&mut self, e: LexError) -> Result<Span<'a>, LexError>{
        self.start = self.current;
        Err(e)
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Span<'a>, LexError>;

    fn next(&mut self) -> Option<Result<Span<'a>, LexError>> {
        match self.scan() {
            Ok(Span { token: Token::Eof, ..}) => None,
            s => Some(s)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lex_eq(input: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(input);
        let output = lexer.filter_map(|r| r.ok().map(|s| s.token)).collect::<Vec<_>>();
        assert_eq!(output, tokens);
    }

    #[test]
    fn test_lexer() {
        assert_lex_eq("+-*//%==!==,:.->", vec![Token::Plus, Token::Minus, Token::Multiply, Token::IntegerDiv, Token::Modulo, Token::Equal, Token::NotEqual, Token::Assign, Token::Comma, Token::Colon, Token::Dot, Token::Arrow]);
        assert_lex_eq("<<=>>=", vec![Token::LessThan, Token::LessThanEqual, Token::GreaterThan, Token::GreaterThanEqual]);
        assert_lex_eq("()", vec![Token::OpenParen, Token::CloseParen]);
        assert_lex_eq("# blah blah", vec![]);
        assert_lex_eq("a = 1", vec![Token::Identifier("a"), Token::Assign, Token::Integer(1)]);
    }
}