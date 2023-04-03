use std::{fmt::Display, iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Newline,
    Indent,
    Dedent, // refer to Python 3 documentation
    Identifier(String),

    String(String),
    IdString(String),
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
    Eof,
}
impl Token {
    pub fn is_identifier(&self) -> bool {
        match self {
            Token::Identifier(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub offset: usize,
}
impl Location {
    fn default() -> Location {
        Location { offset: 0 }
    }

    fn inc(&mut self) {
        self.offset += 1;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Location,
    pub token: Token,
    pub end: Location,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError {
    UnexpectedCharacter { got: char, expected: Option<char>, during: &'static str },
    UnexpectedEof(&'static str),
    OversizedInteger,
    TabError,
}
impl LexError {
    fn unexpected_char(got: char, during: &'static str) -> LexError {
        LexError::UnexpectedCharacter { got, expected: None, during }
    }
    fn unexpected_char_with(got: char, expected: char, during: &'static str) -> LexError {
        LexError::UnexpectedCharacter { got, expected: Some(expected), during }
    }
}

// https://craftinginterpreters.com/scanning.html
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    start: Location,
    current: Location,
    line_start: bool,
    line_empty: bool,
    indent_stack: Vec<usize>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars().peekable(),
            start: Location::default(),
            current: Location::default(),
            line_start: true,
            line_empty: true,
            indent_stack: vec![0],
        }
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

    pub(crate) fn scan(&mut self) -> Result<Span, LexError> {
        loop {
            if let Some(c) = self.peek() {
                if !c.is_whitespace() && self.line_start {
                    // only hit this if first character is non whitespace
                    self.line_start = false;
                    let dedent = self.indent_stack.len() > 1;
                    self.indent_stack.truncate(1);
                    if dedent {
                        return self.span(Token::Dedent);
                    }
                }
            }
            if let Some(c) = self.advance() {
                if !c.is_whitespace() && c != '#' {
                    self.line_empty = false;
                }
                match c {
                    w if w.is_whitespace() && w != '\n' && w != '\r' && self.line_start => {
                        let indent = self.scan_indent(w)?;
                        if *self.indent_stack.last().expect("never empty") < indent {
                            self.indent_stack.push(indent);
                            return self.span(Token::Indent);
                        } else if let Some(idx) =
                            self.indent_stack.iter().position(|l| *l == indent)
                        {
                            if idx == self.indent_stack.len() - 1 && idx != 0 {
                                continue;
                            }
                            self.indent_stack.truncate(idx);
                            return self.span(Token::Dedent);
                        } else {
                            return self.report_error(LexError::TabError);
                        }
                    }
                    '\n' | '\r' => {
                        if c == '\r' {
                            let cc = self.advance();
                            if let Some(cc) = cc {
                                if cc != '\n' {
                                    return self.report_error(LexError::unexpected_char_with(cc, '\n', "eol"));
                                }
                            }
                        }
                        self.line_start = true;
                        if !self.line_empty {
                            self.line_empty = true;
                            return self.span(Token::Newline);
                        } else {
                            self.indent_stack.truncate(1);
                        }
                        self.line_empty = true;
                    }
                    w if w.is_whitespace() && !self.line_start => continue,

                    '(' => return self.span(Token::OpenParen),
                    ')' => return self.span(Token::CloseParen),
                    '[' => return self.span(Token::OpenBracket),
                    ']' => return self.span(Token::CloseBracket),
                    ',' => return self.span(Token::Comma),
                    ':' => return self.span(Token::Colon),
                    '.' => return self.span(Token::Dot),
                    '+' => return self.span(Token::Plus),
                    '-' => {
                        if self.match_next('>') {
                            return self.span(Token::Arrow);
                        } else {
                            return self.span(Token::Minus);
                        }
                    }
                    '*' => return self.span(Token::Multiply),
                    '/' => {
                        if self.match_next('/') {
                            return self.span(Token::IntegerDiv);
                        } else {
                            return self.report_error(LexError::unexpected_char_with(c, '/', "div"));
                        }
                    }
                    '%' => return self.span(Token::Modulo),
                    '<' => {
                        if self.match_next('=') {
                            return self.span(Token::LessThanEqual);
                        } else {
                            return self.span(Token::LessThan);
                        }
                    }
                    '>' => {
                        if self.match_next('=') {
                            return self.span(Token::GreaterThanEqual);
                        } else {
                            return self.span(Token::GreaterThan);
                        }
                    }
                    '=' => {
                        if self.match_next('=') {
                            return self.span(Token::Equal);
                        } else {
                            return self.span(Token::Assign);
                        }
                    }
                    '!' => {
                        if self.match_next('=') {
                            return self.span(Token::NotEqual);
                        } else {
                            // todo
                            return self.report_error(LexError::unexpected_char_with(c, '=', "not equal"));
                        }
                    }
                    '#' => loop {
                        if let Some(c) = self.peek() {
                            if *c != '\n' {
                                // this probably needs some clever thinking to deal with windows style EOL etc.
                                self.advance();
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                    '"' => return self.scan_string(),
                    '0'..='9' => return self.scan_number(c),
                    c if c.is_alphanumeric() => return self.scan_identifier(c),
                    t => return self.report_error(LexError::unexpected_char(t, "fall thru")),
                }
            } else {
                break;
            }
        }
        self.span(Token::Eof)
    }

    fn scan_identifier(&mut self, first: char) -> Result<Span, LexError> {
        let mut buf = String::new();
        buf.push(first);
        loop {
            if let Some(c) = self.peek() {
                if c.is_alphanumeric() {
                    let c = self.advance().unwrap();
                    buf.push(c);
                    continue;
                }
            }
            break;
        }
        return match buf.as_str() {
            "False" => self.span(Token::False),
            "None" => self.span(Token::None),
            "True" => self.span(Token::True),
            "and" => self.span(Token::And),
            "as" => self.span(Token::As),
            "assert" => self.span(Token::Assert),
            "async" => self.span(Token::Async),
            "await" => self.span(Token::Await),
            "break" => self.span(Token::Break),
            "class" => self.span(Token::Class),
            "continue" => self.span(Token::Continue),
            "def" => self.span(Token::Def),
            "del" => self.span(Token::Del),
            "elif" => self.span(Token::Elif),
            "else" => self.span(Token::Else),
            "except" => self.span(Token::Except),
            "finally" => self.span(Token::Finally),
            "for" => self.span(Token::For),
            "from" => self.span(Token::From),
            "global" => self.span(Token::Global),
            "if" => self.span(Token::If),
            "import" => self.span(Token::Import),
            "in" => self.span(Token::In),
            "is" => self.span(Token::Is),
            "lambda" => self.span(Token::Lambda),
            "nonlocal" => self.span(Token::NonLocal),
            "not" => self.span(Token::Not),
            "or" => self.span(Token::Or),
            "pass" => self.span(Token::Pass),
            "raise" => self.span(Token::Raise),
            "return" => self.span(Token::Return),
            "try" => self.span(Token::Try),
            "while" => self.span(Token::While),
            "with" => self.span(Token::With),
            "yield" => self.span(Token::Yield),
            &_ => self.span(Token::Identifier(buf)),
        };
    }

    fn scan_string(&mut self) -> Result<Span, LexError> {
        let mut buf = String::new();
        let mut escaped = false;
        loop {
            if let Some(c) = self.advance() {
                match c {
                    '\\' if escaped => {
                        buf.push('\\');
                        escaped = false
                    }
                    '"' if escaped => {
                        buf.push('"');
                        escaped = false
                    }
                    'n' if escaped => {
                        buf.push('\n');
                        escaped = false
                    }
                    't' if escaped => {
                        buf.push('\t');
                        escaped = false
                    }
                    '\\' => escaped = true,

                    _ if escaped => {
                        return self.report_error(LexError::UnexpectedEof("in string escape"))
                    }
                    '"' => return self.span(Token::String(buf)),
                    _ => buf.push(c),
                }
            } else {
                return self.report_error(LexError::UnexpectedEof("in string"));
            }
        }
    }

    fn scan_number(&mut self, first: char) -> Result<Span, LexError> {
        let mut acc: u32 = first.to_digit(10).unwrap();
        loop {
            if let Some(c) = self.peek() {
                if c.is_digit(10) {
                    let c = self.advance().unwrap();
                    acc *= 10;
                    acc += c.to_digit(10).unwrap();
                    continue;
                } else {
                    break;
                }
            }
            break;
        }
        let signed: i32 = match acc.try_into() {
            Ok(n) => n,
            Err(_) => return Err(LexError::OversizedInteger),
        };
        return self.span(Token::Integer(signed));
    }

    fn span(&mut self, token: Token) -> Result<Span, LexError> {
        let s = Span {
            start: self.start,
            token: token,
            end: self.current,
        };
        self.start = self.current;
        Ok(s)
    }

    fn report_error(&mut self, e: LexError) -> Result<Span, LexError> {
        self.start = self.current;
        Err(e)
    }

    fn scan_indent(&mut self, w: char) -> Result<usize, LexError> {
        let mut level = 0;
        match w {
            '\t' => level += 8,
            ' ' => level += 1,
            _ => return Err(LexError::unexpected_char(w, "scan_indent")),
        }
        self.line_start = false;

        loop {
            if let Some(c) = self.peek() {
                match c {
                    '\t' => level += 8 - (level % 8),
                    ' ' => level += 1,
                    _ => break,
                }
                self.advance();
            } else {
                break;
            }
        }

        Ok(level)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Span, LexError>;

    fn next(&mut self) -> Option<Result<Span, LexError>> {
        match self.scan() {
            Ok(Span {
                token: Token::Eof, ..
            }) => None,
            s => Some(s),
        }
    }
}

#[derive(Debug)]
pub struct LexErrors {
    errors: Vec<LexError>,
}
impl Display for LexErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.errors {
            write!(f, "{:?}", error)?;
        }
        Ok(())
    }
}

impl std::error::Error for LexErrors {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

pub fn lex(input: &str) -> Result<Vec<Token>, LexErrors> {
    let lexer = Lexer::new(input);
    let mut tokens = vec![];
    let mut errors = vec![];
    for result in lexer {
        match result {
            Ok(span) => tokens.push(span.token),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(LexErrors { errors })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_lex_eq(input: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(input);
        let mut output = vec![];
        let mut errs = vec![];
        for r in lexer {
            match r {
                Ok(s) => output.push(s.token),
                Err(e) => errs.push(e),
            }
        }
        assert_eq!(output, tokens, "mismatch for: {}", input);
        assert_eq!(errs, vec![], "errors for: {}", input);
    }

    #[test]
    fn test_one() {
        assert_lex_eq("if True:\n  False\nelse:\n  True", vec![])
    }

    #[test]
    fn test_lexer() {
        assert_lex_eq(
            "+-*//%==!==,:.->",
            vec![
                Token::Plus,
                Token::Minus,
                Token::Multiply,
                Token::IntegerDiv,
                Token::Modulo,
                Token::Equal,
                Token::NotEqual,
                Token::Assign,
                Token::Comma,
                Token::Colon,
                Token::Dot,
                Token::Arrow,
            ],
        );
        assert_lex_eq(
            "<<=>>=",
            vec![
                Token::LessThan,
                Token::LessThanEqual,
                Token::GreaterThan,
                Token::GreaterThanEqual,
            ],
        );
        assert_lex_eq("()", vec![Token::OpenParen, Token::CloseParen]);
        assert_lex_eq("# blah blah", vec![]);
        assert_lex_eq("# blah blah\n", vec![]);
        assert_lex_eq(
            "1234 # blah blah\n",
            vec![Token::Integer(1234), Token::Newline],
        );
        assert_lex_eq(
            r#""hello world""#,
            vec![Token::String("hello world".to_string())],
        );
        assert_lex_eq(
            r#""hello \"world""#,
            vec![Token::String("hello \"world".to_string())],
        );
        assert_lex_eq("1234", vec![Token::Integer(1234)]);
        assert_lex_eq(
            "[1, 2]",
            vec![
                Token::OpenBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::CloseBracket,
            ],
        );
        assert_lex_eq(
            "a = 1",
            vec![
                Token::Identifier("a".to_string()),
                Token::Assign,
                Token::Integer(1),
            ],
        );
        assert_lex_eq(
            "return a",
            vec![Token::Return, Token::Identifier("a".to_string())],
        );
        assert_lex_eq("\n\n\n\n", vec![]);
        assert_lex_eq(
            "def foo(a: int):\n\tb = 0\n",
            vec![
                Token::Def,
                Token::Identifier("foo".to_string()),
                Token::OpenParen,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::Identifier("int".to_string()),
                Token::CloseParen,
                Token::Colon,
                Token::Newline,
                Token::Indent,
                Token::Identifier("b".to_string()),
                Token::Assign,
                Token::Integer(0),
                Token::Newline,
            ],
        )
    }
}
