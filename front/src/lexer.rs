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
    pub line: usize,
    pub col: usize,
}
impl Location {
    pub fn default() -> Location {
        Location { line: 1, col: 0 }
    }

    fn inc_col(&mut self) {
        self.col += 1;
    }

    fn inc_line(&mut self) {
        self.col = 0;
        self.line += 1;
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
    UnexpectedCharacter {
        got: char,
        expected: Option<char>,
        during: &'static str,
    },
    UnexpectedEof(&'static str),
    OversizedInteger,
    TabError,
}
impl LexError {
    fn unexpected_char(got: char, during: &'static str) -> LexError {
        LexError::UnexpectedCharacter {
            got,
            expected: None,
            during,
        }
    }
    fn unexpected_char_with(got: char, expected: char, during: &'static str) -> LexError {
        LexError::UnexpectedCharacter {
            got,
            expected: Some(expected),
            during,
        }
    }
}

#[derive(Debug, PartialEq)]
enum LineState {
    Start,
    Indent(usize),
    Logical,
    Dedent(usize),
}

// https://craftinginterpreters.com/scanning.html
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    start: Location,
    current: Location,
    line_state: LineState,
    indent_stack: Vec<usize>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            chars: input.chars().peekable(),
            start: Location::default(),
            current: Location::default(),
            line_state: LineState::Start,
            indent_stack: vec![0],
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.current.inc_col();
            Some(c)
        } else {
            None
        }
    }

    fn match_next(&mut self, expected: char) -> bool {
        if let Some(c) = self.chars.peek() {
            if *c == expected {
                self.chars.next();
                self.current.inc_col();
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
                let c = *c; // copy for the borrowck gods
                            // line state machine
                            // start transitions to Indent on whitespace, Logical on non-whitespace/non-comment
                            // Indent emits on non-EOL
                            // Logical transitions to Start on EOL
                match self.line_state {
                    LineState::Start if c.is_whitespace() => {
                        self.line_state = LineState::Indent(self.scan_indent()?);
                        continue;
                    }
                    LineState::Start if c == '#' => (),
                    LineState::Start => {
                        self.line_state = LineState::Logical;
                        let dedent = self.indent_stack.len() - 1;
                        self.indent_stack.truncate(1);
                        if dedent > 0 {
                            self.line_state = LineState::Dedent(dedent);
                            continue;
                        }
                    }
                    LineState::Dedent(d) if d > 0 => {
                        self.line_state = LineState::Dedent(d - 1);
                        return self.span(Token::Dedent)
                    }
                    LineState::Dedent(_) => {
                        self.line_state = LineState::Logical;
                        continue;
                    }
                    LineState::Indent(indent) if !c.is_whitespace() && c != '#' => {
                        self.line_state = LineState::Logical;
                        match compute_indentation(&mut self.indent_stack, indent) {
                            IndentationResult::Indent => return self.span(Token::Indent),
                            IndentationResult::Dedent(d) => {
                                self.line_state = LineState::Dedent(d);
                                continue;
                            }
                            IndentationResult::NoOp => (),
                            IndentationResult::Error => return self.report_error(LexError::TabError),
                        }
                    }
                    LineState::Indent(_) => (),
                    LineState::Logical => (),
                }
            }
            if let Some(c) = self.advance() {
                match c {
                    '\n' | '\r' => {
                        self.current.inc_line();
                        if c == '\r' {
                            let cc = self.peek();
                            if let Some(cc) = cc {
                                if *cc == '\n' {
                                    self.advance();
                                }
                            }
                        }

                        match self.line_state {
                            LineState::Start => (),
                            LineState::Dedent(_) => self.line_state = LineState::Start, // todo?
                            LineState::Indent(_) => self.line_state = LineState::Start,
                            LineState::Logical => {
                                self.line_state = LineState::Start;
                                return self.span(Token::Newline);
                            }
                        }
                    }
                    w if w.is_whitespace() && self.line_state == LineState::Logical => continue,
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
                            return self
                                .report_error(LexError::unexpected_char_with(c, '/', "div"));
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
                            return self.report_error(LexError::unexpected_char_with(
                                c,
                                '=',
                                "not equal",
                            ));
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
                    c if c.is_alphanumeric() || c == '_' => return self.scan_identifier(c),
                    t => return self.report_error(LexError::unexpected_char(t, "fall thru")),
                }
            } else {
                // eof handling
                if self.line_state == LineState::Logical {
                    // eof is implicit new line
                    self.line_state = LineState::Start;
                    return self.span(Token::Newline);
                }

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
                if c.is_alphanumeric() || *c == '_' {
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
                    '"' => {
                        if buf.chars().all(|c| c.is_alphanumeric() || c == '_') {
                            return self.span(Token::IdString(buf));
                        } else {
                            return self.span(Token::String(buf));
                        }
                    }
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

    fn scan_indent(&mut self) -> Result<usize, LexError> {
        let mut level = 0;
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

#[derive(Debug, PartialEq)]
enum IndentationResult {
    Indent,
    Dedent(usize),
    NoOp,
    Error
}

fn compute_indentation(indent_stack: &mut Vec<usize>, indent_level: usize) -> IndentationResult {
    let top = *indent_stack.last().expect("never empty");
    if top < indent_level {
        indent_stack.push(indent_level);
        IndentationResult::Indent
    } else if top == indent_level {
        IndentationResult::NoOp
    } else if let Some(idx) =
        indent_stack.iter().rposition(|l| *l == indent_level)
    {
        if idx == indent_stack.len() - 1 && idx != 0 {
            IndentationResult::NoOp
        } else {
            let dedents = indent_stack.len() - (idx + 1);
            indent_stack.truncate(idx+1);
            IndentationResult::Dedent(dedents)
        }
    } else {
        IndentationResult::Error
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
    use Token::*;

    #[test]
    fn test_compute_indentation() {
        assert_eq!(compute_indentation(&mut vec![0], 0), IndentationResult::NoOp);
        assert_eq!(compute_indentation(&mut vec![0], 4), IndentationResult::Indent);
        assert_eq!(compute_indentation(&mut vec![0, 4], 4), IndentationResult::NoOp);
        assert_eq!(compute_indentation(&mut vec![0, 8], 4), IndentationResult::Error);
        assert_eq!(compute_indentation(&mut vec![0, 8, 16], 8), IndentationResult::Dedent(1));
        assert_eq!(compute_indentation(&mut vec![0, 8, 16], 0), IndentationResult::Dedent(2));
        assert_eq!(compute_indentation(&mut vec![0, 2, 4], 0), IndentationResult::Dedent(2));
    }

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
        assert_lex_eq("class Foo(object):\n  def bar(self:\"Foo\"):\n    pass\nx = 1", vec![]);
    }

    #[test]
    fn test_lexer() {
        assert_lex_eq(
            "+-*//%==!==,:.->",
            vec![
                Plus, Minus, Multiply, IntegerDiv, Modulo, Equal, NotEqual, Assign, Comma, Colon,
                Dot, Arrow, Newline,
            ],
        );
        assert_lex_eq(
            "<<=>>=",
            vec![
                LessThan,
                LessThanEqual,
                GreaterThan,
                GreaterThanEqual,
                Newline,
            ],
        );
        assert_lex_eq("()", vec![OpenParen, CloseParen, Newline]);
        assert_lex_eq("# blah blah", vec![]);
        assert_lex_eq("# blah blah\n", vec![]);
        assert_lex_eq("1234 # blah blah\n", vec![Integer(1234), Newline]);
        assert_lex_eq("\"foo\"", vec![IdString("foo".to_string()), Newline]);
        assert_lex_eq(
            r#""hello world""#,
            vec![String("hello world".to_string()), Newline],
        );
        assert_lex_eq(
            r#""hello \"world""#,
            vec![String("hello \"world".to_string()), Newline],
        );
        assert_lex_eq("1234", vec![Integer(1234), Newline]);
        assert_lex_eq(
            "[1, 2]",
            vec![
                OpenBracket,
                Integer(1),
                Comma,
                Integer(2),
                CloseBracket,
                Newline,
            ],
        );
        assert_lex_eq(
            "__init__",
            vec![Identifier("__init__".to_string()), Newline],
        );
        assert_lex_eq(
            "a = 1",
            vec![Identifier("a".to_string()), Assign, Integer(1), Newline],
        );
        assert_lex_eq(
            "return a",
            vec![Return, Identifier("a".to_string()), Newline],
        );
        assert_lex_eq("\n\n\n\n", vec![]);
        assert_lex_eq(
            "def foo(a: int):\n\tb = 0\n",
            vec![
                Def,
                Identifier("foo".to_string()),
                OpenParen,
                Identifier("a".to_string()),
                Colon,
                Identifier("int".to_string()),
                CloseParen,
                Colon,
                Newline,
                Indent,
                Identifier("b".to_string()),
                Assign,
                Integer(0),
                Newline,
            ],
        );
        assert_lex_eq(
            "if True:\n  False\n  # foo\n  False",
            vec![
                If, True, Colon, Newline, Indent, False, Newline, False, Newline,
            ],
        );
    }
}
