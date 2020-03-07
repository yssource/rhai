use crate::any::Dynamic;
use crate::error::{LexError, ParseError, ParseErrorType};
use std::char;
use std::iter::Peekable;
use std::{borrow::Cow, str::Chars};

type LERR = LexError;
type PERR = ParseErrorType;

/// A location (line number + character position) in the input script.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct Position {
    line: usize,
    pos: usize,
}

impl Position {
    /// Create a new `Position`.
    pub fn new(line: usize, position: usize) -> Self {
        Self {
            line,
            pos: position,
        }
    }

    /// Get the line number (1-based), or `None` if EOF.
    pub fn line(&self) -> Option<usize> {
        match self.line {
            0 => None,
            x => Some(x),
        }
    }

    /// Get the character position (1-based), or `None` if at beginning of a line.
    pub fn position(&self) -> Option<usize> {
        match self.pos {
            0 => None,
            x => Some(x),
        }
    }

    /// Advance by one character position.
    pub(crate) fn advance(&mut self) {
        self.pos += 1;
    }

    /// Go backwards by one character position.
    ///
    /// # Panics
    ///
    /// Panics if already at beginning of a line - cannot rewind to a previous line.
    ///
    pub(crate) fn rewind(&mut self) {
        assert!(self.pos > 0, "cannot rewind at position 0");
        self.pos -= 1;
    }

    /// Advance to the next line.
    pub(crate) fn new_line(&mut self) {
        self.line += 1;
        self.pos = 0;
    }

    /// Create a `Position` at EOF.
    pub(crate) fn eof() -> Self {
        Self { line: 0, pos: 0 }
    }

    /// Is the `Position` at EOF?
    pub fn is_eof(&self) -> bool {
        self.line == 0
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new(1, 0)
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_eof() {
            write!(f, "EOF")
        } else {
            write!(f, "line {}, position {}", self.line, self.pos)
        }
    }
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_eof() {
            write!(f, "(EOF)")
        } else {
            write!(f, "({}:{})", self.line, self.pos)
        }
    }
}

/// Compiled AST (abstract syntax tree) of a Rhai script.
pub struct AST(pub(crate) Vec<Stmt>, pub(crate) Vec<FnDef<'static>>);

#[derive(Debug, Clone)]
pub struct FnDef<'a> {
    pub name: Cow<'a, str>,
    pub params: Vec<Cow<'a, str>>,
    pub body: Box<Stmt>,
    pub pos: Position,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    IfElse(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Loop(Box<Stmt>),
    For(String, Box<Expr>, Box<Stmt>),
    Let(String, Option<Box<Expr>>, Position),
    Block(Vec<Stmt>),
    Expr(Box<Expr>),
    Break(Position),
    ReturnWithVal(Option<Box<Expr>>, bool, Position),
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntegerConstant(i64, Position),
    FloatConstant(f64, Position),
    Identifier(String, Position),
    CharConstant(char, Position),
    StringConstant(String, Position),
    FunctionCall(String, Vec<Expr>, Option<Dynamic>, Position),
    Assignment(Box<Expr>, Box<Expr>, Position),
    Dot(Box<Expr>, Box<Expr>, Position),
    Index(Box<Expr>, Box<Expr>, Position),
    Array(Vec<Expr>, Position),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    True(Position),
    False(Position),
    Unit(Position),
}

impl Expr {
    pub fn position(&self) -> Position {
        match self {
            Expr::IntegerConstant(_, pos)
            | Expr::FloatConstant(_, pos)
            | Expr::Identifier(_, pos)
            | Expr::CharConstant(_, pos)
            | Expr::StringConstant(_, pos)
            | Expr::FunctionCall(_, _, _, pos)
            | Expr::Array(_, pos)
            | Expr::True(pos)
            | Expr::False(pos)
            | Expr::Unit(pos) => *pos,

            Expr::Index(e, _, _)
            | Expr::Assignment(e, _, _)
            | Expr::Dot(e, _, _)
            | Expr::And(e, _)
            | Expr::Or(e, _) => e.position(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    IntegerConstant(i64),
    FloatConstant(f64),
    Identifier(String),
    CharConstant(char),
    StringConst(String),
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Plus,
    UnaryPlus,
    Minus,
    UnaryMinus,
    Multiply,
    Divide,
    SemiColon,
    Colon,
    Comma,
    Period,
    Equals,
    True,
    False,
    Let,
    If,
    Else,
    While,
    Loop,
    LessThan,
    GreaterThan,
    Bang,
    LessThanEqualsTo,
    GreaterThanEqualsTo,
    EqualsTo,
    NotEqualsTo,
    Pipe,
    Or,
    Ampersand,
    And,
    Fn,
    Break,
    Return,
    Throw,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    LeftShiftAssign,
    RightShiftAssign,
    AndAssign,
    OrAssign,
    XOrAssign,
    LeftShift,
    RightShift,
    XOr,
    Modulo,
    ModuloAssign,
    PowerOf,
    PowerOfAssign,
    For,
    In,
    LexError(LexError),
}

impl Token {
    pub fn syntax<'a>(&'a self) -> Cow<'a, str> {
        use self::Token::*;

        match *self {
            IntegerConstant(ref i) => i.to_string().into(),
            FloatConstant(ref f) => f.to_string().into(),
            Identifier(ref s) => s.into(),
            CharConstant(ref c) => c.to_string().into(),
            LexError(ref err) => err.to_string().into(),

            ref token => (match token {
                StringConst(_) => "string",
                LeftBrace => "{",
                RightBrace => "}",
                LeftParen => "(",
                RightParen => ")",
                LeftBracket => "[",
                RightBracket => "]",
                Plus => "+",
                UnaryPlus => "+",
                Minus => "-",
                UnaryMinus => "-",
                Multiply => "*",
                Divide => "/",
                SemiColon => ";",
                Colon => ":",
                Comma => ",",
                Period => ".",
                Equals => "=",
                True => "true",
                False => "false",
                Let => "let",
                If => "if",
                Else => "else",
                While => "while",
                Loop => "loop",
                LessThan => "<",
                GreaterThan => ">",
                Bang => "!",
                LessThanEqualsTo => "<=",
                GreaterThanEqualsTo => ">=",
                EqualsTo => "==",
                NotEqualsTo => "!=",
                Pipe => "|",
                Or => "||",
                Ampersand => "&",
                And => "&&",
                Fn => "fn",
                Break => "break",
                Return => "return",
                Throw => "throw",
                PlusAssign => "+=",
                MinusAssign => "-=",
                MultiplyAssign => "*=",
                DivideAssign => "/=",
                LeftShiftAssign => "<<=",
                RightShiftAssign => ">>=",
                AndAssign => "&=",
                OrAssign => "|=",
                XOrAssign => "^=",
                LeftShift => "<<",
                RightShift => ">>",
                XOr => "^",
                Modulo => "%",
                ModuloAssign => "%=",
                PowerOf => "~",
                PowerOfAssign => "~=",
                For => "for",
                In => "in",
                _ => panic!("operator should be match in outer scope"),
            })
            .into(),
        }
    }

    // if another operator is after these, it's probably an unary operator
    // not sure about fn's name
    pub fn is_next_unary(&self) -> bool {
        use self::Token::*;

        match *self {
            LeftBrace        | // (+expr) - is unary
            // RightBrace    | {expr} - expr not unary & is closing
            LeftParen        | // {-expr} - is unary
            // RightParen    | (expr) - expr not unary & is closing
            LeftBracket      | // [-expr] - is unary
            // RightBracket  | [expr] - expr not unary & is closing
            Plus             |
            UnaryPlus        |
            Minus            |
            UnaryMinus       |
            Multiply         |
            Divide           |
            Colon            |
            Comma            |
            Period           |
            Equals           |
            LessThan         |
            GreaterThan      |
            Bang             |
            LessThanEqualsTo |
            GreaterThanEqualsTo |
            EqualsTo         |
            NotEqualsTo      |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            If               |
            While            |
            PlusAssign       |
            MinusAssign      |
            MultiplyAssign   |
            DivideAssign     |
            LeftShiftAssign  |
            RightShiftAssign |
            AndAssign        |
            OrAssign         |
            XOrAssign        |
            LeftShift        |
            RightShift       |
            XOr              |
            Modulo           |
            ModuloAssign     |
            Return           |
            Throw            |
            PowerOf          |
            In               |
            PowerOfAssign => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_binary_op(&self) -> bool {
        use self::Token::*;

        match *self {
            RightBrace | RightParen | RightBracket | Plus | Minus | Multiply | Divide | Comma
            | Equals | LessThan | GreaterThan | LessThanEqualsTo | GreaterThanEqualsTo
            | EqualsTo | NotEqualsTo | Pipe | Or | Ampersand | And | PowerOf => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_unary_op(&self) -> bool {
        use self::Token::*;

        match *self {
            UnaryPlus | UnaryMinus | Equals | Bang | Return | Throw => true,
            _ => false,
        }
    }
}

pub struct TokenIterator<'a> {
    last: Token,
    pos: Position,
    char_stream: Peekable<Chars<'a>>,
}

impl<'a> TokenIterator<'a> {
    fn advance(&mut self) {
        self.pos.advance();
    }
    fn rewind(&mut self) {
        self.pos.rewind();
    }
    fn new_line(&mut self) {
        self.pos.new_line()
    }

    pub fn parse_string_const(
        &mut self,
        enclosing_char: char,
    ) -> Result<String, (LexError, Position)> {
        let mut result = Vec::new();
        let mut escape = String::with_capacity(12);

        loop {
            let next_char = self.char_stream.next();

            if next_char.is_none() {
                return Err((LERR::UnterminatedString, Position::eof()));
            }

            self.advance();

            match next_char.unwrap() {
                '\\' if escape.is_empty() => {
                    escape.push('\\');
                }
                '\\' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\\');
                }
                't' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\t');
                }
                'n' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\n');
                }
                'r' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\r');
                }
                'x' if !escape.is_empty() => {
                    let mut seq = escape.clone();
                    seq.push('x');
                    escape.clear();
                    let mut out_val: u32 = 0;
                    for _ in 0..2 {
                        if let Some(c) = self.char_stream.next() {
                            seq.push(c);
                            self.advance();

                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                        }
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                    }
                }
                'u' if !escape.is_empty() => {
                    let mut seq = escape.clone();
                    seq.push('u');
                    escape.clear();
                    let mut out_val: u32 = 0;
                    for _ in 0..4 {
                        if let Some(c) = self.char_stream.next() {
                            seq.push(c);
                            self.advance();

                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                        }
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                    }
                }
                'U' if !escape.is_empty() => {
                    let mut seq = escape.clone();
                    seq.push('U');
                    escape.clear();
                    let mut out_val: u32 = 0;
                    for _ in 0..8 {
                        if let Some(c) = self.char_stream.next() {
                            seq.push(c);
                            self.advance();

                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                            }
                        } else {
                            return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                        }
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err((LERR::MalformedEscapeSequence(seq), self.pos));
                    }
                }
                x if enclosing_char == x && !escape.is_empty() => result.push(x),
                x if enclosing_char == x && escape.is_empty() => break,
                _ if !escape.is_empty() => {
                    return Err((LERR::MalformedEscapeSequence(escape), self.pos))
                }
                '\n' => {
                    self.rewind();
                    return Err((LERR::UnterminatedString, self.pos));
                }
                x => {
                    escape.clear();
                    result.push(x);
                }
            }
        }

        Ok(result.iter().collect())
    }

    fn inner_next(&mut self) -> Option<(Token, Position)> {
        while let Some(c) = self.char_stream.next() {
            self.advance();

            let pos = self.pos;

            match c {
                '\n' => self.new_line(),
                '0'..='9' => {
                    let mut result = Vec::new();
                    let mut radix_base: Option<u32> = None;
                    result.push(c);

                    while let Some(&next_char) = self.char_stream.peek() {
                        match next_char {
                            '0'..='9' | '_' => {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();
                            }
                            '.' => {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&next_char_in_float) = self.char_stream.peek() {
                                    match next_char_in_float {
                                        '0'..='9' | '_' => {
                                            result.push(next_char_in_float);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                            }
                            'x' | 'X' if c == '0' => {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&next_char_in_hex) = self.char_stream.peek() {
                                    match next_char_in_hex {
                                        '0'..='9' | 'a'..='f' | 'A'..='F' | '_' => {
                                            result.push(next_char_in_hex);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(16);
                            }
                            'o' | 'O' if c == '0' => {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&next_char_in_oct) = self.char_stream.peek() {
                                    match next_char_in_oct {
                                        '0'..='8' | '_' => {
                                            result.push(next_char_in_oct);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(8);
                            }
                            'b' | 'B' if c == '0' => {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();
                                while let Some(&next_char_in_binary) = self.char_stream.peek() {
                                    match next_char_in_binary {
                                        '0' | '1' | '_' => {
                                            result.push(next_char_in_binary);
                                            self.char_stream.next();
                                            self.advance();
                                        }
                                        _ => break,
                                    }
                                }
                                radix_base = Some(2);
                            }
                            _ => break,
                        }
                    }

                    if let Some(radix) = radix_base {
                        let out: String = result.iter().skip(2).filter(|&&c| c != '_').collect();

                        return Some((
                            if let Ok(val) = i64::from_str_radix(&out, radix) {
                                Token::IntegerConstant(val)
                            } else {
                                Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                            },
                            pos,
                        ));
                    } else {
                        let out: String = result.iter().filter(|&&c| c != '_').collect();

                        return Some((
                            if let Ok(val) = out.parse::<i64>() {
                                Token::IntegerConstant(val)
                            } else if let Ok(val) = out.parse::<f64>() {
                                Token::FloatConstant(val)
                            } else {
                                Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                            },
                            pos,
                        ));
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut result = Vec::new();
                    result.push(c);

                    while let Some(&next_char) = self.char_stream.peek() {
                        match next_char {
                            x if x.is_ascii_alphanumeric() || x == '_' => {
                                result.push(x);
                                self.char_stream.next();
                                self.advance();
                            }
                            _ => break,
                        }
                    }

                    let out: String = result.iter().collect();

                    return Some((
                        match out.as_str() {
                            "true" => Token::True,
                            "false" => Token::False,
                            "let" => Token::Let,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "loop" => Token::Loop,
                            "break" => Token::Break,
                            "return" => Token::Return,
                            "throw" => Token::Throw,
                            "fn" => Token::Fn,
                            "for" => Token::For,
                            "in" => Token::In,
                            _ => Token::Identifier(out),
                        },
                        pos,
                    ));
                }
                '"' => {
                    return match self.parse_string_const('"') {
                        Ok(out) => Some((Token::StringConst(out), pos)),
                        Err(e) => Some((Token::LexError(e.0), e.1)),
                    }
                }
                '\'' => match self.parse_string_const('\'') {
                    Ok(result) => {
                        let mut chars = result.chars();

                        return Some((
                            if let Some(first_char) = chars.next() {
                                if chars.count() != 0 {
                                    Token::LexError(LERR::MalformedChar(format!("'{}'", result)))
                                } else {
                                    Token::CharConstant(first_char)
                                }
                            } else {
                                Token::LexError(LERR::MalformedChar(format!("'{}'", result)))
                            },
                            pos,
                        ));
                    }
                    Err(e) => return Some((Token::LexError(e.0), e.1)),
                },
                '{' => return Some((Token::LeftBrace, pos)),
                '}' => return Some((Token::RightBrace, pos)),
                '(' => return Some((Token::LeftParen, pos)),
                ')' => return Some((Token::RightParen, pos)),
                '[' => return Some((Token::LeftBracket, pos)),
                ']' => return Some((Token::RightBracket, pos)),
                '+' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::PlusAssign
                            }
                            _ if self.last.is_next_unary() => Token::UnaryPlus,
                            _ => Token::Plus,
                        },
                        pos,
                    ))
                }
                '-' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::MinusAssign
                            }
                            _ if self.last.is_next_unary() => Token::UnaryMinus,
                            _ => Token::Minus,
                        },
                        pos,
                    ))
                }
                '*' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::MultiplyAssign
                            }
                            _ => Token::Multiply,
                        },
                        pos,
                    ))
                }
                '/' => match self.char_stream.peek() {
                    Some(&'/') => {
                        self.char_stream.next();
                        self.advance();
                        while let Some(c) = self.char_stream.next() {
                            match c {
                                '\n' => {
                                    self.new_line();
                                    break;
                                }
                                _ => self.advance(),
                            }
                        }
                    }
                    Some(&'*') => {
                        let mut level = 1;
                        self.char_stream.next();
                        self.advance();
                        while let Some(c) = self.char_stream.next() {
                            self.advance();

                            match c {
                                '/' => {
                                    if let Some('*') = self.char_stream.next() {
                                        level += 1;
                                    }
                                    self.advance();
                                }
                                '*' => {
                                    if let Some('/') = self.char_stream.next() {
                                        level -= 1;
                                    }
                                    self.advance();
                                }
                                '\n' => self.new_line(),
                                _ => (),
                            }

                            if level == 0 {
                                break;
                            }
                        }
                    }
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::DivideAssign, pos));
                    }
                    _ => return Some((Token::Divide, pos)),
                },
                ';' => return Some((Token::SemiColon, pos)),
                ':' => return Some((Token::Colon, pos)),
                ',' => return Some((Token::Comma, pos)),
                '.' => return Some((Token::Period, pos)),
                '=' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::EqualsTo, pos));
                    }
                    _ => return Some((Token::Equals, pos)),
                },
                '<' => match self.char_stream.peek() {
                    Some(&'=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::LessThanEqualsTo, pos));
                    }
                    Some(&'<') => {
                        self.char_stream.next();
                        self.advance();
                        return match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Some((Token::LeftShiftAssign, pos))
                            }
                            _ => {
                                self.char_stream.next();
                                self.advance();
                                Some((Token::LeftShift, pos))
                            }
                        };
                    }
                    _ => return Some((Token::LessThan, pos)),
                },
                '>' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::GreaterThanEqualsTo
                            }
                            Some(&'>') => {
                                self.char_stream.next();
                                self.advance();
                                match self.char_stream.peek() {
                                    Some(&'=') => {
                                        self.char_stream.next();
                                        self.advance();
                                        Token::RightShiftAssign
                                    }
                                    _ => {
                                        self.char_stream.next();
                                        self.advance();
                                        Token::RightShift
                                    }
                                }
                            }
                            _ => Token::GreaterThan,
                        },
                        pos,
                    ))
                }
                '!' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::NotEqualsTo
                            }
                            _ => Token::Bang,
                        },
                        pos,
                    ))
                }
                '|' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'|') => {
                                self.char_stream.next();
                                self.advance();
                                Token::Or
                            }
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::OrAssign
                            }
                            _ => Token::Pipe,
                        },
                        pos,
                    ))
                }
                '&' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'&') => {
                                self.char_stream.next();
                                self.advance();
                                Token::And
                            }
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::AndAssign
                            }
                            _ => Token::Ampersand,
                        },
                        pos,
                    ))
                }
                '^' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::XOrAssign
                            }
                            _ => Token::XOr,
                        },
                        pos,
                    ))
                }
                '%' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::ModuloAssign
                            }
                            _ => Token::Modulo,
                        },
                        pos,
                    ))
                }
                '~' => {
                    return Some((
                        match self.char_stream.peek() {
                            Some(&'=') => {
                                self.char_stream.next();
                                self.advance();
                                Token::PowerOfAssign
                            }
                            _ => Token::PowerOf,
                        },
                        pos,
                    ))
                }
                x if x.is_whitespace() => (),
                x => return Some((Token::LexError(LERR::UnexpectedChar(x)), pos)),
            }
        }

        None
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = (Token, Position);

    // TODO - perhaps this could be optimized?
    fn next(&mut self) -> Option<Self::Item> {
        self.inner_next().map(|x| {
            self.last = x.0.clone();
            x
        })
    }
}

pub fn lex(input: &str) -> TokenIterator<'_> {
    TokenIterator {
        last: Token::LexError(LERR::InputError("".into())),
        pos: Position::new(1, 0),
        char_stream: input.chars().peekable(),
    }
}

fn get_precedence(token: &Token) -> i8 {
    match *token {
        Token::Equals
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::AndAssign
        | Token::OrAssign
        | Token::XOrAssign
        | Token::ModuloAssign
        | Token::PowerOfAssign => 10,
        Token::Or | Token::XOr | Token::Pipe => 11,
        Token::And | Token::Ampersand => 12,
        Token::LessThan
        | Token::LessThanEqualsTo
        | Token::GreaterThan
        | Token::GreaterThanEqualsTo
        | Token::EqualsTo
        | Token::NotEqualsTo => 15,
        Token::Plus | Token::Minus => 20,
        Token::Divide | Token::Multiply | Token::PowerOf => 40,
        Token::LeftShift | Token::RightShift => 50,
        Token::Modulo => 60,
        Token::Period => 100,
        _ => -1,
    }
}

fn parse_paren_expr<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match input.peek() {
        Some((Token::RightParen, _)) => {
            input.next();
            return Ok(Expr::Unit(begin));
        }
        _ => (),
    }

    let expr = parse_expr(input)?;

    match input.next() {
        Some((Token::RightParen, _)) => Ok(expr),
        Some((_, pos)) => {
            return Err(ParseError::new(
                PERR::MissingRightParen("a matching ( in the expression".into()),
                pos,
            ))
        }
        None => Err(ParseError::new(
            PERR::MissingRightParen("a matching ( in the expression".into()),
            Position::eof(),
        )),
    }
}

fn parse_call_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    let mut args = Vec::new();

    if let Some(&(Token::RightParen, _)) = input.peek() {
        input.next();
        return Ok(Expr::FunctionCall(id, args, None, begin));
    }

    loop {
        args.push(parse_expr(input)?);

        match input.peek() {
            Some(&(Token::RightParen, _)) => {
                input.next();
                return Ok(Expr::FunctionCall(id, args, None, begin));
            }
            Some(&(Token::Comma, _)) => (),
            Some(&(_, pos)) => {
                return Err(ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the arguments list to function call of '{}'",
                        id
                    )),
                    pos,
                ))
            }
            None => {
                return Err(ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the arguments list to function call of '{}'",
                        id
                    )),
                    Position::eof(),
                ))
            }
        }

        input.next();
    }
}

fn parse_index_expr<'a>(
    lhs: Box<Expr>,
    input: &mut Peekable<TokenIterator<'a>>,
    pos: Position,
) -> Result<Expr, ParseError> {
    parse_expr(input).and_then(|idx_expr| match input.peek() {
        Some(&(Token::RightBracket, _)) => {
            input.next();
            return Ok(Expr::Index(lhs, Box::new(idx_expr), pos));
        }
        Some(&(_, pos)) => {
            return Err(ParseError::new(
                PERR::MissingRightBracket("index expression".into()),
                pos,
            ))
        }
        None => {
            return Err(ParseError::new(
                PERR::MissingRightBracket("index expression".into()),
                Position::eof(),
            ))
        }
    })
}

fn parse_ident_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match input.peek() {
        Some(&(Token::LeftParen, _)) => {
            input.next();
            parse_call_expr(id, input, begin)
        }
        Some(&(Token::LeftBracket, pos)) => {
            input.next();
            parse_index_expr(Box::new(Expr::Identifier(id, begin)), input, pos)
        }
        Some(_) => Ok(Expr::Identifier(id, begin)),
        None => Ok(Expr::Identifier(id, Position::eof())),
    }
}

fn parse_array_expr<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    let mut arr = Vec::new();

    match input.peek() {
        Some(&(Token::RightBracket, _)) => (),

        _ => {
            while input.peek().is_some() {
                arr.push(parse_expr(input)?);

                if let Some(&(Token::Comma, _)) = input.peek() {
                    input.next();
                }

                if let Some(&(Token::RightBracket, _)) = input.peek() {
                    break;
                }
            }
        }
    }

    match input.peek() {
        Some(&(Token::RightBracket, _)) => {
            input.next();
            Ok(Expr::Array(arr, begin))
        }
        Some(&(_, pos)) => Err(ParseError::new(
            PERR::MissingRightBracket("the end of array literal".into()),
            pos,
        )),
        None => Err(ParseError::new(
            PERR::MissingRightBracket("the end of array literal".into()),
            Position::eof(),
        )),
    }
}

fn parse_primary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let token = input.next();

    let mut follow_on = false;

    let mut root_expr = match token {
        Some((Token::IntegerConstant(x), pos)) => Ok(Expr::IntegerConstant(x, pos)),
        Some((Token::FloatConstant(x), pos)) => Ok(Expr::FloatConstant(x, pos)),
        Some((Token::CharConstant(c), pos)) => Ok(Expr::CharConstant(c, pos)),
        Some((Token::StringConst(s), pos)) => {
            follow_on = true;
            Ok(Expr::StringConstant(s, pos))
        }
        Some((Token::Identifier(s), pos)) => {
            follow_on = true;
            parse_ident_expr(s, input, pos)
        }
        Some((Token::LeftParen, pos)) => {
            follow_on = true;
            parse_paren_expr(input, pos)
        }
        Some((Token::LeftBracket, pos)) => {
            follow_on = true;
            parse_array_expr(input, pos)
        }
        Some((Token::True, pos)) => Ok(Expr::True(pos)),
        Some((Token::False, pos)) => Ok(Expr::False(pos)),
        Some((Token::LexError(le), pos)) => {
            Err(ParseError::new(PERR::BadInput(le.to_string()), pos))
        }
        Some((token, pos)) => Err(ParseError::new(
            PERR::BadInput(format!("Unexpected '{}'", token.syntax())),
            pos,
        )),
        None => Err(ParseError::new(PERR::InputPastEndOfFile, Position::eof())),
    }?;

    if !follow_on {
        return Ok(root_expr);
    }

    // Tail processing all possible indexing
    while let Some(&(Token::LeftBracket, pos)) = input.peek() {
        input.next();
        root_expr = parse_index_expr(Box::new(root_expr), input, pos)?;
    }

    Ok(root_expr)
}

fn parse_unary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input.peek() {
        Some(&(Token::UnaryMinus, pos)) => {
            input.next();

            Ok(Expr::FunctionCall(
                "-".into(),
                vec![parse_primary(input)?],
                None,
                pos,
            ))
        }
        Some(&(Token::UnaryPlus, _)) => {
            input.next();
            parse_primary(input)
        }
        Some(&(Token::Bang, pos)) => {
            input.next();

            Ok(Expr::FunctionCall(
                "!".into(),
                vec![parse_primary(input)?],
                Some(Box::new(false)), // NOT operator, when operating on invalid operand, defaults to false
                pos,
            ))
        }
        _ => parse_primary(input),
    }
}

fn parse_assignment(lhs: Expr, rhs: Expr, pos: Position) -> Result<Expr, ParseError> {
    fn all_identifiers(expr: &Expr) -> (bool, Position) {
        match expr {
            // variable
            Expr::Identifier(_, pos) => (true, *pos),
            // indexing
            Expr::Index(lhs, _, idx_pos) => match lhs.as_ref() {
                // variable[x]
                &Expr::Identifier(_, pos) => (true, pos),
                // all other indexing is invalid
                _ => (false, *idx_pos),
            },
            // variable.prop.prop.prop...
            Expr::Dot(lhs, rhs, _) => match lhs.as_ref() {
                // variable.prop
                &Expr::Identifier(_, pos) => {
                    let r = all_identifiers(rhs);
                    (r.0, if r.0 { pos } else { r.1 })
                }
                // all other property access is invalid
                _ => (false, lhs.position()),
            },
            // everything else is invalid
            _ => (false, expr.position()),
        }
    }

    let r = all_identifiers(&lhs);

    if r.0 {
        Ok(Expr::Assignment(Box::new(lhs), Box::new(rhs), pos))
    } else {
        Err(ParseError::new(PERR::AssignmentToInvalidLHS, r.1))
    }
}

fn parse_binary_op<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    precedence: i8,
    lhs: Expr,
) -> Result<Expr, ParseError> {
    let mut current_lhs = lhs;

    loop {
        let mut current_precedence = -1;

        if let Some(&(ref current_op, _)) = input.peek() {
            current_precedence = get_precedence(current_op);
        }

        if current_precedence < precedence {
            return Ok(current_lhs);
        }

        if let Some((op_token, pos)) = input.next() {
            input.peek();

            let mut rhs = parse_unary(input)?;

            let mut next_precedence = -1;

            if let Some(&(ref next_op, _)) = input.peek() {
                next_precedence = get_precedence(next_op);
            }

            if current_precedence < next_precedence {
                rhs = parse_binary_op(input, current_precedence + 1, rhs)?;
            } else if current_precedence >= 100 {
                // Always bind right to left for precedence over 100
                rhs = parse_binary_op(input, current_precedence, rhs)?;
            }

            current_lhs = match op_token {
                Token::Plus => Expr::FunctionCall("+".into(), vec![current_lhs, rhs], None, pos),
                Token::Minus => Expr::FunctionCall("-".into(), vec![current_lhs, rhs], None, pos),
                Token::Multiply => {
                    Expr::FunctionCall("*".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::Divide => Expr::FunctionCall("/".into(), vec![current_lhs, rhs], None, pos),

                Token::Equals => parse_assignment(current_lhs, rhs, pos)?,
                Token::PlusAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("+".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::MinusAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("-".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::Period => Expr::Dot(Box::new(current_lhs), Box::new(rhs), pos),

                // Comparison operators default to false when passed invalid operands
                Token::EqualsTo => Expr::FunctionCall(
                    "==".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),
                Token::NotEqualsTo => Expr::FunctionCall(
                    "!=".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),
                Token::LessThan => Expr::FunctionCall(
                    "<".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),
                Token::LessThanEqualsTo => Expr::FunctionCall(
                    "<=".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),
                Token::GreaterThan => Expr::FunctionCall(
                    ">".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),
                Token::GreaterThanEqualsTo => Expr::FunctionCall(
                    ">=".into(),
                    vec![current_lhs, rhs],
                    Some(Box::new(false)),
                    pos,
                ),

                Token::Or => Expr::Or(Box::new(current_lhs), Box::new(rhs)),
                Token::And => Expr::And(Box::new(current_lhs), Box::new(rhs)),
                Token::XOr => Expr::FunctionCall("^".into(), vec![current_lhs, rhs], None, pos),
                Token::OrAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("|".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::AndAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("&".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::XOrAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("^".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::MultiplyAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("*".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::DivideAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("/".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::Pipe => Expr::FunctionCall("|".into(), vec![current_lhs, rhs], None, pos),
                Token::LeftShift => {
                    Expr::FunctionCall("<<".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::RightShift => {
                    Expr::FunctionCall(">>".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::LeftShiftAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("<<".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::RightShiftAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall(">>".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::Ampersand => {
                    Expr::FunctionCall("&".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::Modulo => Expr::FunctionCall("%".into(), vec![current_lhs, rhs], None, pos),
                Token::ModuloAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("%".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                Token::PowerOf => Expr::FunctionCall("~".into(), vec![current_lhs, rhs], None, pos),
                Token::PowerOfAssign => {
                    let lhs_copy = current_lhs.clone();
                    parse_assignment(
                        current_lhs,
                        Expr::FunctionCall("~".into(), vec![lhs_copy, rhs], None, pos),
                        pos,
                    )?
                }
                token => {
                    return Err(ParseError::new(
                        PERR::UnknownOperator(token.syntax().into()),
                        pos,
                    ))
                }
            };
        }
    }
}

fn parse_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let lhs = parse_unary(input)?;
    parse_binary_op(input, 0, lhs)
}

fn parse_if<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = parse_expr(input)?;
    let body = parse_block(input)?;

    match input.peek() {
        Some(&(Token::Else, _)) => {
            input.next();

            let else_body = match input.peek() {
                Some(&(Token::If, _)) => parse_if(input)?,
                _ => parse_block(input)?,
            };

            Ok(Stmt::IfElse(
                Box::new(guard),
                Box::new(body),
                Some(Box::new(else_body)),
            ))
        }
        _ => Ok(Stmt::IfElse(Box::new(guard), Box::new(body), None)),
    }
}

fn parse_while<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = parse_expr(input)?;
    let body = parse_block(input)?;

    Ok(Stmt::While(Box::new(guard), Box::new(body)))
}

fn parse_loop<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let body = parse_block(input)?;

    Ok(Stmt::Loop(Box::new(body)))
}

fn parse_for<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError::new(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError::new(PERR::VarExpectsIdentifier, Position::eof())),
    };

    match input.next() {
        Some((Token::In, _)) => {}
        Some((_, pos)) => return Err(ParseError::new(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError::new(PERR::VarExpectsIdentifier, Position::eof())),
    }

    let expr = parse_expr(input)?;

    let body = parse_block(input)?;

    Ok(Stmt::For(name, Box::new(expr), Box::new(body)))
}

fn parse_var<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    let pos = match input.next() {
        Some((_, tok_pos)) => tok_pos,
        _ => return Err(ParseError::new(PERR::InputPastEndOfFile, Position::eof())),
    };

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError::new(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError::new(PERR::VarExpectsIdentifier, Position::eof())),
    };

    match input.peek() {
        Some(&(Token::Equals, _)) => {
            input.next();
            let init_value = parse_expr(input)?;
            Ok(Stmt::Let(name, Some(Box::new(init_value)), pos))
        }
        _ => Ok(Stmt::Let(name, None, pos)),
    }
}

fn parse_block<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(&(Token::LeftBrace, _)) => (),
        Some(&(_, pos)) => return Err(ParseError::new(PERR::MissingLeftBrace, pos)),
        None => return Err(ParseError::new(PERR::MissingLeftBrace, Position::eof())),
    }

    input.next();

    let mut statements = Vec::new();

    match input.peek() {
        Some(&(Token::RightBrace, _)) => (), // empty block
        Some(&(Token::Fn, pos)) => return Err(ParseError::new(PERR::WrongFnDefinition, pos)),

        _ => {
            while input.peek().is_some() {
                // Parse statements inside the block
                statements.push(parse_stmt(input)?);

                // Notice semicolons are optional
                if let Some(&(Token::SemiColon, _)) = input.peek() {
                    input.next();
                }

                if let Some(&(Token::RightBrace, _)) = input.peek() {
                    break;
                }
            }
        }
    }

    match input.peek() {
        Some(&(Token::RightBrace, _)) => {
            input.next();
            Ok(Stmt::Block(statements))
        }
        Some(&(_, pos)) => Err(ParseError::new(
            PERR::MissingRightBrace("end of block".into()),
            pos,
        )),
        None => Err(ParseError::new(
            PERR::MissingRightBrace("end of block".into()),
            Position::eof(),
        )),
    }
}

fn parse_expr_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    Ok(Stmt::Expr(Box::new(parse_expr(input)?)))
}

fn parse_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(&(Token::If, _)) => parse_if(input),
        Some(&(Token::While, _)) => parse_while(input),
        Some(&(Token::Loop, _)) => parse_loop(input),
        Some(&(Token::For, _)) => parse_for(input),
        Some(&(Token::Break, pos)) => {
            input.next();
            Ok(Stmt::Break(pos))
        }
        Some(&(ref token @ Token::Return, _)) | Some(&(ref token @ Token::Throw, _)) => {
            let is_return = match token {
                Token::Return => true,
                Token::Throw => false,
                _ => panic!(),
            };

            input.next();

            match input.peek() {
                // return; or throw;
                Some(&(Token::SemiColon, pos)) => Ok(Stmt::ReturnWithVal(None, is_return, pos)),
                // Just a return/throw without anything at the end of script
                None => Ok(Stmt::ReturnWithVal(None, is_return, Position::eof())),
                // return or throw with expression
                Some(&(_, pos)) => {
                    let ret = parse_expr(input)?;
                    Ok(Stmt::ReturnWithVal(Some(Box::new(ret)), is_return, pos))
                }
            }
        }
        Some(&(Token::LeftBrace, _)) => parse_block(input),
        Some(&(Token::Let, _)) => parse_var(input),
        _ => parse_expr_stmt(input),
    }
}

fn parse_fn<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<FnDef<'static>, ParseError> {
    let pos = match input.next() {
        Some((_, tok_pos)) => tok_pos,
        _ => return Err(ParseError::new(PERR::InputPastEndOfFile, Position::eof())),
    };

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError::new(PERR::FnMissingName, pos)),
        None => return Err(ParseError::new(PERR::FnMissingName, Position::eof())),
    };

    match input.peek() {
        Some(&(Token::LeftParen, _)) => {
            input.next();
        }
        Some(&(_, pos)) => return Err(ParseError::new(PERR::FnMissingParams(name), pos)),
        None => {
            return Err(ParseError::new(
                PERR::FnMissingParams(name),
                Position::eof(),
            ))
        }
    }

    let mut params = Vec::new();

    match input.peek() {
        Some(&(Token::RightParen, _)) => {
            input.next();
        }
        _ => loop {
            match input.next() {
                Some((Token::RightParen, _)) => break,
                Some((Token::Comma, _)) => (),
                Some((Token::Identifier(s), _)) => {
                    params.push(s.into());
                }
                Some((_, pos)) => return Err(ParseError::new(PERR::MalformedCallExpr, pos)),
                None => return Err(ParseError::new(PERR::MalformedCallExpr, Position::eof())),
            }
        },
    }

    let body = parse_block(input)?;

    Ok(FnDef {
        name: name.into(),
        params: params,
        body: Box::new(body),
        pos: pos,
    })
}

fn parse_top_level<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<AST, ParseError> {
    let mut statements = Vec::new();
    let mut functions = Vec::new();

    while input.peek().is_some() {
        match input.peek() {
            Some(&(Token::Fn, _)) => functions.push(parse_fn(input)?),
            _ => statements.push(parse_stmt(input)?),
        }

        // Notice semicolons are optional
        if let Some(&(Token::SemiColon, _)) = input.peek() {
            input.next();
        }
    }

    Ok(AST(statements, functions))
}

pub fn parse<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<AST, ParseError> {
    parse_top_level(input)
}
