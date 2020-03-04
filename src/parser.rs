use crate::Dynamic;
use std::char;
use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum LexError {
    UnexpectedChar(char),
    UnterminatedString,
    MalformedEscapeSequence(String),
    MalformedNumber(String),
    MalformedChar(String),
    InputError(String),
}

type LERR = LexError;

impl Error for LexError {
    fn description(&self) -> &str {
        match *self {
            LERR::UnexpectedChar(_) => "Unexpected character",
            LERR::UnterminatedString => "Open string is not terminated",
            LERR::MalformedEscapeSequence(_) => "Unexpected values in escape sequence",
            LERR::MalformedNumber(_) => "Unexpected characters in number",
            LERR::MalformedChar(_) => "Char constant not a single character",
            LERR::InputError(_) => "Input error",
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LERR::UnexpectedChar(c) => write!(f, "Unexpected '{}'", c),
            LERR::MalformedEscapeSequence(s) => write!(f, "Invalid escape sequence: '{}'", s),
            LERR::MalformedNumber(s) => write!(f, "Invalid number: '{}'", s),
            LERR::MalformedChar(s) => write!(f, "Invalid character: '{}'", s),
            LERR::InputError(s) => write!(f, "{}", s),
            _ => write!(f, "{}", self.description()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorType {
    BadInput(String),
    InputPastEndOfFile,
    UnknownOperator(String),
    MissingRightParen,
    MissingLeftBrace,
    MissingRightBrace,
    MissingRightBracket,
    MalformedCallExpr,
    MalformedIndexExpr,
    VarExpectsIdentifier,
    WrongFnDefinition,
    FnMissingName,
    FnMissingParams(String),
}

type PERR = ParseErrorType;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct Position {
    line: usize,
    pos: usize,
}

impl Position {
    pub fn new() -> Self {
        Self { line: 1, pos: 0 }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn position(&self) -> usize {
        self.pos
    }

    pub(crate) fn advance(&mut self) {
        self.pos += 1;
    }

    pub(crate) fn rewind(&mut self) {
        if self.pos == 0 {
            panic!("cannot rewind at position 0");
        } else {
            self.pos -= 1;
        }
    }

    pub(crate) fn new_line(&mut self) {
        self.line += 1;
        self.pos = 0;
    }

    pub fn eof() -> Self {
        Self { line: 0, pos: 0 }
    }

    pub fn is_eof(&self) -> bool {
        self.line == 0
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_eof() {
            write!(f, "EOF")
        } else {
            write!(f, "line {}, position {}", self.line, self.pos)
        }
    }
}

impl std::fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_eof() {
            write!(f, "(EOF)")
        } else {
            write!(f, "({}:{})", self.line, self.pos)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError(PERR, Position);

impl ParseError {
    pub fn error_type(&self) -> &PERR {
        &self.0
    }
    pub fn line(&self) -> usize {
        self.1.line()
    }
    pub fn position(&self) -> usize {
        self.1.position()
    }
    pub fn is_eof(&self) -> bool {
        self.1.is_eof()
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.0 {
            PERR::BadInput(ref p) => p,
            PERR::InputPastEndOfFile => "Script is incomplete",
            PERR::UnknownOperator(_) => "Unknown operator",
            PERR::MissingRightParen => "Expecting ')'",
            PERR::MissingLeftBrace => "Expecting '{'",
            PERR::MissingRightBrace => "Expecting '}'",
            PERR::MissingRightBracket => "Expecting ']'",
            PERR::MalformedCallExpr => "Invalid expression in function call arguments",
            PERR::MalformedIndexExpr => "Invalid index in indexing expression",
            PERR::VarExpectsIdentifier => "Expecting name of a variable",
            PERR::FnMissingName => "Expecting name in function declaration",
            PERR::FnMissingParams(_) => "Expecting parameters in function declaration",
            PERR::WrongFnDefinition => "Function definitions must be at top level and cannot be inside a block or another function",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            PERR::BadInput(ref s) => write!(f, "{}", s)?,
            PERR::UnknownOperator(ref s) => write!(f, "{}: '{}'", self.description(), s)?,
            PERR::FnMissingParams(ref s) => write!(f, "Missing parameters for function '{}'", s)?,
            _ => write!(f, "{}", self.description())?,
        }

        if !self.is_eof() {
            write!(f, " ({})", self.1)
        } else {
            write!(f, " at the end of the script but there is no more input")
        }
    }
}

pub struct AST(pub(crate) Vec<Stmt>, pub(crate) Vec<FnDef>);

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<String>,
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
    Assignment(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
    Index(String, Box<Expr>, Position),
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
            | Expr::Index(_, _, pos)
            | Expr::Array(_, pos)
            | Expr::True(pos)
            | Expr::False(pos)
            | Expr::Unit(pos) => *pos,

            Expr::Assignment(_, _) | Expr::Dot(_, _) | Expr::And(_, _) | Expr::Or(_, _) => panic!(),
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
    LexErr(LexError),
}

impl Token {
    pub fn syntax(&self) -> std::borrow::Cow<'static, str> {
        use self::Token::*;

        match *self {
            IntegerConstant(ref s) => s.to_string().into(),
            FloatConstant(ref s) => s.to_string().into(),
            Identifier(ref s) => s.to_string().into(),
            CharConstant(ref s) => s.to_string().into(),
            LexErr(ref err) => err.to_string().into(),

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
                _ => panic!(),
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
    pub fn is_bin_op(&self) -> bool {
        use self::Token::*;

        match *self {
            RightBrace       |
            RightParen       |
            RightBracket     |
            Plus             |
            Minus            |
            Multiply         |
            Divide           |
            Comma            |
            // Period           | <- does period count?
            Equals           |
            LessThan         |
            GreaterThan      |
            LessThanEqualsTo |
            GreaterThanEqualsTo |
            EqualsTo         |
            NotEqualsTo      |
            Pipe             |
            Or               |
            Ampersand        |
            And              |
            PowerOf => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_un_op(&self) -> bool {
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

        let out: String = result.iter().collect();
        Ok(out)
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
                                Token::LexErr(LERR::MalformedNumber(result.iter().collect()))
                            },
                            pos,
                        ));
                    }

                    let out: String = result.iter().filter(|&&c| c != '_').collect();

                    return Some((
                        if let Ok(val) = out.parse::<i64>() {
                            Token::IntegerConstant(val)
                        } else if let Ok(val) = out.parse::<f64>() {
                            Token::FloatConstant(val)
                        } else {
                            Token::LexErr(LERR::MalformedNumber(result.iter().collect()))
                        },
                        pos,
                    ));
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
                            x => Token::Identifier(x.into()),
                        },
                        pos,
                    ));
                }
                '"' => {
                    return match self.parse_string_const('"') {
                        Ok(out) => Some((Token::StringConst(out), pos)),
                        Err(e) => Some((Token::LexErr(e.0), e.1)),
                    }
                }
                '\'' => match self.parse_string_const('\'') {
                    Ok(result) => {
                        let mut chars = result.chars();

                        return Some((
                            if let Some(first_char) = chars.next() {
                                if chars.count() != 0 {
                                    Token::LexErr(LERR::MalformedChar(format!("'{}'", result)))
                                } else {
                                    Token::CharConstant(first_char)
                                }
                            } else {
                                Token::LexErr(LERR::MalformedChar(format!("'{}'", result)))
                            },
                            pos,
                        ));
                    }
                    Err(e) => return Some((Token::LexErr(e.0), e.1)),
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
                                    self.advance();
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
                                '\n' => self.advance(),
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
                x => return Some((Token::LexErr(LERR::UnexpectedChar(x)), pos)),
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
        last: Token::LexErr(LERR::InputError("".into())),
        pos: Position { line: 1, pos: 0 },
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
        _ => Err(ParseError(PERR::MissingRightParen, Position::eof())),
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
            Some(&(_, pos)) => return Err(ParseError(PERR::MalformedCallExpr, pos)),
            None => return Err(ParseError(PERR::MalformedCallExpr, Position::eof())),
        }

        input.next();
    }
}

fn parse_index_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match parse_expr(input) {
        Ok(idx) => match input.peek() {
            Some(&(Token::RightBracket, _)) => {
                input.next();
                return Ok(Expr::Index(id, Box::new(idx), begin));
            }
            Some(&(_, pos)) => return Err(ParseError(PERR::MalformedIndexExpr, pos)),
            None => return Err(ParseError(PERR::MalformedIndexExpr, Position::eof())),
        },
        Err(mut err) => {
            err.0 = PERR::MalformedIndexExpr;
            return Err(err);
        }
    }
}

fn parse_ident_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match input.peek() {
        Some(&(Token::LeftParen, pos)) => {
            input.next();
            parse_call_expr(id, input, pos)
        }
        Some(&(Token::LeftBracket, pos)) => {
            input.next();
            parse_index_expr(id, input, pos)
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
        Some(&(_, pos)) => Err(ParseError(PERR::MissingRightBracket, pos)),
        None => Err(ParseError(PERR::MissingRightBracket, Position::eof())),
    }
}

fn parse_primary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input.next() {
        Some((Token::IntegerConstant(x), pos)) => Ok(Expr::IntegerConstant(x, pos)),
        Some((Token::FloatConstant(x), pos)) => Ok(Expr::FloatConstant(x, pos)),
        Some((Token::StringConst(s), pos)) => Ok(Expr::StringConstant(s, pos)),
        Some((Token::CharConstant(c), pos)) => Ok(Expr::CharConstant(c, pos)),
        Some((Token::Identifier(s), pos)) => parse_ident_expr(s, input, pos),
        Some((Token::LeftParen, pos)) => parse_paren_expr(input, pos),
        Some((Token::LeftBracket, pos)) => parse_array_expr(input, pos),
        Some((Token::True, pos)) => Ok(Expr::True(pos)),
        Some((Token::False, pos)) => Ok(Expr::False(pos)),
        Some((Token::LexErr(le), pos)) => Err(ParseError(PERR::BadInput(le.to_string()), pos)),
        Some((token, pos)) => Err(ParseError(
            PERR::BadInput(format!("Unexpected '{}'", token.syntax())),
            pos,
        )),
        None => Err(ParseError(PERR::InputPastEndOfFile, Position::eof())),
    }
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

                Token::Equals => Expr::Assignment(Box::new(current_lhs), Box::new(rhs)),
                Token::PlusAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "+".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::MinusAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "-".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::Period => Expr::Dot(Box::new(current_lhs), Box::new(rhs)),

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
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "|".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::AndAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "&".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::XOrAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "^".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::MultiplyAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "*".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::DivideAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "/".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
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
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "<<".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::RightShiftAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            ">>".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::Ampersand => {
                    Expr::FunctionCall("&".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::Modulo => Expr::FunctionCall("%".into(), vec![current_lhs, rhs], None, pos),
                Token::ModuloAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "%".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                Token::PowerOf => Expr::FunctionCall("~".into(), vec![current_lhs, rhs], None, pos),
                Token::PowerOfAssign => {
                    let lhs_copy = current_lhs.clone();
                    Expr::Assignment(
                        Box::new(current_lhs),
                        Box::new(Expr::FunctionCall(
                            "~".into(),
                            vec![lhs_copy, rhs],
                            None,
                            pos,
                        )),
                    )
                }
                token => {
                    return Err(ParseError(
                        PERR::UnknownOperator(token.syntax().to_string()),
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
        Some((_, pos)) => return Err(ParseError(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError(PERR::VarExpectsIdentifier, Position::eof())),
    };

    match input.next() {
        Some((Token::In, _)) => {}
        Some((_, pos)) => return Err(ParseError(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError(PERR::VarExpectsIdentifier, Position::eof())),
    }

    let expr = parse_expr(input)?;

    let body = parse_block(input)?;

    Ok(Stmt::For(name, Box::new(expr), Box::new(body)))
}

fn parse_var<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    let pos = match input.next() {
        Some((_, tok_pos)) => tok_pos,
        _ => return Err(ParseError(PERR::InputPastEndOfFile, Position::eof())),
    };

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError(PERR::VarExpectsIdentifier, Position::eof())),
    };

    match input.peek() {
        Some(&(Token::Equals, _)) => {
            input.next();
            let initializer = parse_expr(input)?;
            Ok(Stmt::Let(name, Some(Box::new(initializer)), pos))
        }
        _ => Ok(Stmt::Let(name, None, pos)),
    }
}

fn parse_block<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    match input.peek() {
        Some(&(Token::LeftBrace, _)) => (),
        Some(&(_, pos)) => return Err(ParseError(PERR::MissingLeftBrace, pos)),
        None => return Err(ParseError(PERR::MissingLeftBrace, Position::eof())),
    }

    input.next();

    let mut statements = Vec::new();

    match input.peek() {
        Some(&(Token::RightBrace, _)) => (), // empty block
        Some(&(Token::Fn, pos)) => return Err(ParseError(PERR::WrongFnDefinition, pos)),

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
        Some(&(_, pos)) => Err(ParseError(PERR::MissingRightBrace, pos)),
        None => Err(ParseError(PERR::MissingRightBrace, Position::eof())),
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

fn parse_fn<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<FnDef, ParseError> {
    let pos = match input.next() {
        Some((_, tok_pos)) => tok_pos,
        _ => return Err(ParseError(PERR::InputPastEndOfFile, Position::eof())),
    };

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError(PERR::FnMissingName, pos)),
        None => return Err(ParseError(PERR::FnMissingName, Position::eof())),
    };

    match input.peek() {
        Some(&(Token::LeftParen, _)) => {
            input.next();
        }
        Some(&(_, pos)) => return Err(ParseError(PERR::FnMissingParams(name), pos)),
        None => return Err(ParseError(PERR::FnMissingParams(name), Position::eof())),
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
                    params.push(s);
                }
                Some((_, pos)) => return Err(ParseError(PERR::MalformedCallExpr, pos)),
                None => return Err(ParseError(PERR::MalformedCallExpr, Position::eof())),
            }
        },
    }

    let body = parse_block(input)?;

    Ok(FnDef {
        name: name,
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
