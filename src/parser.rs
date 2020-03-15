//! Main module defining the lexer and parser.

use crate::any::{Any, AnyExt, Dynamic};
use crate::engine::Engine;
use crate::error::{LexError, ParseError, ParseErrorType};
use crate::scope::{Scope, VariableType};

#[cfg(not(feature = "no_optimize"))]
use crate::optimize::optimize_ast;

use std::{
    borrow::Cow, char, cmp::Ordering, fmt, iter::Peekable, str::Chars, str::FromStr, sync::Arc,
    usize,
};

/// The system integer type.
///
/// If the `only_i32` feature is enabled, this will be `i32` instead.
#[cfg(not(feature = "only_i32"))]
pub type INT = i64;

/// The system integer type
///
/// If the `only_i32` feature is not enabled, this will be `i64` instead.
#[cfg(feature = "only_i32")]
pub type INT = i32;

/// The system floating-point type
#[cfg(not(feature = "no_float"))]
pub type FLOAT = f64;

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
        if line == 0 || (line == usize::MAX && position == usize::MAX) {
            panic!("invalid position: ({}, {})", line, position);
        }

        Self {
            line,
            pos: position,
        }
    }

    /// Get the line number (1-based), or `None` if no position or EOF.
    pub fn line(&self) -> Option<usize> {
        if self.is_none() || self.is_eof() {
            None
        } else {
            Some(self.line)
        }
    }

    /// Get the character position (1-based), or `None` if at beginning of a line.
    pub fn position(&self) -> Option<usize> {
        if self.is_none() || self.is_eof() {
            None
        } else if self.pos == 0 {
            None
        } else {
            Some(self.pos)
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

    /// Create a `Position` representing no position.
    pub(crate) fn none() -> Self {
        Self { line: 0, pos: 0 }
    }

    /// Create a `Position` at EOF.
    pub(crate) fn eof() -> Self {
        Self {
            line: usize::MAX,
            pos: usize::MAX,
        }
    }

    /// Is there no `Position`?
    pub fn is_none(&self) -> bool {
        self.line == 0 && self.pos == 0
    }

    /// Is the `Position` at EOF?
    pub fn is_eof(&self) -> bool {
        self.line == usize::MAX && self.pos == usize::MAX
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::new(1, 0)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_eof() {
            write!(f, "EOF")
        } else if self.is_none() {
            write!(f, "none")
        } else {
            write!(f, "line {}, position {}", self.line, self.pos)
        }
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_eof() {
            write!(f, "(EOF)")
        } else {
            write!(f, "({}:{})", self.line, self.pos)
        }
    }
}

/// Compiled AST (abstract syntax tree) of a Rhai script.
#[derive(Debug, Clone)]
pub struct AST(pub(crate) Vec<Stmt>, pub(crate) Vec<Arc<FnDef>>);

#[derive(Debug, Clone)]
pub struct FnDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Stmt,
    pub pos: Position,
}

impl FnDef {
    pub fn compare(&self, name: &str, params_len: usize) -> Ordering {
        match self.name.as_str().cmp(name) {
            Ordering::Equal => self.params.len().cmp(&params_len),
            order => order,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ReturnType {
    Return,
    Exception,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Noop(Position),
    IfElse(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Loop(Box<Stmt>),
    For(String, Box<Expr>, Box<Stmt>),
    Let(String, Option<Box<Expr>>, Position),
    Const(String, Box<Expr>, Position),
    Block(Vec<Stmt>, Position),
    Expr(Box<Expr>),
    Break(Position),
    ReturnWithVal(Option<Box<Expr>>, ReturnType, Position),
}

impl Stmt {
    pub fn is_noop(&self) -> bool {
        matches!(self, Stmt::Noop(_))
    }

    pub fn is_op(&self) -> bool {
        !matches!(self, Stmt::Noop(_))
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Stmt::Let(_, _, _))
    }

    pub fn position(&self) -> Position {
        match self {
            Stmt::Noop(pos)
            | Stmt::Let(_, _, pos)
            | Stmt::Const(_, _, pos)
            | Stmt::Block(_, pos)
            | Stmt::Break(pos)
            | Stmt::ReturnWithVal(_, _, pos) => *pos,
            Stmt::IfElse(expr, _, _) | Stmt::Expr(expr) => expr.position(),
            Stmt::While(_, stmt) | Stmt::Loop(stmt) | Stmt::For(_, _, stmt) => stmt.position(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntegerConstant(INT, Position),
    #[cfg(not(feature = "no_float"))]
    FloatConstant(FLOAT, Position),
    Variable(String, Position),
    Property(String, Position),
    CharConstant(char, Position),
    StringConstant(String, Position),
    Stmt(Box<Stmt>, Position),
    FunctionCall(String, Vec<Expr>, Option<Dynamic>, Position),
    Assignment(Box<Expr>, Box<Expr>, Position),
    Dot(Box<Expr>, Box<Expr>, Position),
    #[cfg(not(feature = "no_index"))]
    Index(Box<Expr>, Box<Expr>, Position),
    #[cfg(not(feature = "no_index"))]
    Array(Vec<Expr>, Position),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    True(Position),
    False(Position),
    Unit(Position),
}

impl Expr {
    pub fn get_constant_value(&self) -> Dynamic {
        match self {
            Expr::IntegerConstant(i, _) => i.into_dynamic(),
            Expr::CharConstant(c, _) => c.into_dynamic(),
            Expr::StringConstant(s, _) => s.into_dynamic(),
            Expr::True(_) => true.into_dynamic(),
            Expr::False(_) => false.into_dynamic(),
            Expr::Unit(_) => ().into_dynamic(),

            #[cfg(not(feature = "no_index"))]
            Expr::Array(items, _) if items.iter().all(Expr::is_constant) => items
                .iter()
                .map(Expr::get_constant_value)
                .collect::<Vec<_>>()
                .into_dynamic(),

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(f, _) => f.into_dynamic(),

            _ => panic!("cannot get value of non-constant expression"),
        }
    }

    pub fn get_constant_str(&self) -> String {
        match self {
            Expr::IntegerConstant(i, _) => i.to_string(),
            Expr::CharConstant(c, _) => c.to_string(),
            Expr::StringConstant(_, _) => "string".to_string(),
            Expr::True(_) => "true".to_string(),
            Expr::False(_) => "false".to_string(),
            Expr::Unit(_) => "()".to_string(),

            #[cfg(not(feature = "no_index"))]
            Expr::Array(items, _) if items.iter().all(Expr::is_constant) => "array".to_string(),

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(f, _) => f.to_string(),

            _ => panic!("cannot get value of non-constant expression"),
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Expr::IntegerConstant(_, pos)
            | Expr::CharConstant(_, pos)
            | Expr::StringConstant(_, pos)
            | Expr::Variable(_, pos)
            | Expr::Property(_, pos)
            | Expr::Stmt(_, pos)
            | Expr::FunctionCall(_, _, _, pos)
            | Expr::True(pos)
            | Expr::False(pos)
            | Expr::Unit(pos) => *pos,

            Expr::Assignment(e, _, _) | Expr::Dot(e, _, _) | Expr::And(e, _) | Expr::Or(e, _) => {
                e.position()
            }

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, pos) => *pos,

            #[cfg(not(feature = "no_index"))]
            Expr::Array(_, pos) => *pos,

            #[cfg(not(feature = "no_index"))]
            Expr::Index(e, _, _) => e.position(),
        }
    }

    /// Is this expression pure?
    ///
    /// A pure expression has no side effects.
    pub fn is_pure(&self) -> bool {
        match self {
            #[cfg(not(feature = "no_index"))]
            Expr::Array(expressions, _) => expressions.iter().all(Expr::is_pure),

            #[cfg(not(feature = "no_index"))]
            Expr::Index(x, y, _) => x.is_pure() && y.is_pure(),

            Expr::And(x, y) | Expr::Or(x, y) => x.is_pure() && y.is_pure(),

            expr => expr.is_constant() || matches!(expr, Expr::Variable(_, _)),
        }
    }

    pub fn is_constant(&self) -> bool {
        match self {
            Expr::IntegerConstant(_, _)
            | Expr::CharConstant(_, _)
            | Expr::StringConstant(_, _)
            | Expr::True(_)
            | Expr::False(_)
            | Expr::Unit(_) => true,

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, _) => true,

            #[cfg(not(feature = "no_index"))]
            Expr::Array(expressions, _) => expressions.iter().all(Expr::is_constant),

            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    IntegerConstant(INT),
    #[cfg(not(feature = "no_float"))]
    FloatConstant(FLOAT),
    Identifier(String),
    CharConstant(char),
    StringConst(String),
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    #[cfg(not(feature = "no_index"))]
    LeftBracket,
    #[cfg(not(feature = "no_index"))]
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
    Const,
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
    #[cfg(not(feature = "no_function"))]
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
            #[cfg(not(feature = "no_float"))]
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
                #[cfg(not(feature = "no_index"))]
                LeftBracket => "[",
                #[cfg(not(feature = "no_index"))]
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
                Const => "const",
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
                #[cfg(not(feature = "no_function"))]
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
            LexError(_)      |
            LeftBrace        | // (+expr) - is unary
            // RightBrace    | {expr} - expr not unary & is closing
            LeftParen        | // {-expr} - is unary
            // RightParen    | (expr) - expr not unary & is closing
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

            #[cfg(not(feature = "no_index"))]
            LeftBracket => true, // [-expr] - is unary
            // RightBracket  | [expr] - expr not unary & is closing

            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_binary_op(&self) -> bool {
        use self::Token::*;

        match *self {
            RightParen | Plus | Minus | Multiply | Divide | Comma | Equals | LessThan
            | GreaterThan | LessThanEqualsTo | GreaterThanEqualsTo | EqualsTo | NotEqualsTo
            | Pipe | Or | Ampersand | And | PowerOf => true,

            #[cfg(not(feature = "no_index"))]
            RightBrace | RightBracket => true,

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
            self.advance();

            match next_char.ok_or((LERR::UnterminatedString, Position::eof()))? {
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
        let mut negated = false;

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
                            #[cfg(not(feature = "no_float"))]
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

                    if negated {
                        result.insert(0, '-');
                    }

                    if let Some(radix) = radix_base {
                        let out: String = result.iter().skip(2).filter(|&&c| c != '_').collect();

                        return Some((
                            INT::from_str_radix(&out, radix)
                                .map(Token::IntegerConstant)
                                .unwrap_or_else(|_| {
                                    Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                                }),
                            pos,
                        ));
                    } else {
                        let out: String = result.iter().filter(|&&c| c != '_').collect();

                        #[cfg(feature = "no_float")]
                        return Some((
                            INT::from_str(&out)
                                .map(Token::IntegerConstant)
                                .unwrap_or_else(|_| {
                                    Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                                }),
                            pos,
                        ));

                        #[cfg(not(feature = "no_float"))]
                        return Some((
                            INT::from_str(&out)
                                .map(Token::IntegerConstant)
                                .or_else(|_| FLOAT::from_str(&out).map(Token::FloatConstant))
                                .unwrap_or_else(|_| {
                                    Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                                }),
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
                            "const" => Token::Const,
                            "if" => Token::If,
                            "else" => Token::Else,
                            "while" => Token::While,
                            "loop" => Token::Loop,
                            "break" => Token::Break,
                            "return" => Token::Return,
                            "throw" => Token::Throw,
                            "for" => Token::For,
                            "in" => Token::In,

                            #[cfg(not(feature = "no_function"))]
                            "fn" => Token::Fn,

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

                #[cfg(not(feature = "no_index"))]
                '[' => return Some((Token::LeftBracket, pos)),
                #[cfg(not(feature = "no_index"))]
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
                '-' => match self.char_stream.peek() {
                    // Negative number?
                    Some('0'..='9') if self.last.is_next_unary() => negated = true,
                    Some('0'..='9') => return Some((Token::Minus, pos)),
                    Some('=') => {
                        self.char_stream.next();
                        self.advance();
                        return Some((Token::MinusAssign, pos));
                    }
                    _ if self.last.is_next_unary() => return Some((Token::UnaryMinus, pos)),
                    _ => return Some((Token::Minus, pos)),
                },
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

fn get_precedence(token: &Token) -> u8 {
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

        Token::Or | Token::XOr | Token::Pipe => 50,

        Token::And | Token::Ampersand => 60,

        Token::LessThan
        | Token::LessThanEqualsTo
        | Token::GreaterThan
        | Token::GreaterThanEqualsTo
        | Token::EqualsTo
        | Token::NotEqualsTo => 70,

        Token::Plus | Token::Minus => 80,

        Token::Divide | Token::Multiply | Token::PowerOf => 90,

        Token::LeftShift | Token::RightShift => 100,

        Token::Modulo => 110,

        Token::Period => 120,

        _ => 0,
    }
}

fn is_bind_right(token: &Token) -> bool {
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
        | Token::PowerOfAssign => true,

        Token::Period => true,

        _ => false,
    }
}

fn parse_paren_expr<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match input.peek() {
        // ()
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
    let mut args_expr_list = Vec::new();

    if let Some(&(Token::RightParen, _)) = input.peek() {
        input.next();
        return Ok(Expr::FunctionCall(id, args_expr_list, None, begin));
    }

    loop {
        args_expr_list.push(parse_expr(input)?);

        match input.peek() {
            Some(&(Token::RightParen, _)) => {
                input.next();
                return Ok(Expr::FunctionCall(id, args_expr_list, None, begin));
            }
            Some(&(Token::Comma, _)) => (),
            Some(&(_, pos)) => {
                return Err(ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the parameters list to function call of '{}'",
                        id
                    )),
                    pos,
                ))
            }
            None => {
                return Err(ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the parameters list to function call of '{}'",
                        id
                    )),
                    Position::eof(),
                ))
            }
        }

        input.next();
    }
}

#[cfg(not(feature = "no_index"))]
fn parse_index_expr<'a>(
    lhs: Box<Expr>,
    input: &mut Peekable<TokenIterator<'a>>,
    pos: Position,
) -> Result<Expr, ParseError> {
    let idx_expr = parse_expr(input)?;

    // Check type of indexing - must be integer
    match &idx_expr {
        Expr::IntegerConstant(i, pos) if *i < 0 => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(format!(
                    "Array access expects non-negative index: {} < 0",
                    i
                )),
                *pos,
            ))
        }
        #[cfg(not(feature = "no_float"))]
        Expr::FloatConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not a float".into()),
                *pos,
            ))
        }
        Expr::CharConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a character".into(),
                ),
                *pos,
            ))
        }
        Expr::StringConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not a string".into()),
                *pos,
            ))
        }
        Expr::Assignment(_, _, pos) | Expr::Unit(pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not ()".into()),
                *pos,
            ))
        }
        Expr::And(lhs, _) | Expr::Or(lhs, _) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a boolean".into(),
                ),
                lhs.position(),
            ))
        }
        Expr::True(pos) | Expr::False(pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a boolean".into(),
                ),
                *pos,
            ))
        }
        _ => (),
    }

    // Check if there is a closing bracket
    match input.peek() {
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
    }
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
        #[cfg(not(feature = "no_index"))]
        Some(&(Token::LeftBracket, pos)) => {
            input.next();
            parse_index_expr(Box::new(Expr::Variable(id, begin)), input, pos)
        }
        Some(_) => Ok(Expr::Variable(id, begin)),
        None => Ok(Expr::Variable(id, Position::eof())),
    }
}

#[cfg(not(feature = "no_index"))]
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
    // Block statement as expression
    match input.peek() {
        Some(&(Token::LeftBrace, pos)) => {
            return parse_block(input).map(|block| Expr::Stmt(Box::new(block), pos))
        }
        _ => (),
    }

    let token = input.next();

    let mut can_be_indexed = false;

    #[allow(unused_mut)]
    let mut root_expr = match token {
        #[cfg(not(feature = "no_float"))]
        Some((Token::FloatConstant(x), pos)) => Ok(Expr::FloatConstant(x, pos)),

        Some((Token::IntegerConstant(x), pos)) => Ok(Expr::IntegerConstant(x, pos)),
        Some((Token::CharConstant(c), pos)) => Ok(Expr::CharConstant(c, pos)),
        Some((Token::StringConst(s), pos)) => {
            can_be_indexed = true;
            Ok(Expr::StringConstant(s, pos))
        }
        Some((Token::Identifier(s), pos)) => {
            can_be_indexed = true;
            parse_ident_expr(s, input, pos)
        }
        Some((Token::LeftParen, pos)) => {
            can_be_indexed = true;
            parse_paren_expr(input, pos)
        }
        #[cfg(not(feature = "no_index"))]
        Some((Token::LeftBracket, pos)) => {
            can_be_indexed = true;
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

    if can_be_indexed {
        // Tail processing all possible indexing
        #[cfg(not(feature = "no_index"))]
        while let Some(&(Token::LeftBracket, pos)) = input.peek() {
            input.next();
            root_expr = parse_index_expr(Box::new(root_expr), input, pos)?;
        }
    }

    Ok(root_expr)
}

fn parse_unary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input.peek() {
        Some(&(Token::UnaryMinus, pos)) => {
            input.next();

            match parse_unary(input) {
                // Negative integer
                Ok(Expr::IntegerConstant(i, _)) => i
                    .checked_neg()
                    .map(|x| Expr::IntegerConstant(x, pos))
                    .or_else(|| {
                        #[cfg(not(feature = "no_float"))]
                        return Some(Expr::FloatConstant(-(i as FLOAT), pos));

                        #[cfg(feature = "no_float")]
                        return None;
                    })
                    .ok_or_else(|| {
                        ParseError::new(
                            PERR::BadInput(LERR::MalformedNumber(format!("-{}", i)).to_string()),
                            pos,
                        )
                    }),

                // Negative float
                #[cfg(not(feature = "no_float"))]
                Ok(Expr::FloatConstant(f, pos)) => Ok(Expr::FloatConstant(-f, pos)),

                // Call negative function
                Ok(expr) => Ok(Expr::FunctionCall("-".into(), vec![expr], None, pos)),

                err @ Err(_) => err,
            }
        }
        Some(&(Token::UnaryPlus, _)) => {
            input.next();
            parse_unary(input)
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
    fn valid_assignment_chain(expr: &Expr, is_top: bool) -> Option<ParseError> {
        match expr {
            Expr::Variable(_, _) => {
                assert!(is_top, "property expected but gets variable");
                None
            }
            Expr::Property(_, _) => {
                assert!(!is_top, "variable expected but gets property");
                None
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, _) if matches!(idx_lhs.as_ref(), &Expr::Variable(_, _)) => {
                assert!(is_top, "property expected but gets variable");
                None
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, _) if matches!(idx_lhs.as_ref(), &Expr::Property(_, _)) => {
                assert!(!is_top, "variable expected but gets property");
                None
            }

            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, pos) => Some(ParseError::new(
                match idx_lhs.as_ref() {
                    Expr::Index(_, _, _) => ParseErrorType::AssignmentToCopy,
                    _ => ParseErrorType::AssignmentToInvalidLHS,
                },
                *pos,
            )),

            Expr::Dot(dot_lhs, dot_rhs, _) => match dot_lhs.as_ref() {
                Expr::Variable(_, _) if is_top => valid_assignment_chain(dot_rhs, false),
                Expr::Property(_, _) if !is_top => valid_assignment_chain(dot_rhs, false),

                #[cfg(not(feature = "no_index"))]
                Expr::Index(idx_lhs, _, _)
                    if matches!(idx_lhs.as_ref(), &Expr::Variable(_, _)) && is_top =>
                {
                    valid_assignment_chain(dot_rhs, false)
                }
                #[cfg(not(feature = "no_index"))]
                Expr::Index(idx_lhs, _, _)
                    if matches!(idx_lhs.as_ref(), &Expr::Property(_, _)) && !is_top =>
                {
                    valid_assignment_chain(dot_rhs, false)
                }
                #[cfg(not(feature = "no_index"))]
                Expr::Index(idx_lhs, _, _) => Some(ParseError::new(
                    ParseErrorType::AssignmentToCopy,
                    idx_lhs.position(),
                )),

                expr => panic!("unexpected dot expression {:#?}", expr),
            },

            _ => Some(ParseError::new(
                ParseErrorType::AssignmentToInvalidLHS,
                expr.position(),
            )),
        }
    }

    //println!("{:#?} = {:#?}", lhs, rhs);

    match valid_assignment_chain(&lhs, true) {
        None => Ok(Expr::Assignment(Box::new(lhs), Box::new(rhs), pos)),
        Some(err) => Err(err),
    }
}

fn parse_op_assignment(
    function: &str,
    lhs: Expr,
    rhs: Expr,
    pos: Position,
) -> Result<Expr, ParseError> {
    let lhs_copy = lhs.clone();

    parse_assignment(
        lhs,
        Expr::FunctionCall(function.into(), vec![lhs_copy, rhs], None, pos),
        pos,
    )
}

fn parse_binary_op<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    parent_precedence: u8,
    lhs: Expr,
) -> Result<Expr, ParseError> {
    let mut current_lhs = lhs;

    loop {
        let (current_precedence, bind_right) = if let Some(&(ref current_op, _)) = input.peek() {
            (get_precedence(current_op), is_bind_right(current_op))
        } else {
            (0, false)
        };

        // Bind left to the parent lhs expression if precedence is higher
        // If same precedence, then check if the operator binds right
        if current_precedence < parent_precedence
            || (current_precedence == parent_precedence && !bind_right)
        {
            return Ok(current_lhs);
        }

        if let Some((op_token, pos)) = input.next() {
            input.peek();

            let rhs = parse_unary(input)?;

            let next_precedence = if let Some(&(ref next_op, _)) = input.peek() {
                get_precedence(next_op)
            } else {
                0
            };

            // Bind to right if the next operator has higher precedence
            // If same precedence, then check if the operator binds right
            let rhs = if (current_precedence == next_precedence && bind_right)
                || current_precedence < next_precedence
            {
                parse_binary_op(input, current_precedence, rhs)?
            } else {
                // Otherwise bind to left (even if next operator has the same precedence)
                rhs
            };

            current_lhs = match op_token {
                Token::Plus => Expr::FunctionCall("+".into(), vec![current_lhs, rhs], None, pos),
                Token::Minus => Expr::FunctionCall("-".into(), vec![current_lhs, rhs], None, pos),
                Token::Multiply => {
                    Expr::FunctionCall("*".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::Divide => Expr::FunctionCall("/".into(), vec![current_lhs, rhs], None, pos),

                Token::Equals => parse_assignment(current_lhs, rhs, pos)?,
                Token::PlusAssign => parse_op_assignment("+", current_lhs, rhs, pos)?,
                Token::MinusAssign => parse_op_assignment("-", current_lhs, rhs, pos)?,

                Token::Period => {
                    fn change_var_to_property(expr: Expr) -> Expr {
                        match expr {
                            Expr::Dot(lhs, rhs, pos) => Expr::Dot(
                                Box::new(change_var_to_property(*lhs)),
                                Box::new(change_var_to_property(*rhs)),
                                pos,
                            ),
                            #[cfg(not(feature = "no_index"))]
                            Expr::Index(lhs, idx, pos) => {
                                Expr::Index(Box::new(change_var_to_property(*lhs)), idx, pos)
                            }
                            Expr::Variable(s, pos) => Expr::Property(s, pos),
                            expr => expr,
                        }
                    }

                    Expr::Dot(
                        Box::new(current_lhs),
                        Box::new(change_var_to_property(rhs)),
                        pos,
                    )
                }

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
                Token::OrAssign => parse_op_assignment("|", current_lhs, rhs, pos)?,
                Token::AndAssign => parse_op_assignment("&", current_lhs, rhs, pos)?,
                Token::XOrAssign => parse_op_assignment("^", current_lhs, rhs, pos)?,
                Token::MultiplyAssign => parse_op_assignment("*", current_lhs, rhs, pos)?,
                Token::DivideAssign => parse_op_assignment("/", current_lhs, rhs, pos)?,
                Token::Pipe => Expr::FunctionCall("|".into(), vec![current_lhs, rhs], None, pos),
                Token::LeftShift => {
                    Expr::FunctionCall("<<".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::RightShift => {
                    Expr::FunctionCall(">>".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::LeftShiftAssign => parse_op_assignment("<<", current_lhs, rhs, pos)?,
                Token::RightShiftAssign => parse_op_assignment(">>", current_lhs, rhs, pos)?,
                Token::Ampersand => {
                    Expr::FunctionCall("&".into(), vec![current_lhs, rhs], None, pos)
                }
                Token::Modulo => Expr::FunctionCall("%".into(), vec![current_lhs, rhs], None, pos),
                Token::ModuloAssign => parse_op_assignment("%", current_lhs, rhs, pos)?,
                Token::PowerOf => Expr::FunctionCall("~".into(), vec![current_lhs, rhs], None, pos),
                Token::PowerOfAssign => parse_op_assignment("~", current_lhs, rhs, pos)?,
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
    parse_binary_op(input, 1, lhs)
}

fn parse_if<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    input.next();

    let guard = parse_expr(input)?;
    let body = parse_block(input)?;

    match input.peek() {
        Some(&(Token::Else, _)) => {
            input.next();

            let else_body = if matches!(input.peek(), Some(&(Token::If, _))) {
                parse_if(input)?
            } else {
                parse_block(input)?
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

fn parse_var<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    var_type: VariableType,
) -> Result<Stmt, ParseError> {
    let pos = match input.next() {
        Some((_, tok_pos)) => tok_pos,
        _ => return Err(ParseError::new(PERR::InputPastEndOfFile, Position::eof())),
    };

    let name = match input.next() {
        Some((Token::Identifier(s), _)) => s,
        Some((_, pos)) => return Err(ParseError::new(PERR::VarExpectsIdentifier, pos)),
        None => return Err(ParseError::new(PERR::VarExpectsIdentifier, Position::eof())),
    };

    if matches!(input.peek(), Some(&(Token::Equals, _))) {
        input.next();
        let init_value = parse_expr(input)?;

        match var_type {
            VariableType::Normal => Ok(Stmt::Let(name, Some(Box::new(init_value)), pos)),

            VariableType::Constant if init_value.is_constant() => {
                Ok(Stmt::Const(name, Box::new(init_value), pos))
            }
            // Constants require a constant expression
            VariableType::Constant => Err(ParseError(
                PERR::ForbiddenConstantExpr(name.to_string()),
                init_value.position(),
            )),
        }
    } else {
        Ok(Stmt::Let(name, None, pos))
    }
}

fn parse_block<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    let pos = match input.next() {
        Some((Token::LeftBrace, pos)) => pos,
        Some((_, pos)) => return Err(ParseError::new(PERR::MissingLeftBrace, pos)),
        None => return Err(ParseError::new(PERR::MissingLeftBrace, Position::eof())),
    };

    let mut statements = Vec::new();

    match input.peek() {
        Some(&(Token::RightBrace, _)) => (), // empty block

        #[cfg(not(feature = "no_function"))]
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
            Ok(Stmt::Block(statements, pos))
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
            let return_type = match token {
                Token::Return => ReturnType::Return,
                Token::Throw => ReturnType::Exception,
                _ => panic!("token should be return or throw"),
            };

            input.next();

            match input.peek() {
                // return; or throw;
                Some(&(Token::SemiColon, pos)) => Ok(Stmt::ReturnWithVal(None, return_type, pos)),
                // Just a return/throw without anything at the end of script
                None => Ok(Stmt::ReturnWithVal(None, return_type, Position::eof())),
                // return or throw with expression
                Some(&(_, pos)) => {
                    let ret = parse_expr(input)?;
                    Ok(Stmt::ReturnWithVal(Some(Box::new(ret)), return_type, pos))
                }
            }
        }
        Some(&(Token::LeftBrace, _)) => parse_block(input),
        Some(&(Token::Let, _)) => parse_var(input, VariableType::Normal),
        Some(&(Token::Const, _)) => parse_var(input, VariableType::Constant),
        _ => parse_expr_stmt(input),
    }
}

#[cfg(not(feature = "no_function"))]
fn parse_fn<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<FnDef, ParseError> {
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

    if matches!(input.peek(), Some(&(Token::RightParen, _))) {
        input.next();
    } else {
        loop {
            match input.next() {
                Some((Token::RightParen, _)) => break,
                Some((Token::Comma, _)) => (),
                Some((Token::Identifier(s), _)) => {
                    params.push(s.into());
                }
                Some((_, pos)) => {
                    return Err(ParseError::new(
                        PERR::MalformedCallExpr(
                            "Function call arguments missing either a ',' or a ')'".into(),
                        ),
                        pos,
                    ))
                }
                None => {
                    return Err(ParseError::new(
                        PERR::MalformedCallExpr(
                            "Function call arguments missing a closing ')'".into(),
                        ),
                        Position::eof(),
                    ))
                }
            }
        }
    }

    let body = parse_block(input)?;

    Ok(FnDef {
        name,
        params,
        body,
        pos,
    })
}

fn parse_top_level<'a, 'e>(
    input: &mut Peekable<TokenIterator<'a>>,
) -> Result<(Vec<Stmt>, Vec<FnDef>), ParseError> {
    let mut statements = Vec::<Stmt>::new();
    let mut functions = Vec::<FnDef>::new();

    while input.peek().is_some() {
        match input.peek() {
            #[cfg(not(feature = "no_function"))]
            Some(&(Token::Fn, _)) => {
                let f = parse_fn(input)?;

                // Ensure list is sorted
                match functions.binary_search_by(|fn_def| fn_def.compare(&f.name, f.params.len())) {
                    Ok(n) => functions[n] = f,        // Override previous definition
                    Err(n) => functions.insert(n, f), // New function definition
                }
            }
            _ => statements.push(parse_stmt(input)?),
        }

        // Notice semicolons are optional
        if let Some(&(Token::SemiColon, _)) = input.peek() {
            input.next();
        }
    }

    Ok((statements, functions))
}

pub fn parse<'a, 'e>(
    input: &mut Peekable<TokenIterator<'a>>,
    engine: &Engine<'e>,
    scope: &Scope,
) -> Result<AST, ParseError> {
    let (statements, functions) = parse_top_level(input)?;

    Ok(
        #[cfg(not(feature = "no_optimize"))]
        optimize_ast(engine, scope, statements, functions),
        #[cfg(feature = "no_optimize")]
        AST(statements, functions.into_iter().map(Arc::new).collect()),
    )
}

pub fn map_dynamic_to_expr(value: Dynamic, pos: Position) -> (Option<Expr>, Dynamic) {
    if value.is::<INT>() {
        let value2 = value.clone();
        (
            Some(Expr::IntegerConstant(
                *value.downcast::<INT>().expect("value should be INT"),
                pos,
            )),
            value2,
        )
    } else if value.is::<char>() {
        let value2 = value.clone();
        (
            Some(Expr::CharConstant(
                *value.downcast::<char>().expect("value should be char"),
                pos,
            )),
            value2,
        )
    } else if value.is::<String>() {
        let value2 = value.clone();
        (
            Some(Expr::StringConstant(
                *value.downcast::<String>().expect("value should be String"),
                pos,
            )),
            value2,
        )
    } else if value.is::<bool>() {
        let value2 = value.clone();
        (
            Some(
                if *value.downcast::<bool>().expect("value should be bool") {
                    Expr::True(pos)
                } else {
                    Expr::False(pos)
                },
            ),
            value2,
        )
    } else {
        #[cfg(not(feature = "no_float"))]
        {
            if value.is::<FLOAT>() {
                let value2 = value.clone();
                return (
                    Some(Expr::FloatConstant(
                        *value.downcast::<FLOAT>().expect("value should be FLOAT"),
                        pos,
                    )),
                    value2,
                );
            }
        }

        (None, value)
    }
}
