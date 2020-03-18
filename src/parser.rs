//! Main module defining the lexer and parser.

use crate::any::{Any, AnyExt, Dynamic};
use crate::engine::Engine;
use crate::error::{LexError, ParseError, ParseErrorType};
use crate::scope::{Scope, VariableType};

#[cfg(not(feature = "no_optimize"))]
use crate::optimize::optimize_into_ast;

use crate::stdlib::{
    borrow::Cow,
    boxed::Box,
    char,
    cmp::Ordering,
    fmt, format,
    iter::Peekable,
    str::Chars,
    str::FromStr,
    string::{String, ToString},
    sync::Arc,
    usize, vec,
    vec::Vec,
};

/// The system integer type.
///
/// If the `only_i32` feature is enabled, this will be `i32` instead.
#[cfg(not(feature = "only_i32"))]
pub type INT = i64;

/// The system integer type.
///
/// If the `only_i32` feature is not enabled, this will be `i64` instead.
#[cfg(feature = "only_i32")]
pub type INT = i32;

/// The system floating-point type.
#[cfg(not(feature = "no_float"))]
pub type FLOAT = f64;

type LERR = LexError;
type PERR = ParseErrorType;

/// A location (line number + character position) in the input script.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub struct Position {
    /// Line number - 0 = none, MAX = EOF
    line: usize,
    /// Character position - 0 = BOL, MAX = EOF
    pos: usize,
}

impl Position {
    /// Create a new `Position`.
    pub fn new(line: usize, position: usize) -> Self {
        assert!(line != 0, "line cannot be zero");
        assert!(
            line != usize::MAX || position != usize::MAX,
            "invalid position"
        );

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

/// A script-function definition.
#[derive(Debug, Clone)]
pub struct FnDef {
    /// Function name.
    pub name: String,
    /// Names of function parameters.
    pub params: Vec<String>,
    /// Function body.
    pub body: Stmt,
    /// Position of the function definition.
    pub pos: Position,
}

impl FnDef {
    /// Function to order two FnDef records, for binary search.
    pub fn compare(&self, name: &str, params_len: usize) -> Ordering {
        // First order by name
        match self.name.as_str().cmp(name) {
            // Then by number of parameters
            Ordering::Equal => self.params.len().cmp(&params_len),
            order => order,
        }
    }
}

/// `return`/`throw` statement.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum ReturnType {
    /// `return` statement.
    Return,
    /// `throw` statement.
    Exception,
}

/// A statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// No-op.
    Noop(Position),
    /// if expr { stmt } else { stmt }
    IfElse(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// while expr { stmt }
    While(Box<Expr>, Box<Stmt>),
    /// loop { stmt }
    Loop(Box<Stmt>),
    /// for id in expr { stmt }
    For(String, Box<Expr>, Box<Stmt>),
    /// let id = expr
    Let(String, Option<Box<Expr>>, Position),
    /// const id = expr
    Const(String, Box<Expr>, Position),
    /// { stmt; ... }
    Block(Vec<Stmt>, Position),
    /// { stmt }
    Expr(Box<Expr>),
    /// break
    Break(Position),
    /// `return`/`throw`
    ReturnWithVal(Option<Box<Expr>>, ReturnType, Position),
}

impl Stmt {
    /// Get the `Position` of this statement.
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

    /// Is this statement self-terminated (i.e. no need for a semicolon terminator)?
    pub fn is_self_terminated(&self) -> bool {
        match self {
            Stmt::IfElse(_, _, _)
            | Stmt::While(_, _)
            | Stmt::Loop(_)
            | Stmt::For(_, _, _)
            | Stmt::Block(_, _) => true,

            // A No-op requires a semicolon in order to know it is an empty statement!
            Stmt::Noop(_) => false,

            Stmt::Let(_, _, _)
            | Stmt::Const(_, _, _)
            | Stmt::Expr(_)
            | Stmt::Break(_)
            | Stmt::ReturnWithVal(_, _, _) => false,
        }
    }

    /// Is this statement _pure_?
    pub fn is_pure(&self) -> bool {
        match self {
            Stmt::Noop(_) => true,
            Stmt::Expr(expr) => expr.is_pure(),
            Stmt::IfElse(guard, if_block, Some(else_block)) => {
                guard.is_pure() && if_block.is_pure() && else_block.is_pure()
            }
            Stmt::IfElse(guard, block, None) | Stmt::While(guard, block) => {
                guard.is_pure() && block.is_pure()
            }
            Stmt::Loop(block) => block.is_pure(),
            Stmt::For(_, range, block) => range.is_pure() && block.is_pure(),
            Stmt::Let(_, _, _) | Stmt::Const(_, _, _) => false,
            Stmt::Block(statements, _) => statements.iter().all(Stmt::is_pure),
            Stmt::Break(_) | Stmt::ReturnWithVal(_, _, _) => false,
        }
    }
}

/// An expression.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer constant.
    IntegerConstant(INT, Position),
    /// Floating-point constant.
    #[cfg(not(feature = "no_float"))]
    FloatConstant(FLOAT, Position),
    /// Character constant.
    CharConstant(char, Position),
    /// String constant.
    StringConstant(String, Position),
    /// Variable access.
    Variable(String, Position),
    /// Property access.
    Property(String, Position),
    /// { stmt }
    Stmt(Box<Stmt>, Position),
    /// func(expr, ... )
    FunctionCall(String, Vec<Expr>, Option<Dynamic>, Position),
    /// expr = expr
    Assignment(Box<Expr>, Box<Expr>, Position),
    /// lhs.rhs
    Dot(Box<Expr>, Box<Expr>, Position),
    /// expr[expr]
    #[cfg(not(feature = "no_index"))]
    Index(Box<Expr>, Box<Expr>, Position),
    #[cfg(not(feature = "no_index"))]
    /// [ expr, ... ]
    Array(Vec<Expr>, Position),
    /// lhs && rhs
    And(Box<Expr>, Box<Expr>),
    /// lhs || rhs
    Or(Box<Expr>, Box<Expr>),
    /// true
    True(Position),
    /// false
    False(Position),
    /// ()
    Unit(Position),
}

impl Expr {
    /// Get the `Dynamic` value of a constant expression.
    ///
    /// # Panics
    ///
    /// Panics when the expression is not constant.
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

    /// Get the display value of a constant expression.
    ///
    /// # Panics
    ///
    /// Panics when the expression is not constant.
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

    /// Get the `Position` of the expression.
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

            Expr::Assignment(expr, _, _)
            | Expr::Dot(expr, _, _)
            | Expr::And(expr, _)
            | Expr::Or(expr, _) => expr.position(),

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, pos) => *pos,

            #[cfg(not(feature = "no_index"))]
            Expr::Array(_, pos) => *pos,

            #[cfg(not(feature = "no_index"))]
            Expr::Index(expr, _, _) => expr.position(),
        }
    }

    /// Is the expression pure?
    ///
    /// A pure expression has no side effects.
    pub fn is_pure(&self) -> bool {
        match self {
            #[cfg(not(feature = "no_index"))]
            Expr::Array(expressions, _) => expressions.iter().all(Expr::is_pure),

            #[cfg(not(feature = "no_index"))]
            Expr::Index(x, y, _) => x.is_pure() && y.is_pure(),

            Expr::And(x, y) | Expr::Or(x, y) => x.is_pure() && y.is_pure(),

            Expr::Stmt(stmt, _) => stmt.is_pure(),

            expr => expr.is_constant() || matches!(expr, Expr::Variable(_, _)),
        }
    }

    /// Is the expression a constant?
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

            // An array literal is constant if all items are constant
            #[cfg(not(feature = "no_index"))]
            Expr::Array(expressions, _) => expressions.iter().all(Expr::is_constant),

            _ => false,
        }
    }
}

/// Tokens.
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
    /// Get the syntax of the token.
    pub fn syntax<'a>(&'a self) -> Cow<'a, str> {
        use self::Token::*;

        match self {
            IntegerConstant(i) => i.to_string().into(),
            #[cfg(not(feature = "no_float"))]
            FloatConstant(f) => f.to_string().into(),
            Identifier(s) => s.into(),
            CharConstant(c) => c.to_string().into(),
            LexError(err) => err.to_string().into(),

            token => (match token {
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

    // If another operator is after these, it's probably an unary operator
    // (not sure about fn name).
    pub fn is_next_unary(&self) -> bool {
        use self::Token::*;

        match self {
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

    /// Get the precedence number of the token.
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Equals
            | Self::PlusAssign
            | Self::MinusAssign
            | Self::MultiplyAssign
            | Self::DivideAssign
            | Self::LeftShiftAssign
            | Self::RightShiftAssign
            | Self::AndAssign
            | Self::OrAssign
            | Self::XOrAssign
            | Self::ModuloAssign
            | Self::PowerOfAssign => 10,

            Self::Or | Self::XOr | Self::Pipe => 50,

            Self::And | Self::Ampersand => 60,

            Self::LessThan
            | Self::LessThanEqualsTo
            | Self::GreaterThan
            | Self::GreaterThanEqualsTo
            | Self::EqualsTo
            | Self::NotEqualsTo => 70,

            Self::Plus | Self::Minus => 80,

            Self::Divide | Self::Multiply | Self::PowerOf => 90,

            Self::LeftShift | Self::RightShift => 100,

            Self::Modulo => 110,

            Self::Period => 120,

            _ => 0,
        }
    }

    /// Does an expression bind to the right (instead of left)?
    pub fn is_bind_right(&self) -> bool {
        match self {
            // Assignments bind to the right
            Self::Equals
            | Self::PlusAssign
            | Self::MinusAssign
            | Self::MultiplyAssign
            | Self::DivideAssign
            | Self::LeftShiftAssign
            | Self::RightShiftAssign
            | Self::AndAssign
            | Self::OrAssign
            | Self::XOrAssign
            | Self::ModuloAssign
            | Self::PowerOfAssign => true,

            // Property access binds to the right
            Self::Period => true,

            _ => false,
        }
    }
}

/// An iterator on a `Token` stream.
pub struct TokenIterator<'a> {
    /// The last token seen.
    last: Token,
    /// Current position.
    pos: Position,
    /// The input characters stream.
    char_stream: Peekable<Chars<'a>>,
}

impl<'a> TokenIterator<'a> {
    /// Move the current position one character ahead.
    fn advance(&mut self) {
        self.pos.advance();
    }
    /// Move the current position back one character.
    ///
    /// # Panics
    ///
    /// Panics if already at the beginning of a line - cannot rewind to the previous line.
    fn rewind(&mut self) {
        self.pos.rewind();
    }
    /// Move the current position to the next line.
    fn new_line(&mut self) {
        self.pos.new_line()
    }

    /// Parse a string literal wrapped by `enclosing_char`.
    pub fn parse_string_literal(
        &mut self,
        enclosing_char: char,
    ) -> Result<String, (LexError, Position)> {
        let mut result = Vec::new();
        let mut escape = String::with_capacity(12);

        loop {
            let next_char = self.char_stream.next();
            self.advance();

            match next_char.ok_or((LERR::UnterminatedString, Position::eof()))? {
                // \...
                '\\' if escape.is_empty() => {
                    escape.push('\\');
                }
                // \\
                '\\' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\\');
                }
                // \t
                't' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\t');
                }
                // \n
                'n' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\n');
                }
                // \r
                'r' if !escape.is_empty() => {
                    escape.clear();
                    result.push('\r');
                }
                // \x??, \u????, \U????????
                ch @ 'x' | ch @ 'u' | ch @ 'U' if !escape.is_empty() => {
                    let mut seq = escape.clone();
                    seq.push(ch);
                    escape.clear();

                    let mut out_val: u32 = 0;
                    let len = match ch {
                        'x' => 2,
                        'u' => 4,
                        'U' => 8,
                        _ => panic!("should be 'x', 'u' or 'U'"),
                    };

                    for _ in 0..len {
                        let c = self.char_stream.next().ok_or_else(|| {
                            (LERR::MalformedEscapeSequence(seq.to_string()), self.pos)
                        })?;

                        seq.push(c);
                        self.advance();

                        out_val *= 16;
                        out_val += c.to_digit(16).ok_or_else(|| {
                            (LERR::MalformedEscapeSequence(seq.to_string()), self.pos)
                        })?;
                    }

                    result.push(
                        char::from_u32(out_val)
                            .ok_or_else(|| (LERR::MalformedEscapeSequence(seq), self.pos))?,
                    );
                }

                // \{enclosing_char} - escaped
                ch if enclosing_char == ch && !escape.is_empty() => result.push(ch),

                // Close wrapper
                ch if enclosing_char == ch && escape.is_empty() => break,

                // Unknown escape sequence
                _ if !escape.is_empty() => {
                    return Err((LERR::MalformedEscapeSequence(escape), self.pos))
                }

                // Cannot have new-lines inside string literals
                '\n' => {
                    self.rewind();
                    return Err((LERR::UnterminatedString, self.pos));
                }

                // All other characters
                ch => {
                    escape.clear();
                    result.push(ch);
                }
            }
        }

        Ok(result.iter().collect())
    }

    /// Get the next token.
    fn inner_next(&mut self) -> Option<(Token, Position)> {
        let mut negated = false;

        while let Some(c) = self.char_stream.next() {
            self.advance();

            let pos = self.pos;

            match c {
                // \n
                '\n' => self.new_line(),
                // digit ...
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
                            // 0x????, 0o????, 0b????
                            ch @ 'x' | ch @ 'X' | ch @ 'o' | ch @ 'O' | ch @ 'b' | ch @ 'B'
                                if c == '0' =>
                            {
                                result.push(next_char);
                                self.char_stream.next();
                                self.advance();

                                let valid = match ch {
                                    'x' | 'X' => [
                                        'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F',
                                        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_',
                                    ],
                                    'o' | 'O' => [
                                        '0', '1', '2', '3', '4', '5', '6', '7', '_', '_', '_', '_',
                                        '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_',
                                    ],
                                    'b' | 'B' => [
                                        '0', '1', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_',
                                        '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_',
                                    ],
                                    _ => panic!("unexpected character {}", ch),
                                };

                                radix_base = Some(match ch {
                                    'x' | 'X' => 16,
                                    'o' | 'O' => 8,
                                    'b' | 'B' => 2,
                                    _ => panic!("unexpected character {}", ch),
                                });

                                while let Some(&next_char_in_hex) = self.char_stream.peek() {
                                    if !valid.contains(&next_char_in_hex) {
                                        break;
                                    }

                                    result.push(next_char_in_hex);
                                    self.char_stream.next();
                                    self.advance();
                                }
                            }

                            _ => break,
                        }
                    }

                    if negated {
                        result.insert(0, '-');
                    }

                    // Parse number
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
                        let num = INT::from_str(&out).map(Token::IntegerConstant);

                        // If integer parsing is unnecessary, try float instead
                        #[cfg(not(feature = "no_float"))]
                        let num = num.or_else(|_| FLOAT::from_str(&out).map(Token::FloatConstant));

                        return Some((
                            num.unwrap_or_else(|_| {
                                Token::LexError(LERR::MalformedNumber(result.iter().collect()))
                            }),
                            pos,
                        ));
                    }
                }
                // letter ...
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

                    let has_letter = result.iter().any(char::is_ascii_alphabetic);
                    let identifier: String = result.iter().collect();

                    return Some((
                        match identifier.as_str() {
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

                            _ if has_letter => Token::Identifier(identifier),

                            _ => Token::LexError(LERR::MalformedIdentifier(identifier)),
                        },
                        pos,
                    ));
                }
                // " - string literal
                '"' => {
                    return match self.parse_string_literal('"') {
                        Ok(out) => Some((Token::StringConst(out), pos)),
                        Err(e) => Some((Token::LexError(e.0), e.1)),
                    }
                }
                // ' - character literal
                '\'' => match self.parse_string_literal('\'') {
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

                // Braces
                '{' => return Some((Token::LeftBrace, pos)),
                '}' => return Some((Token::RightBrace, pos)),

                // Parentheses
                '(' => return Some((Token::LeftParen, pos)),
                ')' => return Some((Token::RightParen, pos)),

                // Indexing
                #[cfg(not(feature = "no_index"))]
                '[' => return Some((Token::LeftBracket, pos)),
                #[cfg(not(feature = "no_index"))]
                ']' => return Some((Token::RightBracket, pos)),

                // Operators
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

/// Tokenize an input text stream.
pub fn lex(input: &str) -> TokenIterator<'_> {
    TokenIterator {
        last: Token::LexError(LERR::InputError("".into())),
        pos: Position::new(1, 0),
        char_stream: input.chars().peekable(),
    }
}

/// Parse ( expr )
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
        // ( xxx )
        Some((Token::RightParen, _)) => Ok(expr),
        // ( xxx ???
        Some((_, pos)) => {
            return Err(ParseError::new(
                PERR::MissingRightParen("a matching ( in the expression".into()),
                pos,
            ))
        }
        // ( xxx
        None => Err(ParseError::new(
            PERR::MissingRightParen("a matching ( in the expression".into()),
            Position::eof(),
        )),
    }
}

/// Parse a function call.
fn parse_call_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    let mut args_expr_list = Vec::new();

    // id()
    if let (Token::RightParen, _) = input.peek().ok_or_else(|| {
        ParseError::new(
            PERR::MissingRightParen(format!(
                "closing the arguments to call of function '{}'",
                id
            )),
            Position::eof(),
        )
    })? {
        input.next();
        return Ok(Expr::FunctionCall(id, args_expr_list, None, begin));
    }

    loop {
        args_expr_list.push(parse_expr(input)?);

        match input.peek().ok_or_else(|| {
            ParseError::new(
                PERR::MissingRightParen(format!(
                    "closing the arguments to call of function '{}'",
                    id
                )),
                Position::eof(),
            )
        })? {
            (Token::RightParen, _) => {
                input.next();
                return Ok(Expr::FunctionCall(id, args_expr_list, None, begin));
            }
            (Token::Comma, _) => (),
            (_, pos) => {
                return Err(ParseError::new(
                    PERR::MissingComma(format!(
                        "separating the arguments to call of function '{}'",
                        id
                    )),
                    *pos,
                ))
            }
        }

        input.next();
    }
}

/// Parse an indexing expression.s
#[cfg(not(feature = "no_index"))]
fn parse_index_expr<'a>(
    lhs: Box<Expr>,
    input: &mut Peekable<TokenIterator<'a>>,
    pos: Position,
) -> Result<Expr, ParseError> {
    let idx_expr = parse_expr(input)?;

    // Check type of indexing - must be integer
    match &idx_expr {
        // lhs[int]
        Expr::IntegerConstant(i, pos) if *i < 0 => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(format!(
                    "Array access expects non-negative index: {} < 0",
                    i
                )),
                *pos,
            ))
        }
        // lhs[float]
        #[cfg(not(feature = "no_float"))]
        Expr::FloatConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not a float".into()),
                *pos,
            ))
        }
        // lhs[char]
        Expr::CharConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a character".into(),
                ),
                *pos,
            ))
        }
        // lhs[string]
        Expr::StringConstant(_, pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not a string".into()),
                *pos,
            ))
        }
        // lhs[??? = ??? ], lhs[()]
        Expr::Assignment(_, _, pos) | Expr::Unit(pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr("Array access expects integer index, not ()".into()),
                *pos,
            ))
        }
        // lhs[??? && ???], lhs[??? || ???]
        Expr::And(lhs, _) | Expr::Or(lhs, _) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a boolean".into(),
                ),
                lhs.position(),
            ))
        }
        // lhs[true], lhs[false]
        Expr::True(pos) | Expr::False(pos) => {
            return Err(ParseError::new(
                PERR::MalformedIndexExpr(
                    "Array access expects integer index, not a boolean".into(),
                ),
                *pos,
            ))
        }
        // All other expressions
        _ => (),
    }

    // Check if there is a closing bracket
    match input.peek().ok_or_else(|| {
        ParseError::new(
            PERR::MissingRightBracket("index expression".into()),
            Position::eof(),
        )
    })? {
        (Token::RightBracket, _) => {
            input.next();
            return Ok(Expr::Index(lhs, Box::new(idx_expr), pos));
        }
        (_, pos) => {
            return Err(ParseError::new(
                PERR::MissingRightBracket("index expression".into()),
                *pos,
            ))
        }
    }
}

/// Parse an expression that begins with an identifier.
fn parse_ident_expr<'a>(
    id: String,
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    match input.peek() {
        // id(...) - function call
        Some((Token::LeftParen, _)) => {
            input.next();
            parse_call_expr(id, input, begin)
        }
        // id[...] - indexing
        #[cfg(not(feature = "no_index"))]
        Some((Token::LeftBracket, pos)) => {
            let pos = *pos;
            input.next();
            parse_index_expr(Box::new(Expr::Variable(id, begin)), input, pos)
        }
        // id - variable
        Some(_) => Ok(Expr::Variable(id, begin)),
        // EOF
        None => Ok(Expr::Variable(id, Position::eof())),
    }
}

/// Parse an array literal.
#[cfg(not(feature = "no_index"))]
fn parse_array_literal<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    begin: Position,
) -> Result<Expr, ParseError> {
    let mut arr = Vec::new();

    if !matches!(input.peek(), Some((Token::RightBracket, _))) {
        while input.peek().is_some() {
            arr.push(parse_expr(input)?);

            match input.peek().ok_or_else(|| {
                ParseError(
                    PERR::MissingRightBracket("separating items in array literal".into()),
                    Position::eof(),
                )
            })? {
                (Token::Comma, _) => {
                    input.next();
                }
                (Token::RightBracket, _) => break,
                (_, pos) => {
                    return Err(ParseError(
                        PERR::MissingComma("separating items in array literal".into()),
                        *pos,
                    ))
                }
            }
        }
    }

    match input.peek().ok_or_else(|| {
        ParseError::new(
            PERR::MissingRightBracket("the end of array literal".into()),
            Position::eof(),
        )
    })? {
        (Token::RightBracket, _) => {
            input.next();
            Ok(Expr::Array(arr, begin))
        }
        (_, pos) => Err(ParseError::new(
            PERR::MissingRightBracket("the end of array literal".into()),
            *pos,
        )),
    }
}

/// Parse a primary expression.
fn parse_primary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    // { - block statement as expression
    match input.peek() {
        Some((Token::LeftBrace, pos)) => {
            let pos = *pos;
            return parse_block(input, false).map(|block| Expr::Stmt(Box::new(block), pos));
        }
        _ => (),
    }

    let token = input.next();

    let mut can_be_indexed = false;

    #[allow(unused_mut)]
    let mut root_expr = match token
        .ok_or_else(|| ParseError::new(PERR::InputPastEndOfFile, Position::eof()))?
    {
        #[cfg(not(feature = "no_float"))]
        (Token::FloatConstant(x), pos) => Ok(Expr::FloatConstant(x, pos)),

        (Token::IntegerConstant(x), pos) => Ok(Expr::IntegerConstant(x, pos)),
        (Token::CharConstant(c), pos) => Ok(Expr::CharConstant(c, pos)),
        (Token::StringConst(s), pos) => {
            can_be_indexed = true;
            Ok(Expr::StringConstant(s, pos))
        }
        (Token::Identifier(s), pos) => {
            can_be_indexed = true;
            parse_ident_expr(s, input, pos)
        }
        (Token::LeftParen, pos) => {
            can_be_indexed = true;
            parse_paren_expr(input, pos)
        }
        #[cfg(not(feature = "no_index"))]
        (Token::LeftBracket, pos) => {
            can_be_indexed = true;
            parse_array_literal(input, pos)
        }
        (Token::True, pos) => Ok(Expr::True(pos)),
        (Token::False, pos) => Ok(Expr::False(pos)),
        (Token::LexError(err), pos) => Err(ParseError::new(PERR::BadInput(err.to_string()), pos)),
        (token, pos) => Err(ParseError::new(
            PERR::BadInput(format!("Unexpected '{}'", token.syntax())),
            pos,
        )),
    }?;

    if can_be_indexed {
        // Tail processing all possible indexing
        #[cfg(not(feature = "no_index"))]
        while let Some((Token::LeftBracket, pos)) = input.peek() {
            let pos = *pos;
            input.next();
            root_expr = parse_index_expr(Box::new(root_expr), input, pos)?;
        }
    }

    Ok(root_expr)
}

/// Parse a potential unary operator.
fn parse_unary<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    match input
        .peek()
        .ok_or_else(|| ParseError::new(PERR::InputPastEndOfFile, Position::eof()))?
    {
        // -expr
        (Token::UnaryMinus, pos) => {
            let pos = *pos;

            input.next();

            match parse_unary(input)? {
                // Negative integer
                Expr::IntegerConstant(i, _) => i
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
                Expr::FloatConstant(f, pos) => Ok(Expr::FloatConstant(-f, pos)),

                // Call negative function
                expr => Ok(Expr::FunctionCall("-".into(), vec![expr], None, pos)),
            }
        }
        // +expr
        (Token::UnaryPlus, _) => {
            input.next();
            parse_unary(input)
        }
        // !expr
        (Token::Bang, pos) => {
            let pos = *pos;

            input.next();

            Ok(Expr::FunctionCall(
                "!".into(),
                vec![parse_primary(input)?],
                Some(Box::new(false)), // NOT operator, when operating on invalid operand, defaults to false
                pos,
            ))
        }
        // All other tokens
        _ => parse_primary(input),
    }
}

/// Parse an assignment.
fn parse_assignment(lhs: Expr, rhs: Expr, pos: Position) -> Result<Expr, ParseError> {
    // Is the LHS in a valid format for an assignment target?
    fn valid_assignment_chain(expr: &Expr, is_top: bool) -> Option<ParseError> {
        match expr {
            // var
            Expr::Variable(_, _) => {
                assert!(is_top, "property expected but gets variable");
                None
            }
            // property
            Expr::Property(_, _) => {
                assert!(!is_top, "variable expected but gets property");
                None
            }

            // var[...]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, _) if matches!(idx_lhs.as_ref(), &Expr::Variable(_, _)) => {
                assert!(is_top, "property expected but gets variable");
                None
            }
            // property[...]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, _) if matches!(idx_lhs.as_ref(), &Expr::Property(_, _)) => {
                assert!(!is_top, "variable expected but gets property");
                None
            }

            // idx_lhs[...]
            #[cfg(not(feature = "no_index"))]
            Expr::Index(idx_lhs, _, pos) => Some(ParseError::new(
                match idx_lhs.as_ref() {
                    Expr::Index(_, _, _) => ParseErrorType::AssignmentToCopy,
                    _ => ParseErrorType::AssignmentToInvalidLHS,
                },
                *pos,
            )),

            // dot_lhs.dot_rhs
            Expr::Dot(dot_lhs, dot_rhs, _) => match dot_lhs.as_ref() {
                // var.dot_rhs
                Expr::Variable(_, _) if is_top => valid_assignment_chain(dot_rhs, false),
                // property.dot_rhs
                Expr::Property(_, _) if !is_top => valid_assignment_chain(dot_rhs, false),
                // var[...]
                #[cfg(not(feature = "no_index"))]
                Expr::Index(idx_lhs, _, _)
                    if matches!(idx_lhs.as_ref(), &Expr::Variable(_, _)) && is_top =>
                {
                    valid_assignment_chain(dot_rhs, false)
                }
                // property[...]
                #[cfg(not(feature = "no_index"))]
                Expr::Index(idx_lhs, _, _)
                    if matches!(idx_lhs.as_ref(), &Expr::Property(_, _)) && !is_top =>
                {
                    valid_assignment_chain(dot_rhs, false)
                }
                // idx_lhs[...]
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

    match valid_assignment_chain(&lhs, true) {
        None => Ok(Expr::Assignment(Box::new(lhs), Box::new(rhs), pos)),
        Some(err) => Err(err),
    }
}

/// Parse an operator-assignment expression.
fn parse_op_assignment(op: &str, lhs: Expr, rhs: Expr, pos: Position) -> Result<Expr, ParseError> {
    let lhs_copy = lhs.clone();

    // lhs op= rhs -> lhs = op(lhs, rhs)
    parse_assignment(
        lhs,
        Expr::FunctionCall(op.into(), vec![lhs_copy, rhs], None, pos),
        pos,
    )
}

/// Parse a binary expression.
fn parse_binary_op<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    parent_precedence: u8,
    lhs: Expr,
) -> Result<Expr, ParseError> {
    let mut current_lhs = lhs;

    loop {
        let (current_precedence, bind_right) = if let Some((ref current_op, _)) = input.peek() {
            (current_op.precedence(), current_op.is_bind_right())
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

            let next_precedence = if let Some((next_op, _)) = input.peek() {
                next_op.precedence()
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

/// Parse an expression.
fn parse_expr<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Expr, ParseError> {
    let lhs = parse_unary(input)?;
    parse_binary_op(input, 1, lhs)
}

/// Parse an if statement.
fn parse_if<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    breakable: bool,
) -> Result<Stmt, ParseError> {
    // if ...
    input.next();

    // if guard { body }
    let guard = parse_expr(input)?;
    let if_body = parse_block(input, breakable)?;

    // if guard { body } else ...
    let else_body = if matches!(input.peek(), Some((Token::Else, _))) {
        input.next();

        Some(Box::new(if matches!(input.peek(), Some((Token::If, _))) {
            // if guard { body } else if ...
            parse_if(input, breakable)?
        } else {
            // if guard { body } else { else-body }
            parse_block(input, breakable)?
        }))
    } else {
        None
    };

    Ok(Stmt::IfElse(Box::new(guard), Box::new(if_body), else_body))
}

/// Parse a while loop.
fn parse_while<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    // while ...
    input.next();

    // while guard { body }
    let guard = parse_expr(input)?;
    let body = parse_block(input, true)?;

    Ok(Stmt::While(Box::new(guard), Box::new(body)))
}

/// Parse a loop statement.
fn parse_loop<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    // loop ...
    input.next();

    // loop { body }
    let body = parse_block(input, true)?;

    Ok(Stmt::Loop(Box::new(body)))
}

/// Parse a for loop.
fn parse_for<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    // for ...
    input.next();

    // for name ...
    let name = match input
        .next()
        .ok_or_else(|| ParseError::new(PERR::VariableExpected, Position::eof()))?
    {
        // Variable name
        (Token::Identifier(s), _) => s,
        // Bad identifier
        (Token::LexError(err), pos) => {
            return Err(ParseError::new(PERR::BadInput(err.to_string()), pos))
        }
        // Not a variable name
        (_, pos) => return Err(ParseError::new(PERR::VariableExpected, pos)),
    };

    // for name in ...
    match input
        .next()
        .ok_or_else(|| ParseError::new(PERR::MissingIn, Position::eof()))?
    {
        (Token::In, _) => (),
        (_, pos) => return Err(ParseError::new(PERR::MissingIn, pos)),
    }

    // for name in expr { body }
    let expr = parse_expr(input)?;
    let body = parse_block(input, true)?;

    Ok(Stmt::For(name, Box::new(expr), Box::new(body)))
}

/// Parse a variable definition statement.
fn parse_let<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    var_type: VariableType,
) -> Result<Stmt, ParseError> {
    // let/const... (specified in `var_type`)
    input.next();

    // let name ...
    let (name, pos) = match input
        .next()
        .ok_or_else(|| ParseError::new(PERR::VariableExpected, Position::eof()))?
    {
        (Token::Identifier(s), pos) => (s, pos),
        (Token::LexError(err), pos) => {
            return Err(ParseError::new(PERR::BadInput(err.to_string()), pos))
        }
        (_, pos) => return Err(ParseError::new(PERR::VariableExpected, pos)),
    };

    // let name = ...
    if matches!(input.peek(), Some((Token::Equals, _))) {
        input.next();

        // let name = expr
        let init_value = parse_expr(input)?;

        match var_type {
            // let name = expr
            VariableType::Normal => Ok(Stmt::Let(name, Some(Box::new(init_value)), pos)),
            // const name = { expr:constant }
            VariableType::Constant if init_value.is_constant() => {
                Ok(Stmt::Const(name, Box::new(init_value), pos))
            }
            // const name = expr - error
            VariableType::Constant => Err(ParseError(
                PERR::ForbiddenConstantExpr(name.to_string()),
                init_value.position(),
            )),
        }
    } else {
        // let name
        Ok(Stmt::Let(name, None, pos))
    }
}

/// Parse a statement block.
fn parse_block<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    breakable: bool,
) -> Result<Stmt, ParseError> {
    // Must start with {
    let pos = match input
        .next()
        .ok_or_else(|| ParseError::new(PERR::MissingLeftBrace, Position::eof()))?
    {
        (Token::LeftBrace, pos) => pos,
        (_, pos) => return Err(ParseError::new(PERR::MissingLeftBrace, pos)),
    };

    let mut statements = Vec::new();

    while !matches!(input.peek(), Some((Token::RightBrace, _))) {
        // Parse statements inside the block
        let stmt = parse_stmt(input, breakable)?;

        // See if it needs a terminating semicolon
        let need_semicolon = !stmt.is_self_terminated();

        statements.push(stmt);

        match input.peek() {
            // EOF
            None => break,
            // { ... stmt }
            Some((Token::RightBrace, _)) => break,
            // { ... stmt;
            Some((Token::SemiColon, _)) if need_semicolon => {
                input.next();
            }
            // { ... { stmt } ;
            Some((Token::SemiColon, _)) if !need_semicolon => (),
            // { ... { stmt } ???
            Some((_, _)) if !need_semicolon => (),
            // { ... stmt ??? - error
            Some((_, pos)) => {
                // Semicolons are not optional between statements
                return Err(ParseError::new(
                    PERR::MissingSemicolon("terminating a statement".into()),
                    *pos,
                ));
            }
        }
    }

    match input.peek().ok_or_else(|| {
        ParseError::new(
            PERR::MissingRightBrace("end of block".into()),
            Position::eof(),
        )
    })? {
        (Token::RightBrace, _) => {
            input.next();
            Ok(Stmt::Block(statements, pos))
        }
        (_, pos) => Err(ParseError::new(
            PERR::MissingRightBrace("end of block".into()),
            *pos,
        )),
    }
}

/// Parse an expression as a statement.
fn parse_expr_stmt<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<Stmt, ParseError> {
    Ok(Stmt::Expr(Box::new(parse_expr(input)?)))
}

/// Parse a single statement.
fn parse_stmt<'a>(
    input: &mut Peekable<TokenIterator<'a>>,
    breakable: bool,
) -> Result<Stmt, ParseError> {
    match input
        .peek()
        .ok_or_else(|| ParseError::new(PERR::InputPastEndOfFile, Position::eof()))?
    {
        // Semicolon - empty statement
        (Token::SemiColon, pos) => Ok(Stmt::Noop(*pos)),

        // fn ...
        #[cfg(not(feature = "no_function"))]
        (Token::Fn, pos) => return Err(ParseError::new(PERR::WrongFnDefinition, *pos)),

        (Token::If, _) => parse_if(input, breakable),
        (Token::While, _) => parse_while(input),
        (Token::Loop, _) => parse_loop(input),
        (Token::For, _) => parse_for(input),
        (Token::Break, pos) if breakable => {
            let pos = *pos;
            input.next();
            Ok(Stmt::Break(pos))
        }
        (Token::Break, pos) => return Err(ParseError::new(PERR::LoopBreak, *pos)),
        (token @ Token::Return, _) | (token @ Token::Throw, _) => {
            let return_type = match token {
                Token::Return => ReturnType::Return,
                Token::Throw => ReturnType::Exception,
                _ => panic!("token should be return or throw"),
            };

            input.next();

            match input.peek() {
                // `return`/`throw` at EOF
                None => Ok(Stmt::ReturnWithVal(None, return_type, Position::eof())),
                // `return;` or `throw;`
                Some((Token::SemiColon, pos)) => {
                    let pos = *pos;
                    Ok(Stmt::ReturnWithVal(None, return_type, pos))
                }
                // `return` or `throw` with expression
                Some((_, pos)) => {
                    let pos = *pos;
                    Ok(Stmt::ReturnWithVal(
                        Some(Box::new(parse_expr(input)?)),
                        return_type,
                        pos,
                    ))
                }
            }
        }
        (Token::LeftBrace, _) => parse_block(input, breakable),
        (Token::Let, _) => parse_let(input, VariableType::Normal),
        (Token::Const, _) => parse_let(input, VariableType::Constant),
        _ => parse_expr_stmt(input),
    }
}

/// Parse a function definition.
#[cfg(not(feature = "no_function"))]
fn parse_fn<'a>(input: &mut Peekable<TokenIterator<'a>>) -> Result<FnDef, ParseError> {
    let pos = input
        .next()
        .ok_or_else(|| ParseError::new(PERR::InputPastEndOfFile, Position::eof()))?
        .1;

    let name = match input
        .next()
        .ok_or_else(|| ParseError::new(PERR::FnMissingName, Position::eof()))?
    {
        (Token::Identifier(s), _) => s,
        (_, pos) => return Err(ParseError::new(PERR::FnMissingName, pos)),
    };

    match input
        .peek()
        .ok_or_else(|| ParseError::new(PERR::FnMissingParams(name.clone()), Position::eof()))?
    {
        (Token::LeftParen, _) => {
            input.next();
        }
        (_, pos) => return Err(ParseError::new(PERR::FnMissingParams(name), *pos)),
    }

    let mut params = Vec::new();

    if matches!(input.peek(), Some((Token::RightParen, _))) {
        input.next();
    } else {
        loop {
            match input.next().ok_or_else(|| {
                ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the parameters list of function '{}'",
                        name
                    )),
                    Position::eof(),
                )
            })? {
                (Token::Identifier(s), _) => {
                    params.push(s.into());
                }
                (_, pos) => {
                    return Err(ParseError::new(
                        PERR::MissingRightParen(format!(
                            "closing the parameters list of function '{}'",
                            name
                        )),
                        pos,
                    ))
                }
            }

            match input.next().ok_or_else(|| {
                ParseError::new(
                    PERR::MissingRightParen(format!(
                        "closing the parameters list of function '{}'",
                        name
                    )),
                    Position::eof(),
                )
            })? {
                (Token::RightParen, _) => break,
                (Token::Comma, _) => (),
                (Token::Identifier(_), _) => {
                    return Err(ParseError::new(
                        PERR::MissingComma(format!(
                            "separating the parameters of function '{}'",
                            name
                        )),
                        pos,
                    ))
                }
                (_, pos) => {
                    return Err(ParseError::new(
                        PERR::MissingRightParen(format!(
                            "closing the parameters list of function '{}'",
                            name
                        )),
                        pos,
                    ))
                }
            }
        }
    }

    let body = match input.peek() {
        Some((Token::LeftBrace, _)) => parse_block(input, false)?,
        Some((_, pos)) => return Err(ParseError::new(PERR::FnMissingBody(name), *pos)),
        None => return Err(ParseError::new(PERR::FnMissingBody(name), Position::eof())),
    };

    Ok(FnDef {
        name,
        params,
        body,
        pos,
    })
}

/// Parse the global level statements.
fn parse_global_level<'a, 'e>(
    input: &mut Peekable<TokenIterator<'a>>,
) -> Result<(Vec<Stmt>, Vec<FnDef>), ParseError> {
    let mut statements = Vec::<Stmt>::new();
    let mut functions = Vec::<FnDef>::new();

    while input.peek().is_some() {
        #[cfg(not(feature = "no_function"))]
        {
            // Collect all the function definitions
            if matches!(input.peek().expect("should not be None"), (Token::Fn, _)) {
                let f = parse_fn(input)?;

                // Ensure list is sorted
                match functions.binary_search_by(|fn_def| fn_def.compare(&f.name, f.params.len())) {
                    Ok(n) => functions[n] = f,        // Override previous definition
                    Err(n) => functions.insert(n, f), // New function definition
                }

                continue;
            }
        }

        // Actual statement
        let stmt = parse_stmt(input, false)?;

        let need_semicolon = !stmt.is_self_terminated();

        statements.push(stmt);

        match input.peek() {
            // EOF
            None => break,
            // stmt ;
            Some((Token::SemiColon, _)) if need_semicolon => {
                input.next();
            }
            // stmt ;
            Some((Token::SemiColon, _)) if !need_semicolon => (),
            // { stmt } ???
            Some((_, _)) if !need_semicolon => (),
            // stmt ??? - error
            Some((_, pos)) => {
                // Semicolons are not optional between statements
                return Err(ParseError::new(
                    PERR::MissingSemicolon("terminating a statement".into()),
                    *pos,
                ));
            }
        }
    }

    Ok((statements, functions))
}

/// Run the parser on an input stream, returning an AST.
pub fn parse<'a, 'e>(
    input: &mut Peekable<TokenIterator<'a>>,
    engine: &Engine<'e>,
    scope: &Scope,
) -> Result<AST, ParseError> {
    let (statements, functions) = parse_global_level(input)?;

    Ok(
        // Optimize AST
        #[cfg(not(feature = "no_optimize"))]
        optimize_into_ast(engine, scope, statements, functions),
        //
        // Do not optimize AST if `no_optimize`
        #[cfg(feature = "no_optimize")]
        AST(statements, functions.into_iter().map(Arc::new).collect()),
    )
}

/// Map a `Dynamic` value to an expression.
///
/// Returns Some(expression) if conversion is successful.  Otherwise None.
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
