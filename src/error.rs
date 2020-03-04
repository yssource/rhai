use crate::parser::Position;
use std::char;
use std::error::Error;
use std::fmt;

/// Error when tokenizing the script text.
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum LexError {
    /// An unexpected character is encountered when tokenizing the script text.
    UnexpectedChar(char),
    /// A string literal is not terminated before a new-line or EOF.
    UnterminatedString,
    /// An string/character/numeric escape sequence is in an invalid format.
    MalformedEscapeSequence(String),
    /// An numeric literal is in an invalid format.
    MalformedNumber(String),
    /// An character literal is in an invalid format.
    MalformedChar(String),
    /// Error in the script text.
    InputError(String),
}

impl Error for LexError {
    fn description(&self) -> &str {
        match *self {
            Self::UnexpectedChar(_) => "Unexpected character",
            Self::UnterminatedString => "Open string is not terminated",
            Self::MalformedEscapeSequence(_) => "Unexpected values in escape sequence",
            Self::MalformedNumber(_) => "Unexpected characters in number",
            Self::MalformedChar(_) => "Char constant not a single character",
            Self::InputError(_) => "Input error",
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected '{}'", c),
            Self::MalformedEscapeSequence(s) => write!(f, "Invalid escape sequence: '{}'", s),
            Self::MalformedNumber(s) => write!(f, "Invalid number: '{}'", s),
            Self::MalformedChar(s) => write!(f, "Invalid character: '{}'", s),
            Self::InputError(s) => write!(f, "{}", s),
            _ => write!(f, "{}", self.description()),
        }
    }
}

/// Type of error encountered when parsing a script.
#[derive(Debug, PartialEq, Clone)]
pub enum ParseErrorType {
    /// Error in the script text. Wrapped value is the error message.
    BadInput(String),
    /// The script ends prematurely.
    InputPastEndOfFile,
    /// An unknown operator is encountered. Wrapped value is the operator.
    UnknownOperator(String),
    /// An open `(` is missing the corresponding closing `)`.
    MissingRightParen,
    /// Expecting `(` but not finding one.
    MissingLeftBrace,
    /// An open `{` is missing the corresponding closing `}`.
    MissingRightBrace,
    /// An open `[` is missing the corresponding closing `]`.
    MissingRightBracket,
    /// An expression in function call arguments `()` has syntax error.
    MalformedCallExpr,
    /// An expression in indexing brackets `[]` has syntax error.
    MalformedIndexExpr,
    /// Missing a variable name after the `let` keyword.
    VarExpectsIdentifier,
    /// Defining a function `fn` in an appropriate place (e.g. inside another function).
    WrongFnDefinition,
    /// Missing a function name after the `fn` keyword.
    FnMissingName,
    /// A function definition is missing the parameters list. Wrapped value is the function name.
    FnMissingParams(String),
}

/// Error when parsing a script.
#[derive(Debug, PartialEq, Clone)]
pub struct ParseError(ParseErrorType, Position);

impl ParseError {
    /// Create a new `ParseError`.
    pub(crate) fn new(err: ParseErrorType, pos: Position) -> Self {
        Self(err, pos)
    }

    /// Get the parse error.
    pub fn error_type(&self) -> &ParseErrorType {
        &self.0
    }

    /// Get the location in the script of the error.
    pub fn position(&self) -> Position {
        self.1
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.0 {
            ParseErrorType::BadInput(ref p) => p,
            ParseErrorType::InputPastEndOfFile => "Script is incomplete",
            ParseErrorType::UnknownOperator(_) => "Unknown operator",
            ParseErrorType::MissingRightParen => "Expecting ')'",
            ParseErrorType::MissingLeftBrace => "Expecting '{'",
            ParseErrorType::MissingRightBrace => "Expecting '}'",
            ParseErrorType::MissingRightBracket => "Expecting ']'",
            ParseErrorType::MalformedCallExpr => "Invalid expression in function call arguments",
            ParseErrorType::MalformedIndexExpr => "Invalid index in indexing expression",
            ParseErrorType::VarExpectsIdentifier => "Expecting name of a variable",
            ParseErrorType::FnMissingName => "Expecting name in function declaration",
            ParseErrorType::FnMissingParams(_) => "Expecting parameters in function declaration",
            ParseErrorType::WrongFnDefinition => "Function definitions must be at top level and cannot be inside a block or another function",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            ParseErrorType::BadInput(ref s) => write!(f, "{}", s)?,
            ParseErrorType::UnknownOperator(ref s) => write!(f, "{}: '{}'", self.description(), s)?,
            ParseErrorType::FnMissingParams(ref s) => {
                write!(f, "Missing parameters for function '{}'", s)?
            }
            _ => write!(f, "{}", self.description())?,
        }

        if !self.1.is_eof() {
            write!(f, " ({})", self.1)
        } else {
            write!(f, " at the end of the script but there is no more input")
        }
    }
}
