//! Module containing error definitions for the parsing process.

use crate::parser::Position;
use std::{char, error::Error, fmt};

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
    /// An identifier is in an invalid format.
    MalformedIdentifier(String),
}

impl Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected '{}'", c),
            Self::MalformedEscapeSequence(s) => write!(f, "Invalid escape sequence: '{}'", s),
            Self::MalformedNumber(s) => write!(f, "Invalid number: '{}'", s),
            Self::MalformedChar(s) => write!(f, "Invalid character: '{}'", s),
            Self::MalformedIdentifier(s) => {
                write!(f, "Variable name is not in a legal format: '{}'", s)
            }
            Self::InputError(s) => write!(f, "{}", s),
            Self::UnterminatedString => write!(f, "Open string is not terminated"),
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
    MissingRightParen(String),
    /// Expecting `(` but not finding one.
    MissingLeftBrace,
    /// An open `{` is missing the corresponding closing `}`.
    MissingRightBrace(String),
    /// An open `[` is missing the corresponding closing `]`.
    #[cfg(not(feature = "no_index"))]
    MissingRightBracket(String),
    /// A list of expressions is missing the separating ','.
    MissingComma(String),
    /// An expression in function call arguments `()` has syntax error.
    MalformedCallExpr(String),
    /// An expression in indexing brackets `[]` has syntax error.
    #[cfg(not(feature = "no_index"))]
    MalformedIndexExpr(String),
    /// Invalid expression assigned to constant.
    ForbiddenConstantExpr(String),
    /// Missing a variable name after the `let`, `const` or `for` keywords.
    VariableExpected,
    /// A `for` statement is missing the `in` keyword.
    MissingIn,
    /// Defining a function `fn` in an appropriate place (e.g. inside another function).
    #[cfg(not(feature = "no_function"))]
    WrongFnDefinition,
    /// Missing a function name after the `fn` keyword.
    #[cfg(not(feature = "no_function"))]
    FnMissingName,
    /// A function definition is missing the parameters list. Wrapped value is the function name.
    #[cfg(not(feature = "no_function"))]
    FnMissingParams(String),
    /// Assignment to an inappropriate LHS (left-hand-side) expression.
    AssignmentToInvalidLHS,
    /// Assignment to a copy of a value.
    AssignmentToCopy,
    /// Assignment to an a constant variable.
    AssignmentToConstant(String),
}

/// Error when parsing a script.
#[derive(Debug, PartialEq, Clone)]
pub struct ParseError(pub(crate) ParseErrorType, pub(crate) Position);

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

    pub(crate) fn desc(&self) -> &str {
        match self.0 {
            ParseErrorType::BadInput(ref p) => p,
            ParseErrorType::InputPastEndOfFile => "Script is incomplete",
            ParseErrorType::UnknownOperator(_) => "Unknown operator",
            ParseErrorType::MissingRightParen(_) => "Expecting ')'",
            ParseErrorType::MissingLeftBrace => "Expecting '{'",
            ParseErrorType::MissingRightBrace(_) => "Expecting '}'",
            #[cfg(not(feature = "no_index"))]
            ParseErrorType::MissingRightBracket(_) => "Expecting ']'",
            ParseErrorType::MissingComma(_) => "Expecting ','",
            ParseErrorType::MalformedCallExpr(_) => "Invalid expression in function call arguments",
            #[cfg(not(feature = "no_index"))]
            ParseErrorType::MalformedIndexExpr(_) => "Invalid index in indexing expression",
            ParseErrorType::ForbiddenConstantExpr(_) => "Expecting a constant",
            ParseErrorType::MissingIn => "Expecting 'in'",
            ParseErrorType::VariableExpected => "Expecting name of a variable",
            #[cfg(not(feature = "no_function"))]
            ParseErrorType::FnMissingName => "Expecting name in function declaration",
            #[cfg(not(feature = "no_function"))]
            ParseErrorType::FnMissingParams(_) => "Expecting parameters in function declaration",
            #[cfg(not(feature = "no_function"))]
            ParseErrorType::WrongFnDefinition => "Function definitions must be at top level and cannot be inside a block or another function",
            ParseErrorType::AssignmentToInvalidLHS => "Cannot assign to this expression",
            ParseErrorType::AssignmentToCopy => "Cannot assign to this expression because it will only be changing a copy of the value",
            ParseErrorType::AssignmentToConstant(_) => "Cannot assign to a constant variable."
        }
    }
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            ParseErrorType::BadInput(ref s) | ParseErrorType::MalformedCallExpr(ref s) => {
                write!(f, "{}", if s.is_empty() { self.desc() } else { s })?
            }
            ParseErrorType::ForbiddenConstantExpr(ref s) => {
                write!(f, "Expecting a constant to assign to '{}'", s)?
            }
            ParseErrorType::UnknownOperator(ref s) => write!(f, "{}: '{}'", self.desc(), s)?,

            #[cfg(not(feature = "no_index"))]
            ParseErrorType::MalformedIndexExpr(ref s) => {
                write!(f, "{}", if s.is_empty() { self.desc() } else { s })?
            }

            #[cfg(not(feature = "no_function"))]
            ParseErrorType::FnMissingParams(ref s) => {
                write!(f, "Expecting parameters for function '{}'", s)?
            }

            ParseErrorType::MissingRightParen(ref s) | ParseErrorType::MissingRightBrace(ref s) => {
                write!(f, "{} for {}", self.desc(), s)?
            }

            #[cfg(not(feature = "no_index"))]
            ParseErrorType::MissingRightBracket(ref s) => write!(f, "{} for {}", self.desc(), s)?,

            ParseErrorType::MissingComma(ref s) => write!(f, "{} for {}", self.desc(), s)?,

            ParseErrorType::AssignmentToConstant(ref s) if s.is_empty() => {
                write!(f, "{}", self.desc())?
            }
            ParseErrorType::AssignmentToConstant(ref s) => {
                write!(f, "Cannot assign to constant '{}'", s)?
            }
            _ => write!(f, "{}", self.desc())?,
        }

        if !self.1.is_eof() {
            write!(f, " ({})", self.1)
        } else if !self.1.is_none() {
            // Do not write any position if None
            Ok(())
        } else {
            write!(f, " at the end of the script but there is no more input")
        }
    }
}
