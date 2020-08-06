//! Module containing error definitions for the evaluation process.

use crate::any::Dynamic;
use crate::error::ParseErrorType;
use crate::parser::INT;
use crate::token::Position;

use crate::stdlib::{
    boxed::Box,
    error::Error,
    fmt,
    string::{String, ToString},
};

#[cfg(not(feature = "no_std"))]
#[cfg(not(target_arch = "wasm32"))]
use crate::stdlib::path::PathBuf;

/// Evaluation result.
///
/// All wrapped `Position` values represent the location in the script where the error occurs.
///
/// Currently, `EvalAltResult` is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
#[derive(Debug)]
#[non_exhaustive]
pub enum EvalAltResult {
    /// Syntax error.
    ErrorParsing(ParseErrorType, Position),

    /// Error reading from a script file. Wrapped value is the path of the script file.
    ///
    /// Never appears under the `no_std` feature.
    #[cfg(not(feature = "no_std"))]
    #[cfg(not(target_arch = "wasm32"))]
    ErrorReadingScriptFile(PathBuf, Position, std::io::Error),

    /// Call to an unknown function. Wrapped value is the signature of the function.
    ErrorFunctionNotFound(String, Position),
    /// An error has occurred inside a called function.
    /// Wrapped values are the name of the function and the interior error.
    ErrorInFunctionCall(String, Box<EvalAltResult>, Position),
    /// Access to `this` that is not bound.
    ErrorUnboundThis(Position),
    /// Non-boolean operand encountered for boolean operator. Wrapped value is the operator.
    ErrorBooleanArgMismatch(String, Position),
    /// Non-character value encountered where a character is required.
    ErrorCharMismatch(Position),
    /// Array access out-of-bounds.
    /// Wrapped values are the current number of elements in the array and the index number.
    ErrorArrayBounds(usize, INT, Position),
    /// String indexing out-of-bounds.
    /// Wrapped values are the current number of characters in the string and the index number.
    ErrorStringBounds(usize, INT, Position),
    /// Trying to index into a type that is not an array, an object map, or a string, and has no indexer function defined.
    ErrorIndexingType(String, Position),
    /// Trying to index into an array or string with an index that is not `i64`.
    ErrorNumericIndexExpr(Position),
    /// Trying to index into a map with an index that is not `String`.
    ErrorStringIndexExpr(Position),
    /// Trying to import with an expression that is not `String`.
    ErrorImportExpr(Position),
    /// Invalid arguments for `in` operator.
    ErrorInExpr(Position),
    /// The guard expression in an `if` or `while` statement does not return a boolean value.
    ErrorLogicGuard(Position),
    /// The `for` statement encounters a type that is not an iterator.
    ErrorFor(Position),
    /// Usage of an unknown variable. Wrapped value is the name of the variable.
    ErrorVariableNotFound(String, Position),
    /// Usage of an unknown module. Wrapped value is the name of the module.
    ErrorModuleNotFound(String, Position),
    /// Data race detected when accessing a variable. Wrapped value is the name of the variable.
    ErrorDataRace(String, Position),
    /// Assignment to an inappropriate LHS (left-hand-side) expression.
    ErrorAssignmentToUnknownLHS(Position),
    /// Assignment to a constant variable.
    ErrorAssignmentToConstant(String, Position),
    /// Returned type is not the same as the required output type.
    /// Wrapped values are the type requested and type of the actual result.
    ErrorMismatchOutputType(String, String, Position),
    /// Inappropriate member access.
    ErrorDotExpr(String, Position),
    /// Arithmetic error encountered. Wrapped value is the error message.
    ErrorArithmetic(String, Position),
    /// Number of operations over maximum limit.
    ErrorTooManyOperations(Position),
    /// Modules over maximum limit.
    ErrorTooManyModules(Position),
    /// Call stack over maximum limit.
    ErrorStackOverflow(Position),
    /// Data value over maximum size limit. Wrapped values are the data type, maximum size and current size.
    ErrorDataTooLarge(String, usize, usize, Position),
    /// The script is prematurely terminated.
    ErrorTerminated(Position),
    /// Run-time error encountered. Wrapped value is the error message.
    ErrorRuntime(String, Position),

    /// Breaking out of loops - not an error if within a loop.
    /// The wrapped value, if true, means breaking clean out of the loop (i.e. a `break` statement).
    /// The wrapped value, if false, means breaking the current context (i.e. a `continue` statement).
    ErrorLoopBreak(bool, Position),
    /// Not an error: Value returned from a script via the `return` keyword.
    /// Wrapped value is the result value.
    Return(Dynamic, Position),
}

impl EvalAltResult {
    pub(crate) fn desc(&self) -> &str {
        match self {
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            Self::ErrorReadingScriptFile(_, _, _) => "Cannot read from script file",

            Self::ErrorParsing(p, _) => p.desc(),
            Self::ErrorInFunctionCall(_, _, _) => "Error in called function",
            Self::ErrorFunctionNotFound(_, _) => "Function not found",
            Self::ErrorUnboundThis(_) => "'this' is not bound",
            Self::ErrorBooleanArgMismatch(_, _) => "Boolean operator expects boolean operands",
            Self::ErrorCharMismatch(_) => "Character expected",
            Self::ErrorNumericIndexExpr(_) => {
                "Indexing into an array or string expects an integer index"
            }
            Self::ErrorStringIndexExpr(_) => "Indexing into an object map expects a string index",
            Self::ErrorIndexingType(_, _) => {
                "Indexing can only be performed on an array, an object map, a string, or a type with an indexer function defined"
            }
            Self::ErrorImportExpr(_) => "Importing a module expects a string path",
            Self::ErrorArrayBounds(_, index, _) if *index < 0 => {
                "Array access expects non-negative index"
            }
            Self::ErrorArrayBounds(0, _, _) => "Empty array has nothing to access",
            Self::ErrorArrayBounds(_, _, _) => "Array index out of bounds",
            Self::ErrorStringBounds(_, index, _) if *index < 0 => {
                "Indexing a string expects a non-negative index"
            }
            Self::ErrorStringBounds(0, _, _) => "Empty string has nothing to index",
            Self::ErrorStringBounds(_, _, _) => "String index out of bounds",
            Self::ErrorLogicGuard(_) => "Boolean value expected",
            Self::ErrorFor(_) => "For loop expects an array, object map, or range",
            Self::ErrorVariableNotFound(_, _) => "Variable not found",
            Self::ErrorModuleNotFound(_, _) => "Module not found",
            Self::ErrorDataRace(_, _) => "Data race detected when accessing variable",
            Self::ErrorAssignmentToUnknownLHS(_) => {
                "Assignment to an unsupported left-hand side expression"
            }
            Self::ErrorAssignmentToConstant(_, _) => "Assignment to a constant variable",
            Self::ErrorMismatchOutputType(_, _, _) => "Output type is incorrect",
            Self::ErrorInExpr(_) => "Malformed 'in' expression",
            Self::ErrorDotExpr(_, _) => "Malformed dot expression",
            Self::ErrorArithmetic(_, _) => "Arithmetic error",
            Self::ErrorTooManyOperations(_) => "Too many operations",
            Self::ErrorTooManyModules(_) => "Too many modules imported",
            Self::ErrorStackOverflow(_) => "Stack overflow",
            Self::ErrorDataTooLarge(_, _, _, _) => "Data size exceeds maximum limit",
            Self::ErrorTerminated(_) => "Script terminated.",
            Self::ErrorRuntime(_, _) => "Runtime error",
            Self::ErrorLoopBreak(true, _) => "Break statement not inside a loop",
            Self::ErrorLoopBreak(false, _) => "Continue statement not inside a loop",
            Self::Return(_, _) => "[Not Error] Function returns value",
        }
    }
}

impl Error for EvalAltResult {}

impl fmt::Display for EvalAltResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let desc = self.desc();
        let pos = self.position();

        match self {
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            Self::ErrorReadingScriptFile(path, _, err) => {
                write!(f, "{} '{}': {}", desc, path.display(), err)?
            }

            Self::ErrorParsing(p, _) => write!(f, "Syntax error: {}", p)?,

            Self::ErrorInFunctionCall(s, err, _) => {
                write!(f, "Error in call to function '{}' : {}", s, err)?
            }

            Self::ErrorFunctionNotFound(s, _)
            | Self::ErrorVariableNotFound(s, _)
            | Self::ErrorDataRace(s, _)
            | Self::ErrorModuleNotFound(s, _) => write!(f, "{}: '{}'", desc, s)?,

            Self::ErrorDotExpr(s, _) if !s.is_empty() => write!(f, "{}", s)?,

            Self::ErrorIndexingType(_, _)
            | Self::ErrorNumericIndexExpr(_)
            | Self::ErrorStringIndexExpr(_)
            | Self::ErrorUnboundThis(_)
            | Self::ErrorImportExpr(_)
            | Self::ErrorLogicGuard(_)
            | Self::ErrorFor(_)
            | Self::ErrorAssignmentToUnknownLHS(_)
            | Self::ErrorInExpr(_)
            | Self::ErrorDotExpr(_, _)
            | Self::ErrorTooManyOperations(_)
            | Self::ErrorTooManyModules(_)
            | Self::ErrorStackOverflow(_)
            | Self::ErrorTerminated(_) => f.write_str(desc)?,

            Self::ErrorRuntime(s, _) => f.write_str(if s.is_empty() { desc } else { s })?,

            Self::ErrorAssignmentToConstant(s, _) => write!(f, "{}: '{}'", desc, s)?,
            Self::ErrorMismatchOutputType(r, s, _) => {
                write!(f, "{} (expecting {}): {}", desc, s, r)?
            }
            Self::ErrorArithmetic(s, _) => f.write_str(s)?,

            Self::ErrorLoopBreak(_, _) => f.write_str(desc)?,
            Self::Return(_, _) => f.write_str(desc)?,

            Self::ErrorBooleanArgMismatch(op, _) => {
                write!(f, "{} operator expects boolean operands", op)?
            }
            Self::ErrorCharMismatch(_) => write!(f, "string indexing expects a character value")?,
            Self::ErrorArrayBounds(_, index, _) if *index < 0 => {
                write!(f, "{}: {} < 0", desc, index)?
            }
            Self::ErrorArrayBounds(0, _, _) => f.write_str(desc)?,
            Self::ErrorArrayBounds(1, index, _) => write!(
                f,
                "Array index {} is out of bounds: only one element in the array",
                index
            )?,
            Self::ErrorArrayBounds(max, index, _) => write!(
                f,
                "Array index {} is out of bounds: only {} elements in the array",
                index, max
            )?,
            Self::ErrorStringBounds(_, index, _) if *index < 0 => {
                write!(f, "{}: {} < 0", desc, index)?
            }
            Self::ErrorStringBounds(0, _, _) => f.write_str(desc)?,
            Self::ErrorStringBounds(1, index, _) => write!(
                f,
                "String index {} is out of bounds: only one character in the string",
                index
            )?,
            Self::ErrorStringBounds(max, index, _) => write!(
                f,
                "String index {} is out of bounds: only {} characters in the string",
                index, max
            )?,
            Self::ErrorDataTooLarge(typ, max, size, _) => {
                write!(f, "{} ({}) exceeds the maximum limit ({})", typ, size, max)?
            }
        }

        // Do not write any position if None
        if !pos.is_none() {
            write!(f, " ({})", pos)?;
        }

        Ok(())
    }
}

impl<T: AsRef<str>> From<T> for Box<EvalAltResult> {
    fn from(err: T) -> Self {
        Box::new(EvalAltResult::ErrorRuntime(
            err.as_ref().to_string(),
            Position::none(),
        ))
    }
}

impl EvalAltResult {
    /// Get the `Position` of this error.
    pub fn position(&self) -> Position {
        match self {
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            Self::ErrorReadingScriptFile(_, pos, _) => *pos,

            Self::ErrorParsing(_, pos)
            | Self::ErrorFunctionNotFound(_, pos)
            | Self::ErrorInFunctionCall(_, _, pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorBooleanArgMismatch(_, pos)
            | Self::ErrorCharMismatch(pos)
            | Self::ErrorArrayBounds(_, _, pos)
            | Self::ErrorStringBounds(_, _, pos)
            | Self::ErrorIndexingType(_, pos)
            | Self::ErrorNumericIndexExpr(pos)
            | Self::ErrorStringIndexExpr(pos)
            | Self::ErrorImportExpr(pos)
            | Self::ErrorLogicGuard(pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableNotFound(_, pos)
            | Self::ErrorModuleNotFound(_, pos)
            | Self::ErrorDataRace(_, pos)
            | Self::ErrorAssignmentToUnknownLHS(pos)
            | Self::ErrorAssignmentToConstant(_, pos)
            | Self::ErrorMismatchOutputType(_, _, pos)
            | Self::ErrorInExpr(pos)
            | Self::ErrorDotExpr(_, pos)
            | Self::ErrorArithmetic(_, pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(_, _, _, pos)
            | Self::ErrorTerminated(pos)
            | Self::ErrorRuntime(_, pos)
            | Self::ErrorLoopBreak(_, pos)
            | Self::Return(_, pos) => *pos,
        }
    }

    /// Override the `Position` of this error.
    pub fn set_position(&mut self, new_position: Position) {
        match self {
            #[cfg(not(feature = "no_std"))]
            #[cfg(not(target_arch = "wasm32"))]
            Self::ErrorReadingScriptFile(_, pos, _) => *pos = new_position,

            Self::ErrorParsing(_, pos)
            | Self::ErrorFunctionNotFound(_, pos)
            | Self::ErrorInFunctionCall(_, _, pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorBooleanArgMismatch(_, pos)
            | Self::ErrorCharMismatch(pos)
            | Self::ErrorArrayBounds(_, _, pos)
            | Self::ErrorStringBounds(_, _, pos)
            | Self::ErrorIndexingType(_, pos)
            | Self::ErrorNumericIndexExpr(pos)
            | Self::ErrorStringIndexExpr(pos)
            | Self::ErrorImportExpr(pos)
            | Self::ErrorLogicGuard(pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableNotFound(_, pos)
            | Self::ErrorModuleNotFound(_, pos)
            | Self::ErrorDataRace(_, pos)
            | Self::ErrorAssignmentToUnknownLHS(pos)
            | Self::ErrorAssignmentToConstant(_, pos)
            | Self::ErrorMismatchOutputType(_, _, pos)
            | Self::ErrorInExpr(pos)
            | Self::ErrorDotExpr(_, pos)
            | Self::ErrorArithmetic(_, pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(_, _, _, pos)
            | Self::ErrorTerminated(pos)
            | Self::ErrorRuntime(_, pos)
            | Self::ErrorLoopBreak(_, pos)
            | Self::Return(_, pos) => *pos = new_position,
        }
    }

    /// Consume the current `EvalAltResult` and return a new one with the specified `Position`
    /// if the current position is `Position::None`.
    pub(crate) fn new_position(mut self: Box<Self>, new_position: Position) -> Box<Self> {
        if self.position().is_none() {
            self.set_position(new_position);
        }
        self
    }
}

impl<T> From<EvalAltResult> for Result<T, Box<EvalAltResult>> {
    fn from(err: EvalAltResult) -> Self {
        Err(err.into())
    }
}
