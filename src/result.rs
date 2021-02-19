//! Module containing error definitions for the evaluation process.

use crate::stdlib::{
    boxed::Box,
    error::Error,
    fmt,
    string::{String, ToString},
};
use crate::{Dynamic, ImmutableString, ParseErrorType, Position, INT};

/// Evaluation result.
///
/// All wrapped [`Position`] values represent the location in the script where the error occurs.
///
/// # Thread Safety
///
/// Currently, [`EvalAltResult`] is neither [`Send`] nor [`Sync`].
/// Turn on the `sync` feature to make it [`Send`] `+` [`Sync`].
#[derive(Debug)]
#[non_exhaustive]
pub enum EvalAltResult {
    /// System error. Wrapped values are the error message and the internal error.
    #[cfg(not(feature = "sync"))]
    ErrorSystem(String, Box<dyn Error>),
    /// System error. Wrapped values are the error message and the internal error.
    #[cfg(feature = "sync")]
    ErrorSystem(String, Box<dyn Error + Send + Sync>),

    /// Syntax error.
    ErrorParsing(ParseErrorType, Position),

    /// Usage of an unknown variable. Wrapped value is the variable name.
    ErrorVariableNotFound(String, Position),
    /// Call to an unknown function. Wrapped value is the function signature.
    ErrorFunctionNotFound(String, Position),
    /// An error has occurred inside a called function.
    /// Wrapped values are the function name, function source, and the interior error.
    ErrorInFunctionCall(String, String, Box<EvalAltResult>, Position),
    /// Usage of an unknown [module][crate::Module]. Wrapped value is the [module][crate::Module] name.
    ErrorModuleNotFound(String, Position),
    /// An error has occurred while loading a [module][crate::Module].
    /// Wrapped value are the [module][crate::Module] name and the interior error.
    ErrorInModule(String, Box<EvalAltResult>, Position),
    /// Access to `this` that is not bound.
    ErrorUnboundThis(Position),
    /// Data is not of the required type.
    /// Wrapped values are the type requested and type of the actual result.
    ErrorMismatchDataType(String, String, Position),
    /// Returned type is not the same as the required output type.
    /// Wrapped values are the type requested and type of the actual result.
    ErrorMismatchOutputType(String, String, Position),
    /// Array access out-of-bounds.
    /// Wrapped values are the current number of elements in the array and the index number.
    ErrorArrayBounds(usize, INT, Position),
    /// String indexing out-of-bounds.
    /// Wrapped values are the current number of characters in the string and the index number.
    ErrorStringBounds(usize, INT, Position),
    /// Trying to index into a type that is not an array, an object map, or a string, and has no
    /// indexer function defined. Wrapped value is the type name.
    ErrorIndexingType(String, Position),
    /// Invalid arguments for `in` operator.
    ErrorInExpr(Position),
    /// The `for` statement encounters a type that is not an iterator.
    ErrorFor(Position),
    /// Data race detected when accessing a variable. Wrapped value is the variable name.
    ErrorDataRace(String, Position),
    /// Assignment to a constant variable. Wrapped value is the variable name.
    ErrorAssignmentToConstant(String, Position),
    /// Inappropriate property access. Wrapped value is the property name.
    ErrorDotExpr(String, Position),
    /// Arithmetic error encountered. Wrapped value is the error message.
    ErrorArithmetic(String, Position),
    /// Number of operations over maximum limit.
    ErrorTooManyOperations(Position),
    /// [Modules][crate::Module] over maximum limit.
    ErrorTooManyModules(Position),
    /// Call stack over maximum limit.
    ErrorStackOverflow(Position),
    /// Data value over maximum size limit. Wrapped value is the type name.
    ErrorDataTooLarge(String, Position),
    /// The script is prematurely terminated. Wrapped value is the termination token.
    ErrorTerminated(Dynamic, Position),
    /// Run-time error encountered. Wrapped value is the error token.
    ErrorRuntime(Dynamic, Position),

    /// Breaking out of loops - not an error if within a loop.
    /// The wrapped value, if true, means breaking clean out of the loop (i.e. a `break` statement).
    /// The wrapped value, if false, means breaking the current context (i.e. a `continue` statement).
    LoopBreak(bool, Position),
    /// Not an error: Value returned from a script via the `return` keyword.
    /// Wrapped value is the result value.
    Return(Dynamic, Position),
}

impl EvalAltResult {
    pub(crate) fn desc(&self) -> &str {
        match self {
            #[allow(deprecated)]
            Self::ErrorSystem(_, s) => s.description(),
            Self::ErrorParsing(p, _) => p.desc(),
            Self::ErrorInFunctionCall(_,_, _, _) => "Error in called function",
            Self::ErrorInModule(_, _, _) => "Error in module",
            Self::ErrorFunctionNotFound(_, _) => "Function not found",
            Self::ErrorUnboundThis(_) => "'this' is not bound",
            Self::ErrorMismatchDataType(_, _, _) => "Data type is incorrect",
            Self::ErrorIndexingType(_, _) => {
                "Indexing can only be performed on an array, an object map, a string, or a type with an indexer function defined"
            }
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
            Self::ErrorFor(_) => "For loop expects an array, object map, or range",
            Self::ErrorVariableNotFound(_, _) => "Variable not found",
            Self::ErrorModuleNotFound(_, _) => "Module not found",
            Self::ErrorDataRace(_, _) => "Data race detected when accessing variable",
            Self::ErrorAssignmentToConstant(_, _) => "Cannot modify a constant",
            Self::ErrorMismatchOutputType(_, _, _) => "Output type is incorrect",
            Self::ErrorInExpr(_) => "Malformed 'in' expression",
            Self::ErrorDotExpr(_, _) => "Malformed dot expression",
            Self::ErrorArithmetic(_, _) => "Arithmetic error",
            Self::ErrorTooManyOperations(_) => "Too many operations",
            Self::ErrorTooManyModules(_) => "Too many modules imported",
            Self::ErrorStackOverflow(_) => "Stack overflow",
            Self::ErrorDataTooLarge(_, _) => "Data size exceeds maximum limit",
            Self::ErrorTerminated(_,_) => "Script terminated.",
            Self::ErrorRuntime(_, _) => "Runtime error",
            Self::LoopBreak(true, _) => "Break statement not inside a loop",
            Self::LoopBreak(false, _) => "Continue statement not inside a loop",
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
            Self::ErrorSystem(s, _) if s.is_empty() => f.write_str(desc)?,
            Self::ErrorSystem(s, _) => write!(f, "{}: {}", s, desc)?,

            Self::ErrorParsing(p, _) => write!(f, "Syntax error: {}", p)?,

            #[cfg(not(feature = "no_function"))]
            Self::ErrorInFunctionCall(s, src, err, _) if crate::engine::is_anonymous_fn(s) => {
                write!(f, "{}, in call to closure", err)?;
                if !src.is_empty() {
                    write!(f, " @ '{}'", src)?;
                }
            }
            Self::ErrorInFunctionCall(s, src, err, _) => {
                write!(f, "{}, in call to function {}", err, s)?;
                if !src.is_empty() {
                    write!(f, " @ '{}'", src)?;
                }
            }

            Self::ErrorInModule(s, err, _) if s.is_empty() => {
                write!(f, "Error in module: {}", err)?
            }
            Self::ErrorInModule(s, err, _) => write!(f, "Error in module '{}': {}", s, err)?,

            Self::ErrorFunctionNotFound(s, _)
            | Self::ErrorVariableNotFound(s, _)
            | Self::ErrorDataRace(s, _) => write!(f, "{}: {}", desc, s)?,

            Self::ErrorModuleNotFound(s, _) => write!(f, "{}: '{}'", desc, s)?,

            Self::ErrorDotExpr(s, _) if !s.is_empty() => write!(f, "{}", s)?,

            Self::ErrorIndexingType(s, _) => write!(f, "Indexer not registered for type '{}'", s)?,

            Self::ErrorUnboundThis(_)
            | Self::ErrorFor(_)
            | Self::ErrorInExpr(_)
            | Self::ErrorDotExpr(_, _)
            | Self::ErrorTooManyOperations(_)
            | Self::ErrorTooManyModules(_)
            | Self::ErrorStackOverflow(_)
            | Self::ErrorTerminated(_, _) => f.write_str(desc)?,

            Self::ErrorRuntime(d, _) if d.is::<ImmutableString>() => {
                let s = d.as_str().unwrap();
                write!(f, "{}: {}", desc, if s.is_empty() { desc } else { s })?
            }
            Self::ErrorRuntime(d, _) if d.is::<()>() => f.write_str(desc)?,
            Self::ErrorRuntime(d, _) => write!(f, "{}: {}", desc, d)?,

            Self::ErrorAssignmentToConstant(s, _) => write!(f, "Cannot modify constant {}", s)?,
            Self::ErrorMismatchOutputType(s, r, _) => {
                write!(f, "Output type is incorrect: {} (expecting {})", r, s)?
            }
            Self::ErrorMismatchDataType(s, r, _) if r.is_empty() => {
                write!(f, "Data type is incorrect, expecting {}", s)?
            }
            Self::ErrorMismatchDataType(s, r, _) if s.is_empty() => {
                write!(f, "Data type is incorrect: {}", r)?
            }
            Self::ErrorMismatchDataType(s, r, _) => {
                write!(f, "Data type is incorrect: {} (expecting {})", r, s)?
            }
            Self::ErrorArithmetic(s, _) => f.write_str(s)?,

            Self::LoopBreak(_, _) => f.write_str(desc)?,
            Self::Return(_, _) => f.write_str(desc)?,

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
            Self::ErrorDataTooLarge(typ, _) => write!(f, "{} exceeds maximum limit", typ)?,
        }

        // Do not write any position if None
        if !pos.is_none() {
            write!(f, " ({})", pos)?;
        }

        Ok(())
    }
}

impl<T: AsRef<str>> From<T> for EvalAltResult {
    #[inline(always)]
    fn from(err: T) -> Self {
        Self::ErrorRuntime(err.as_ref().to_string().into(), Position::NONE)
    }
}

impl<T: AsRef<str>> From<T> for Box<EvalAltResult> {
    #[inline(always)]
    fn from(err: T) -> Self {
        Box::new(EvalAltResult::ErrorRuntime(
            err.as_ref().to_string().into(),
            Position::NONE,
        ))
    }
}

impl EvalAltResult {
    /// Is this a pseudo error?  A pseudo error is one that does not occur naturally.
    ///
    /// [`LoopBreak`][EvalAltResult::LoopBreak] or [`Return`][EvalAltResult::Return] are pseudo errors.
    pub fn is_pseudo_error(&self) -> bool {
        match self {
            Self::LoopBreak(_, _) | Self::Return(_, _) => true,
            _ => false,
        }
    }
    /// Can this error be caught?
    ///
    /// # Panics
    ///
    /// Panics when [`LoopBreak`][EvalAltResult::LoopBreak] or [`Return`][EvalAltResult::Return].
    pub fn is_catchable(&self) -> bool {
        match self {
            Self::ErrorSystem(_, _) => false,
            Self::ErrorParsing(_, _) => false,

            Self::ErrorFunctionNotFound(_, _)
            | Self::ErrorInFunctionCall(_, _, _, _)
            | Self::ErrorInModule(_, _, _)
            | Self::ErrorUnboundThis(_)
            | Self::ErrorMismatchDataType(_, _, _)
            | Self::ErrorArrayBounds(_, _, _)
            | Self::ErrorStringBounds(_, _, _)
            | Self::ErrorIndexingType(_, _)
            | Self::ErrorFor(_)
            | Self::ErrorVariableNotFound(_, _)
            | Self::ErrorModuleNotFound(_, _)
            | Self::ErrorDataRace(_, _)
            | Self::ErrorAssignmentToConstant(_, _)
            | Self::ErrorMismatchOutputType(_, _, _)
            | Self::ErrorInExpr(_)
            | Self::ErrorDotExpr(_, _)
            | Self::ErrorArithmetic(_, _)
            | Self::ErrorRuntime(_, _) => true,

            Self::ErrorTooManyOperations(_)
            | Self::ErrorTooManyModules(_)
            | Self::ErrorStackOverflow(_)
            | Self::ErrorDataTooLarge(_, _)
            | Self::ErrorTerminated(_, _) => false,

            Self::LoopBreak(_, _) => panic!("EvalAltResult::LoopBreak should not occur naturally"),
            Self::Return(_, _) => panic!("EvalAltResult::Return should not occur naturally"),
        }
    }
    /// Is this error a system exception?
    ///
    /// # Panics
    ///
    /// Panics when [`LoopBreak`][EvalAltResult::LoopBreak] or [`Return`][EvalAltResult::Return].
    pub fn is_system_exception(&self) -> bool {
        match self {
            Self::ErrorSystem(_, _) => true,
            Self::ErrorParsing(_, _) => true,

            Self::ErrorTooManyOperations(_)
            | Self::ErrorTooManyModules(_)
            | Self::ErrorStackOverflow(_)
            | Self::ErrorDataTooLarge(_, _) => true,

            Self::ErrorTerminated(_, _) => true,

            Self::LoopBreak(_, _) => panic!("EvalAltResult::LoopBreak should not occur naturally"),
            Self::Return(_, _) => panic!("EvalAltResult::Return should not occur naturally"),

            _ => false,
        }
    }
    /// Get the [position][Position] of this error.
    pub fn position(&self) -> Position {
        match self {
            Self::ErrorSystem(_, _) => Position::NONE,

            Self::ErrorParsing(_, pos)
            | Self::ErrorFunctionNotFound(_, pos)
            | Self::ErrorInFunctionCall(_, _, _, pos)
            | Self::ErrorInModule(_, _, pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorMismatchDataType(_, _, pos)
            | Self::ErrorArrayBounds(_, _, pos)
            | Self::ErrorStringBounds(_, _, pos)
            | Self::ErrorIndexingType(_, pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableNotFound(_, pos)
            | Self::ErrorModuleNotFound(_, pos)
            | Self::ErrorDataRace(_, pos)
            | Self::ErrorAssignmentToConstant(_, pos)
            | Self::ErrorMismatchOutputType(_, _, pos)
            | Self::ErrorInExpr(pos)
            | Self::ErrorDotExpr(_, pos)
            | Self::ErrorArithmetic(_, pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(_, pos)
            | Self::ErrorTerminated(_, pos)
            | Self::ErrorRuntime(_, pos)
            | Self::LoopBreak(_, pos)
            | Self::Return(_, pos) => *pos,
        }
    }
    /// Clear the [position][Position] information of this error.
    pub fn clear_position(&mut self) -> &mut Self {
        self.set_position(Position::NONE);
        self
    }
    /// Override the [position][Position] of this error.
    pub fn set_position(&mut self, new_position: Position) {
        match self {
            Self::ErrorSystem(_, _) => (),

            Self::ErrorParsing(_, pos)
            | Self::ErrorFunctionNotFound(_, pos)
            | Self::ErrorInFunctionCall(_, _, _, pos)
            | Self::ErrorInModule(_, _, pos)
            | Self::ErrorUnboundThis(pos)
            | Self::ErrorMismatchDataType(_, _, pos)
            | Self::ErrorArrayBounds(_, _, pos)
            | Self::ErrorStringBounds(_, _, pos)
            | Self::ErrorIndexingType(_, pos)
            | Self::ErrorFor(pos)
            | Self::ErrorVariableNotFound(_, pos)
            | Self::ErrorModuleNotFound(_, pos)
            | Self::ErrorDataRace(_, pos)
            | Self::ErrorAssignmentToConstant(_, pos)
            | Self::ErrorMismatchOutputType(_, _, pos)
            | Self::ErrorInExpr(pos)
            | Self::ErrorDotExpr(_, pos)
            | Self::ErrorArithmetic(_, pos)
            | Self::ErrorTooManyOperations(pos)
            | Self::ErrorTooManyModules(pos)
            | Self::ErrorStackOverflow(pos)
            | Self::ErrorDataTooLarge(_, pos)
            | Self::ErrorTerminated(_, pos)
            | Self::ErrorRuntime(_, pos)
            | Self::LoopBreak(_, pos)
            | Self::Return(_, pos) => *pos = new_position,
        }
    }
    /// Consume the current [`EvalAltResult`] and return a new one with the specified [`Position`]
    /// if the current position is [`Position::None`].
    #[inline(always)]
    pub(crate) fn fill_position(mut self: Box<Self>, new_position: Position) -> Box<Self> {
        if self.position().is_none() {
            self.set_position(new_position);
        }
        self
    }
}

impl<T> From<EvalAltResult> for Result<T, Box<EvalAltResult>> {
    #[inline(always)]
    fn from(err: EvalAltResult) -> Self {
        Err(err.into())
    }
}
