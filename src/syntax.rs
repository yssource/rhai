//! Module implementing custom syntax for `Engine`.

use crate::any::Dynamic;
use crate::engine::{Engine, EvalContext, MARKER_BLOCK, MARKER_EXPR, MARKER_IDENT};
use crate::error::{LexError, ParseError};
use crate::fn_native::{SendSync, Shared};
use crate::parser::Expr;
use crate::result::EvalAltResult;
use crate::token::{is_valid_identifier, Position, Token};
use crate::StaticVec;

use crate::stdlib::{
    boxed::Box,
    fmt, format,
    string::{String, ToString},
};

/// A general expression evaluation trait object.
#[cfg(not(feature = "sync"))]
pub type FnCustomSyntaxEval =
    dyn Fn(&mut EvalContext, &[Expression]) -> Result<Dynamic, Box<EvalAltResult>>;
/// A general expression evaluation trait object.
#[cfg(feature = "sync")]
pub type FnCustomSyntaxEval =
    dyn Fn(&mut EvalContext, &[Expression]) -> Result<Dynamic, Box<EvalAltResult>> + Send + Sync;

/// An expression sub-tree in an AST.
#[derive(Debug, Clone, Hash)]
pub struct Expression<'a>(&'a Expr);

impl<'a> From<&'a Expr> for Expression<'a> {
    #[inline(always)]
    fn from(expr: &'a Expr) -> Self {
        Self(expr)
    }
}

impl Expression<'_> {
    /// If this expression is a variable name, return it.  Otherwise `None`.
    #[inline(always)]
    pub fn get_variable_name(&self) -> Option<&str> {
        self.0.get_variable_access(true)
    }
    /// Get the expression.
    #[inline(always)]
    pub(crate) fn expr(&self) -> &Expr {
        &self.0
    }
    /// Get the position of this expression.
    #[inline(always)]
    pub fn position(&self) -> Position {
        self.0.position()
    }
}

impl EvalContext<'_, '_, '_, '_, '_, '_, '_, '_> {
    /// Evaluate an expression tree.
    ///
    /// ## WARNING - Low Level API
    ///
    /// This function is very low level.  It evaluates an expression from an AST.
    #[inline(always)]
    pub fn eval_expression_tree(
        &mut self,
        expr: &Expression,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        self.engine().eval_expr(
            self.scope,
            self.mods,
            self.state,
            self.namespace(),
            self.this_ptr,
            expr.expr(),
            self.call_level(),
        )
    }
}

#[derive(Clone)]
pub struct CustomSyntax {
    pub segments: StaticVec<String>,
    pub func: Shared<FnCustomSyntaxEval>,
    pub scope_delta: isize,
}

impl fmt::Debug for CustomSyntax {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.segments, f)
    }
}

impl Engine {
    /// Register a custom syntax with the `Engine`.
    ///
    /// * `keywords` holds a slice of strings that define the custom syntax.  
    /// * `new_vars` is the number of new variables declared by this custom syntax, or the number of variables removed (if negative).  
    /// * `func` is the implementation function.
    pub fn register_custom_syntax<S: AsRef<str> + ToString>(
        &mut self,
        keywords: impl AsRef<[S]>,
        new_vars: isize,
        func: impl Fn(&mut EvalContext, &[Expression]) -> Result<Dynamic, Box<EvalAltResult>>
            + SendSync
            + 'static,
    ) -> Result<&mut Self, ParseError> {
        let keywords = keywords.as_ref();

        let mut segments: StaticVec<_> = Default::default();

        for s in keywords {
            let s = s.as_ref().trim();

            // skip empty keywords
            if s.is_empty() {
                continue;
            }

            let seg = match s {
                // Markers not in first position
                MARKER_EXPR | MARKER_BLOCK | MARKER_IDENT if !segments.is_empty() => s.to_string(),
                // Standard symbols not in first position
                s if !segments.is_empty() && Token::lookup_from_syntax(s).is_some() => {
                    // Make it a custom keyword/operator if it is a disabled standard keyword/operator
                    // or a reserved keyword/operator.
                    if self
                        .disabled_symbols
                        .as_ref()
                        .map(|d| d.contains(s))
                        .unwrap_or(false)
                        || Token::lookup_from_syntax(s)
                            .map(|token| token.is_reserved())
                            .unwrap_or(false)
                    {
                        // If symbol is disabled, make it a custom keyword
                        if self.custom_keywords.is_none() {
                            self.custom_keywords = Some(Default::default());
                        }

                        if !self.custom_keywords.as_ref().unwrap().contains_key(s) {
                            self.custom_keywords.as_mut().unwrap().insert(s.into(), 0);
                        }
                    }

                    s.into()
                }
                // Identifier
                s if is_valid_identifier(s.chars()) => {
                    if self.custom_keywords.is_none() {
                        self.custom_keywords = Some(Default::default());
                    }

                    if !self.custom_keywords.as_ref().unwrap().contains_key(s) {
                        self.custom_keywords.as_mut().unwrap().insert(s.into(), 0);
                    }

                    s.into()
                }
                // Anything else is an error
                _ => {
                    return Err(LexError::ImproperSymbol(format!(
                        "Improper symbol for custom syntax: '{}'",
                        s
                    ))
                    .into_err(Position::none())
                    .into());
                }
            };

            segments.push(seg);
        }

        // If the syntax has no keywords, just ignore the registration
        if segments.is_empty() {
            return Ok(self);
        }

        // Remove the first keyword as the discriminator
        let key = segments.remove(0);

        let syntax = CustomSyntax {
            segments,
            func: (Box::new(func) as Box<FnCustomSyntaxEval>).into(),
            scope_delta: new_vars,
        };

        if self.custom_syntax.is_none() {
            self.custom_syntax = Some(Default::default());
        }

        self.custom_syntax
            .as_mut()
            .unwrap()
            .insert(key, syntax.into());

        Ok(self)
    }
}
