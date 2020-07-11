//! Module containing implementation for custom syntax.
#![cfg(feature = "internals")]

use crate::any::Dynamic;
use crate::engine::{Engine, Expression, Imports, State, MARKER_BLOCK, MARKER_EXPR, MARKER_IDENT};
use crate::error::LexError;
use crate::fn_native::{SendSync, Shared};
use crate::module::Module;
use crate::result::EvalAltResult;
use crate::scope::Scope;
use crate::token::{is_valid_identifier, Token};
use crate::utils::StaticVec;

use crate::stdlib::{
    fmt,
    rc::Rc,
    string::{String, ToString},
    sync::Arc,
};

/// A general function trail object.
#[cfg(not(feature = "sync"))]
pub type FnCustomSyntaxEval = dyn Fn(
    &Engine,
    &mut Scope,
    &mut Imports,
    &mut State,
    &Module,
    &mut Option<&mut Dynamic>,
    &[Expression],
    usize,
) -> Result<Dynamic, Box<EvalAltResult>>;
/// A general function trail object.
#[cfg(feature = "sync")]
pub type FnCustomSyntaxEval = dyn Fn(
        &Engine,
        &mut Scope,
        &mut Imports,
        &mut State,
        &Module,
        &mut Option<&mut Dynamic>,
        &[Expr],
        usize,
    ) -> Result<Dynamic, Box<EvalAltResult>>
    + Send
    + Sync;

#[derive(Clone)]
pub struct CustomSyntax {
    pub segments: StaticVec<String>,
    pub func: Shared<FnCustomSyntaxEval>,
    pub scope_delta: isize,
}

impl fmt::Debug for CustomSyntax {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.segments, f)
    }
}

impl Engine {
    pub fn register_custom_syntax<S: AsRef<str> + ToString>(
        &mut self,
        value: &[S],
        scope_delta: isize,
        func: impl Fn(
                &Engine,
                &mut Scope,
                &mut Imports,
                &mut State,
                &Module,
                &mut Option<&mut Dynamic>,
                &[Expression],
                usize,
            ) -> Result<Dynamic, Box<EvalAltResult>>
            + SendSync
            + 'static,
    ) -> Result<(), Box<LexError>> {
        if value.is_empty() {
            return Err(Box::new(LexError::ImproperSymbol("".to_string())));
        }

        let mut segments: StaticVec<_> = Default::default();

        for s in value {
            let seg = match s.as_ref() {
                // Markers not in first position
                MARKER_EXPR | MARKER_BLOCK | MARKER_IDENT if !segments.is_empty() => s.to_string(),
                // Standard symbols not in first position
                s if !segments.is_empty() && Token::lookup_from_syntax(s).is_some() => {
                    if self
                        .disabled_symbols
                        .as_ref()
                        .map(|d| d.contains(s))
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
                _ => return Err(Box::new(LexError::ImproperSymbol(s.to_string()))),
            };

            segments.push(seg);
        }

        let key = segments.remove(0);

        let syntax = CustomSyntax {
            segments,
            #[cfg(not(feature = "sync"))]
            func: Rc::new(func),
            #[cfg(feature = "sync")]
            func: Arc::new(func),
            scope_delta,
        };

        if self.custom_syntax.is_none() {
            self.custom_syntax = Some(Default::default());
        }

        self.custom_syntax
            .as_mut()
            .unwrap()
            .insert(key, syntax.into());

        Ok(())
    }
}
