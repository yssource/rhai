//! Types to support chaining operations (i.e. indexing and dotting).
#![cfg(any(not(feature = "no_index"), not(feature = "no_object")))]

use crate::ast::Expr;
use crate::{Dynamic, Position};
use std::hash::Hash;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// Method of chaining.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum ChainType {
    /// Indexing.
    #[cfg(not(feature = "no_index"))]
    Indexing,
    /// Dotting.
    #[cfg(not(feature = "no_object"))]
    Dotting,
}

impl From<&Expr> for ChainType {
    #[inline]
    fn from(expr: &Expr) -> Self {
        match expr {
            #[cfg(not(feature = "no_index"))]
            Expr::Index(_, _, _) => Self::Indexing,
            #[cfg(not(feature = "no_object"))]
            Expr::Dot(_, _, _) => Self::Dotting,
            expr => unreachable!("Expr::Index or Expr::Dot expected but gets {:?}", expr),
        }
    }
}

/// Value of a chaining argument.
#[derive(Debug, Clone, Hash)]
pub enum ChainArgument {
    /// Dot-property access.
    #[cfg(not(feature = "no_object"))]
    Property(Position),
    /// Arguments to a dot method call.
    /// Wrapped values are the arguments plus the [position][Position] of the first argument.
    ///
    /// Since many dotted function calls have no arguments (e.g. `string.len()`), it is better to
    /// reduce the size of [`ChainArgument`] by using a boxed slice.
    #[cfg(not(feature = "no_object"))]
    MethodCallArgs(Option<Box<[Dynamic]>>, Position),
    /// Index value and [position][Position].
    #[cfg(not(feature = "no_index"))]
    IndexValue(Dynamic, Position),
}

impl ChainArgument {
    /// Return the index value.
    #[inline(always)]
    #[cfg(not(feature = "no_index"))]
    #[must_use]
    pub fn into_index_value(self) -> Option<Dynamic> {
        match self {
            Self::IndexValue(value, _) => Some(value),
            #[cfg(not(feature = "no_object"))]
            _ => None,
        }
    }
    /// Return the list of method call arguments.
    ///
    /// # Panics
    ///
    /// Panics if the [`ChainArgument`] is not [`MethodCallArgs`][ChainArgument::MethodCallArgs].
    #[inline(always)]
    #[cfg(not(feature = "no_object"))]
    #[must_use]
    pub fn into_fn_call_args(self) -> (crate::FnArgsVec<Dynamic>, Position) {
        match self {
            Self::MethodCallArgs(None, pos) => (crate::FnArgsVec::new_const(), pos),
            Self::MethodCallArgs(Some(mut values), pos) => {
                (values.iter_mut().map(std::mem::take).collect(), pos)
            }
            x => unreachable!("ChainArgument::MethodCallArgs expected but gets {:?}", x),
        }
    }
    /// Return the [position][Position].
    #[inline(always)]
    #[must_use]
    #[allow(dead_code)]
    pub const fn position(&self) -> Position {
        match self {
            #[cfg(not(feature = "no_object"))]
            ChainArgument::Property(pos) => *pos,
            #[cfg(not(feature = "no_object"))]
            ChainArgument::MethodCallArgs(_, pos) => *pos,
            #[cfg(not(feature = "no_index"))]
            ChainArgument::IndexValue(_, pos) => *pos,
        }
    }
    /// Create n [`MethodCallArgs`][ChainArgument::MethodCallArgs].
    #[inline(always)]
    #[cfg(not(feature = "no_object"))]
    #[must_use]
    pub fn from_fn_call_args(values: crate::FnArgsVec<Dynamic>, pos: Position) -> Self {
        if values.is_empty() {
            Self::MethodCallArgs(None, pos)
        } else {
            Self::MethodCallArgs(Some(values.into_vec().into()), pos)
        }
    }
    /// Create an [`IndexValue`][ChainArgument::IndexValue].
    #[inline(always)]
    #[cfg(not(feature = "no_index"))]
    #[must_use]
    pub const fn from_index_value(value: Dynamic, pos: Position) -> Self {
        Self::IndexValue(value, pos)
    }
}
