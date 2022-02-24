//! Module defining script-defined functions.
#![cfg(not(feature = "no_function"))]

use super::{FnAccess, StmtBlock};
use crate::{Identifier, StaticVec};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{fmt, hash::Hash};

/// _(internals)_ Encapsulated AST environment.
/// Exported under the `internals` feature only.
///
/// 1) other functions defined within the same AST
/// 2) the stack of imported [modules][crate::Module]
/// 3) global constants
///
/// Not available under `no_module` or `no_function`.
#[cfg(not(feature = "no_module"))]
#[cfg(not(feature = "no_function"))]
#[derive(Debug, Clone)]
pub struct EncapsulatedEnviron {
    /// Functions defined within the same [`AST`][crate::AST].
    pub lib: crate::Shared<crate::Module>,
    /// Imported [modules][crate::Module].
    pub imports: Box<[(Identifier, crate::Shared<crate::Module>)]>,
    /// Globally-defined constants.
    pub constants: Option<crate::eval::GlobalConstants>,
}

/// _(internals)_ A type containing information on a script-defined function.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone)]
pub struct ScriptFnDef {
    /// Function body.
    pub body: StmtBlock,
    /// Encapsulated AST environment, if any.
    ///
    /// Not available under `no_module` or `no_function`.
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    pub environ: Option<EncapsulatedEnviron>,
    /// Function name.
    pub name: Identifier,
    /// Function access mode.
    pub access: FnAccess,
    /// Names of function parameters.
    pub params: StaticVec<Identifier>,
    /// _(metadata)_ Function doc-comments (if any).
    /// Exported under the `metadata` feature only.
    #[cfg(feature = "metadata")]
    pub comments: Option<Box<[Box<str>]>>,
}

impl fmt::Display for ScriptFnDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}({})",
            match self.access {
                FnAccess::Public => "",
                FnAccess::Private => "private ",
            },
            self.name,
            self.params
                .iter()
                .map(|s| s.as_str())
                .collect::<StaticVec<_>>()
                .join(", ")
        )
    }
}

/// A type containing the metadata of a script-defined function.
///
/// Not available under `no_function`.
///
/// Created by [`AST::iter_functions`][super::AST::iter_functions].
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ScriptFnMetadata<'a> {
    /// _(metadata)_ Function doc-comments (if any).
    /// Exported under the `metadata` feature only.
    ///
    /// Block doc-comments are kept in a single string slice with line-breaks within.
    ///
    /// Line doc-comments are kept in one string slice per line without the termination line-break.
    ///
    /// Leading white-spaces are stripped, and each string slice always starts with the
    /// corresponding doc-comment leader: `///` or `/**`.
    #[cfg(feature = "metadata")]
    pub comments: Vec<&'a str>,
    /// Function access mode.
    pub access: FnAccess,
    /// Function name.
    pub name: &'a str,
    /// Function parameters (if any).
    pub params: Vec<&'a str>,
}

impl fmt::Display for ScriptFnMetadata<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}({})",
            match self.access {
                FnAccess::Public => "",
                FnAccess::Private => "private ",
            },
            self.name,
            self.params
                .iter()
                .cloned()
                .collect::<StaticVec<_>>()
                .join(", ")
        )
    }
}

impl<'a> From<&'a ScriptFnDef> for ScriptFnMetadata<'a> {
    #[inline]
    fn from(value: &'a ScriptFnDef) -> Self {
        Self {
            #[cfg(feature = "metadata")]
            comments: value
                .comments
                .as_ref()
                .map_or_else(|| Vec::new(), |v| v.iter().map(Box::as_ref).collect()),
            access: value.access,
            name: &value.name,
            params: value.params.iter().map(|s| s.as_str()).collect(),
        }
    }
}

impl std::cmp::PartialOrd for ScriptFnMetadata<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for ScriptFnMetadata<'_> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.name.cmp(other.name) {
            std::cmp::Ordering::Equal => self.params.len().cmp(&other.params.len()),
            cmp => cmp,
        }
    }
}
