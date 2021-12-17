//! Module defining script identifiers.

use crate::{Identifier, Position};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{fmt, hash::Hash};

/// _(internals)_ An identifier containing a name and a [position][Position].
/// Exported under the `internals` feature only.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    /// Identifier name.
    pub name: Identifier,
    /// Position.
    pub pos: Position,
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.name)?;
        self.pos.debug_print(f)
    }
}

impl AsRef<str> for Ident {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        self.name.as_ref()
    }
}

impl Ident {
    #[inline(always)]
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}
