use crate::engine::{make_getter, make_setter, FN_GET, FN_SET};
use crate::{Identifier, ImmutableString};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{collections::BTreeMap, ops::AddAssign};

/// _(internals)_ A factory of identifiers from text strings.
/// Exported under the `internals` feature only.
///
/// Since [`SmartString`](https://crates.io/crates/smartstring) is used as [`Identifier`],
/// this just returns a copy because most identifiers in Rhai are short and ASCII-based.
///
/// Property getters and setters are interned separately.
#[derive(Debug, Clone, Default, Hash)]
pub struct StringsInterner {
    /// Property getters.
    getters: BTreeMap<Identifier, ImmutableString>,
    /// Property setters.
    setters: BTreeMap<Identifier, ImmutableString>,
}

impl StringsInterner {
    /// Create a new [`IdentifierBuilder`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            getters: BTreeMap::new(),
            setters: BTreeMap::new(),
        }
    }
    /// Get an identifier from a text string and prefix, adding it to the interner if necessary.
    ///
    /// # Panics
    ///
    /// Panics if the prefix is not recognized.
    #[inline]
    #[must_use]
    pub fn get(
        &mut self,
        prefix: &'static str,
        text: impl AsRef<str> + Into<Identifier> + Into<ImmutableString>,
    ) -> ImmutableString {
        let (dict, mapper) = match prefix {
            "" => return text.into(),
            FN_GET => (&mut self.getters, make_getter as fn(&str) -> String),
            FN_SET => (&mut self.setters, make_setter as fn(&str) -> String),
            _ => unreachable!("unsupported prefix {}", prefix),
        };

        if dict.contains_key(text.as_ref()) {
            self.getters.get(text.as_ref()).expect("exists").clone()
        } else {
            let value: ImmutableString = mapper(text.as_ref()).into();
            let text = text.into();
            dict.insert(text, value.clone());
            value
        }
    }
    /// Merge another [`IdentifierBuilder`] into this.
    #[inline(always)]
    pub fn merge(&mut self, _other: &Self) {}
}

impl AddAssign for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.getters.extend(rhs.getters.into_iter());
        self.setters.extend(rhs.setters.into_iter());
    }
}
