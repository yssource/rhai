#[cfg(not(feature = "no_object"))]
use crate::engine::{make_getter, make_setter, FN_GET, FN_SET};
use crate::{Identifier, ImmutableString};
use std::ops::AddAssign;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// _(internals)_ A factory of identifiers from text strings.
/// Exported under the `internals` feature only.
///
/// Normal identifiers are not interned since they use `SmartString` because most identifiers in
/// Rhai are short and ASCII-based. Thus copying is relatively fast.
///
/// Property getters and setters are interned separately.
#[derive(Debug, Clone, Default, Hash)]
pub struct StringsInterner {
    /// Property getters.
    #[cfg(not(feature = "no_object"))]
    getters: std::collections::BTreeMap<Identifier, ImmutableString>,
    /// Property setters.
    #[cfg(not(feature = "no_object"))]
    setters: std::collections::BTreeMap<Identifier, ImmutableString>,
}

impl StringsInterner {
    /// Create a new [`StringsInterner`].
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            #[cfg(not(feature = "no_object"))]
            getters: std::collections::BTreeMap::new(),
            #[cfg(not(feature = "no_object"))]
            setters: std::collections::BTreeMap::new(),
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
        #[cfg(not(feature = "no_object"))]
        {
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

        #[cfg(feature = "no_object")]
        match prefix {
            "" => return text.into(),
            _ => unreachable!("unsupported prefix {}", prefix),
        }
    }
}

impl AddAssign<Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        let _rhs = rhs;

        #[cfg(not(feature = "no_object"))]
        {
            self.getters.extend(_rhs.getters.into_iter());
            self.setters.extend(_rhs.setters.into_iter());
        }
    }
}

impl AddAssign<&Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: &Self) {
        let _rhs = rhs;

        #[cfg(not(feature = "no_object"))]
        {
            self.getters
                .extend(_rhs.getters.iter().map(|(k, v)| (k.clone(), v.clone())));
            self.setters
                .extend(_rhs.setters.iter().map(|(k, v)| (k.clone(), v.clone())));
        }
    }
}
