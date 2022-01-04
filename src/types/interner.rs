#[cfg(not(feature = "no_object"))]
use crate::engine::{make_getter, make_setter, FN_GET, FN_SET};
use crate::{Identifier, ImmutableString};
use std::ops::AddAssign;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// _(internals)_ A factory of identifiers from text strings.
/// Exported under the `internals` feature only.
///
/// Normal identifiers, property getters and setters are interned separately.
#[derive(Debug, Clone, Default, Hash)]
pub struct StringsInterner {
    /// Normal strings.
    strings: std::collections::BTreeMap<Identifier, ImmutableString>,
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
            strings: std::collections::BTreeMap::new(),
            #[cfg(not(feature = "no_object"))]
            getters: std::collections::BTreeMap::new(),
            #[cfg(not(feature = "no_object"))]
            setters: std::collections::BTreeMap::new(),
        }
    }
    /// Get an identifier from a text string and prefix, adding it to the interner if necessary.
    ///
    /// # Prefix
    ///
    /// Currently recognized prefixes are:
    ///
    /// * `""` - None (normal string)
    /// * `"get$"` - Property getter, not available under `no_object`
    /// * `"set$"` - Property setter, not available under `no_object`
    ///
    /// # Panics
    ///
    /// Panics if the prefix is not recognized.
    #[inline]
    #[must_use]
    pub fn get(&mut self, prefix: impl AsRef<str>, text: impl AsRef<str>) -> ImmutableString {
        let (dict, mapper): (_, fn(&str) -> Identifier) = match prefix.as_ref() {
            "" => (&mut self.strings, |s| s.into()),

            #[cfg(not(feature = "no_object"))]
            FN_GET => (&mut self.getters, |s| make_getter(s)),
            #[cfg(not(feature = "no_object"))]
            FN_SET => (&mut self.setters, |s| make_setter(s)),

            _ => unreachable!("unsupported prefix {}", prefix.as_ref()),
        };

        if dict.contains_key(text.as_ref()) {
            dict.get(text.as_ref()).expect("exists").clone()
        } else {
            let value: ImmutableString = mapper(text.as_ref()).into();
            dict.insert(text.as_ref().into(), value.clone());
            value
        }
    }
}

impl AddAssign<Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.strings.extend(rhs.strings.into_iter());
        #[cfg(not(feature = "no_object"))]
        self.getters.extend(rhs.getters.into_iter());
        #[cfg(not(feature = "no_object"))]
        self.setters.extend(rhs.setters.into_iter());
    }
}

impl AddAssign<&Self> for StringsInterner {
    #[inline(always)]
    fn add_assign(&mut self, rhs: &Self) {
        self.strings
            .extend(rhs.strings.iter().map(|(k, v)| (k.clone(), v.clone())));
        #[cfg(not(feature = "no_object"))]
        self.getters
            .extend(rhs.getters.iter().map(|(k, v)| (k.clone(), v.clone())));
        #[cfg(not(feature = "no_object"))]
        self.setters
            .extend(rhs.setters.iter().map(|(k, v)| (k.clone(), v.clone())));
    }
}
