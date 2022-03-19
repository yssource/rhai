//! Collection of custom types.

use crate::Identifier;
use std::{any::type_name, collections::BTreeMap, fmt};

/// _(internals)_ A custom type.
/// Exported under the `internals` feature only.
pub type CustomType = Identifier;

/// _(internals)_ A collection of custom types.
/// Exported under the `internals` feature only.
#[derive(Clone, Hash, Default)]
pub struct CustomTypesCollection(BTreeMap<Identifier, CustomType>);

impl fmt::Debug for CustomTypesCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("CustomTypesCollection ")?;
        f.debug_map().entries(self.0.iter()).finish()
    }
}

impl CustomTypesCollection {
    /// Create a new [`CustomTypesCollection`].
    #[inline(always)]
    pub fn new() -> Self {
        Self(BTreeMap::new())
    }
    /// Register a custom type.
    #[inline(always)]
    pub fn add(&mut self, key: impl Into<Identifier>, name: impl Into<Identifier>) {
        self.0.insert(key.into(), name.into());
    }
    /// Register a custom type.
    #[inline(always)]
    pub fn add_type<T>(&mut self, name: &str) {
        self.0.insert(type_name::<T>().into(), name.into());
    }
    /// Find a custom type.
    #[inline(always)]
    pub fn get(&self, key: &str) -> Option<&str> {
        self.0.get(key).map(CustomType::as_str)
    }
}
