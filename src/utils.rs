//! Module containing various utility types and functions.

use crate::stdlib::{
    any::TypeId,
    fmt,
    hash::{Hash, Hasher},
    mem,
    vec::Vec,
};

#[cfg(not(feature = "no_std"))]
use crate::stdlib::collections::hash_map::DefaultHasher;

#[cfg(feature = "no_std")]
use ahash::AHasher;

/// Calculate a `u64` hash key from a module-qualified function name and parameter types.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via `TypeId` values from an iterator.
///
/// ### Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
pub fn calc_fn_spec<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    params: impl Iterator<Item = TypeId>,
) -> u64 {
    #[cfg(feature = "no_std")]
    let mut s: AHasher = Default::default();
    #[cfg(not(feature = "no_std"))]
    let mut s = DefaultHasher::new();

    // We always skip the first module
    modules.skip(1).for_each(|m| m.hash(&mut s));
    s.write(fn_name.as_bytes());
    params.for_each(|t| t.hash(&mut s));
    s.finish()
}

/// Calculate a `u64` hash key from a function name and number of parameters (without regard to types).
pub(crate) fn calc_fn_def(fn_name: &str, num_params: usize) -> u64 {
    #[cfg(feature = "no_std")]
    let mut s: AHasher = Default::default();
    #[cfg(not(feature = "no_std"))]
    let mut s = DefaultHasher::new();

    s.write(fn_name.as_bytes());
    s.write_usize(num_params);
    s.finish()
}

/// A type to hold a number of values in static storage for speed, and any spill-overs in a `Vec`.
///
/// This is essentially a knock-off of the [`staticvec`](https://crates.io/crates/staticvec) crate.
/// This simplified implementation here is to avoid pulling in another crate.
#[derive(Clone, Hash, Default)]
pub struct StaticVec<T: Default + Clone> {
    /// Total number of values held.
    len: usize,
    /// Static storage. 4 slots should be enough for most cases - i.e. four levels of indirection.
    list: [T; 4],
    /// Dynamic storage. For spill-overs.
    more: Vec<T>,
}

impl<T: Default + Clone> StaticVec<T> {
    /// Create a new `StaticVec`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Push a new value to the end of this `StaticVec`.
    pub fn push<X: Into<T>>(&mut self, value: X) {
        if self.len >= self.list.len() {
            self.more.push(value.into());
        } else {
            self.list[self.len] = value.into();
        }
        self.len += 1;
    }
    /// Pop a value from the end of this `StaticVec`.
    ///
    /// # Panics
    ///
    /// Panics if the `StaticVec` is empty.
    pub fn pop(&mut self) -> T {
        let result = if self.len <= 0 {
            panic!("nothing to pop!")
        } else if self.len <= self.list.len() {
            mem::take(self.list.get_mut(self.len - 1).unwrap())
        } else {
            self.more.pop().unwrap()
        };

        self.len -= 1;

        result
    }
    /// Get the number of items in this `StaticVec`.
    pub fn len(&self) -> usize {
        self.len
    }
    /// Get an item at a particular index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    pub fn get(&self, index: usize) -> &T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        if index < self.list.len() {
            self.list.get(index).unwrap()
        } else {
            self.more.get(index - self.list.len()).unwrap()
        }
    }
    /// Get an iterator to entries in the `StaticVec`.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        let num = if self.len >= self.list.len() {
            self.list.len()
        } else {
            self.len
        };

        self.list[..num].iter().chain(self.more.iter())
    }
    /// Get a mutable iterator to entries in the `StaticVec`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        let num = if self.len >= self.list.len() {
            self.list.len()
        } else {
            self.len
        };

        self.list[..num].iter_mut().chain(self.more.iter_mut())
    }
}

impl<T: Default + Clone + fmt::Debug> fmt::Debug for StaticVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ ")?;
        self.iter().try_for_each(|v| write!(f, "{:?}, ", v))?;
        write!(f, "]")
    }
}
