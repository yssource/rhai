//! Module containing various utility types and functions.

use crate::r#unsafe::unsafe_zeroed;

use crate::stdlib::{
    any::TypeId,
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
    mem,
    vec::Vec,
};

#[cfg(not(feature = "no_std"))]
use crate::stdlib::collections::hash_map::DefaultHasher;

#[cfg(feature = "no_std")]
use ahash::AHasher;

#[inline(always)]
pub fn EMPTY_TYPE_ID() -> TypeId {
    TypeId::of::<()>()
}

/// Calculate a `u64` hash key from a module-qualified function name and parameter types.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via `TypeId` values from an iterator.
///
/// # Note
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

/// A type to hold a number of values in static storage for speed, and any spill-overs in a `Vec`.
///
/// This is essentially a knock-off of the [`staticvec`](https://crates.io/crates/staticvec) crate.
/// This simplified implementation here is to avoid pulling in another crate.
///
/// # Safety
///
/// This type uses some unsafe code (mainly to zero out unused array slots) for efficiency.
//
// TODO - remove unsafe code
#[derive(Clone, Hash)]
pub struct StaticVec<T> {
    /// Total number of values held.
    len: usize,
    /// Static storage. 4 slots should be enough for most cases - i.e. four levels of indirection.
    list: [T; 4],
    /// Dynamic storage. For spill-overs.
    more: Vec<T>,
}

impl<T: PartialEq> PartialEq for StaticVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len && self.list == other.list && self.more == other.more
    }
}

impl<T: Eq> Eq for StaticVec<T> {}

impl<T> FromIterator<T> for StaticVec<T> {
    fn from_iter<X: IntoIterator<Item = T>>(iter: X) -> Self {
        let mut vec = StaticVec::new();

        for x in iter {
            vec.push(x);
        }

        vec
    }
}

impl<T> Default for StaticVec<T> {
    fn default() -> Self {
        Self {
            len: 0,
            list: unsafe_zeroed(),
            more: Vec::new(),
        }
    }
}

impl<T> StaticVec<T> {
    /// Create a new `StaticVec`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Push a new value to the end of this `StaticVec`.
    pub fn push<X: Into<T>>(&mut self, value: X) {
        if self.len == self.list.len() {
            // Move the fixed list to the Vec
            for x in 0..self.list.len() {
                let def_val: T = unsafe_zeroed();
                self.more
                    .push(mem::replace(self.list.get_mut(x).unwrap(), def_val));
            }
            self.more.push(value.into());
        } else if self.len > self.list.len() {
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
            let def_val: T = unsafe_zeroed();
            mem::replace(self.list.get_mut(self.len - 1).unwrap(), def_val)
        } else {
            let r = self.more.pop().unwrap();

            // Move back to the fixed list
            if self.more.len() == self.list.len() {
                for x in 0..self.list.len() {
                    self.list[self.list.len() - 1 - x] = self.more.pop().unwrap();
                }
            }

            r
        };

        self.len -= 1;

        result
    }
    /// Get the number of items in this `StaticVec`.
    pub fn len(&self) -> usize {
        self.len
    }
    /// Get a reference to the item at a particular index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    pub fn get_ref(&self, index: usize) -> &T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        if self.len < self.list.len() {
            self.list.get(index).unwrap()
        } else {
            self.more.get(index).unwrap()
        }
    }
    /// Get a mutable reference to the item at a particular index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        if self.len < self.list.len() {
            self.list.get_mut(index).unwrap()
        } else {
            self.more.get_mut(index).unwrap()
        }
    }
    /// Get an iterator to entries in the `StaticVec`.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        if self.len > self.list.len() {
            self.more.iter()
        } else {
            self.list[..self.len].iter()
        }
    }
    /// Get a mutable iterator to entries in the `StaticVec`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        if self.len > self.list.len() {
            self.more.iter_mut()
        } else {
            self.list[..self.len].iter_mut()
        }
    }
}

impl<T: Copy> StaticVec<T> {
    /// Get the item at a particular index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    pub fn get(&self, index: usize) -> T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        if self.len < self.list.len() {
            *self.list.get(index).unwrap()
        } else {
            *self.more.get(index).unwrap()
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for StaticVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[ ")?;
        self.iter().try_for_each(|v| write!(f, "{:?}, ", v))?;
        write!(f, "]")
    }
}

impl<T> AsRef<[T]> for StaticVec<T> {
    fn as_ref(&self) -> &[T] {
        if self.len > self.list.len() {
            &self.more[..]
        } else {
            &self.list[..self.len]
        }
    }
}

impl<T> AsMut<[T]> for StaticVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        if self.len > self.list.len() {
            &mut self.more[..]
        } else {
            &mut self.list[..self.len]
        }
    }
}
