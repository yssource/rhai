//! Module containing various utility types and functions.
//!
//! # Safety
//!
//! The `StaticVec` type has some `unsafe` blocks.

use crate::r#unsafe::unsafe_uninit;

use crate::stdlib::{
    any::TypeId,
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
    mem,
    ops::Drop,
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

const MAX_STATIC_VEC: usize = 4;

/// A type to hold a number of values in static storage for speed, and any spill-overs in a `Vec`.
///
/// This is essentially a knock-off of the [`staticvec`](https://crates.io/crates/staticvec) crate.
/// This simplified implementation here is to avoid pulling in another crate.
///
/// # Safety
///
/// This type uses some unsafe code (mainly for uninitialized/unused array slots) for efficiency.
//
// TODO - remove unsafe code
pub struct StaticVec<T> {
    /// Total number of values held.
    len: usize,
    /// Static storage. 4 slots should be enough for most cases - i.e. four items of fast, no-allocation access.
    list: [mem::MaybeUninit<T>; MAX_STATIC_VEC],
    /// Dynamic storage. For spill-overs.
    more: Vec<T>,
}

impl<T> Drop for StaticVec<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

impl<T> Default for StaticVec<T> {
    fn default() -> Self {
        Self {
            len: 0,
            list: unsafe_uninit(),
            more: Vec::new(),
        }
    }
}

impl<T: PartialEq> PartialEq for StaticVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len
            //&& self.list[0..self.len] == other.list[0..self.len]
            && self.more == other.more
    }
}

impl<T: Clone> Clone for StaticVec<T> {
    fn clone(&self) -> Self {
        let mut value: Self = Default::default();
        value.len = self.len;

        if self.len <= self.list.len() {
            for x in 0..self.len {
                let item: &T = unsafe { mem::transmute(self.list.get(x).unwrap()) };
                value.list[x] = mem::MaybeUninit::new(item.clone());
            }
        } else {
            value.more = self.more.clone();
        }

        value
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

impl<T> StaticVec<T> {
    fn extract(value: mem::MaybeUninit<T>) -> T {
        unsafe { value.assume_init() }
    }
    /// Create a new `StaticVec`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Empty the `StaticVec`.
    pub fn clear(&mut self) {
        if self.len <= self.list.len() {
            for x in 0..self.len {
                Self::extract(mem::replace(
                    self.list.get_mut(x).unwrap(),
                    mem::MaybeUninit::uninit(),
                ));
            }
        } else {
            self.more.clear();
        }
        self.len = 0;
    }
    /// Push a new value to the end of this `StaticVec`.
    pub fn push<X: Into<T>>(&mut self, value: X) {
        if self.len == self.list.len() {
            // Move the fixed list to the Vec
            self.more.extend(
                self.list
                    .iter_mut()
                    .map(|v| mem::replace(v, mem::MaybeUninit::uninit()))
                    .map(Self::extract),
            );
            self.more.push(value.into());
        } else if self.len < self.list.len() {
            mem::replace(
                self.list.get_mut(self.len).unwrap(),
                mem::MaybeUninit::new(value.into()),
            );
        } else {
            self.more.push(value.into());
        }
        self.len += 1;
    }
    /// Insert a new value to this `StaticVec` at a particular position.
    pub fn insert<X: Into<T>>(&mut self, index: usize, value: X) {
        if index > self.len {
            panic!("index OOB in StaticVec");
        }

        if self.len == self.list.len() {
            // Move the fixed list to the Vec
            self.more.extend(
                self.list
                    .iter_mut()
                    .map(|v| mem::replace(v, mem::MaybeUninit::uninit()))
                    .map(Self::extract),
            );
            self.more.insert(index, value.into());
        } else if self.len < self.list.len() {
            for x in (index..self.len).rev() {
                let temp = mem::replace(self.list.get_mut(x).unwrap(), mem::MaybeUninit::uninit());
                mem::replace(self.list.get_mut(x + 1).unwrap(), temp);
            }
            mem::replace(
                self.list.get_mut(index).unwrap(),
                mem::MaybeUninit::new(value.into()),
            );
        } else {
            self.more.insert(index, value.into());
        }
        self.len += 1;
    }
    /// Pop a value from the end of this `StaticVec`.
    ///
    /// # Panics
    ///
    /// Panics if the `StaticVec` is empty.
    pub fn pop(&mut self) -> T {
        if self.len <= 0 {
            panic!("nothing to pop!");
        }

        let result = if self.len <= self.list.len() {
            Self::extract(mem::replace(
                self.list.get_mut(self.len - 1).unwrap(),
                mem::MaybeUninit::uninit(),
            ))
        } else {
            let r = self.more.pop().unwrap();

            // Move back to the fixed list
            if self.more.len() == self.list.len() {
                for index in (0..self.list.len()).rev() {
                    mem::replace(
                        self.list.get_mut(index).unwrap(),
                        mem::MaybeUninit::new(self.more.pop().unwrap()),
                    );
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
    /// Is this `StaticVec` empty?
    pub fn is_empty(&self) -> bool {
        self.len == 0
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

        let list: &[T; MAX_STATIC_VEC] = unsafe { mem::transmute(&self.list) };

        if self.len <= list.len() {
            list.get(index).unwrap()
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

        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.len <= list.len() {
            list.get_mut(index).unwrap()
        } else {
            self.more.get_mut(index).unwrap()
        }
    }
    /// Get an iterator to entries in the `StaticVec`.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        let list: &[T; MAX_STATIC_VEC] = unsafe { mem::transmute(&self.list) };

        if self.len <= list.len() {
            list[..self.len].iter()
        } else {
            self.more.iter()
        }
    }
    /// Get a mutable iterator to entries in the `StaticVec`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.len <= list.len() {
            list[..self.len].iter_mut()
        } else {
            self.more.iter_mut()
        }
    }
}

impl<T: Default> StaticVec<T> {
    /// Get the item at a particular index, replacing it with the default.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    pub fn get(&mut self, index: usize) -> T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        mem::take(if self.len <= self.list.len() {
            unsafe { mem::transmute(self.list.get_mut(index).unwrap()) }
        } else {
            self.more.get_mut(index).unwrap()
        })
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
        let list: &[T; MAX_STATIC_VEC] = unsafe { mem::transmute(&self.list) };

        if self.len <= list.len() {
            &list[..self.len]
        } else {
            &self.more[..]
        }
    }
}

impl<T> AsMut<[T]> for StaticVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.len <= list.len() {
            &mut list[..self.len]
        } else {
            &mut self.more[..]
        }
    }
}

impl<T> From<StaticVec<T>> for Vec<T> {
    fn from(mut value: StaticVec<T>) -> Self {
        if value.len <= value.list.len() {
            value
                .list
                .iter_mut()
                .map(|v| mem::replace(v, mem::MaybeUninit::uninit()))
                .map(StaticVec::extract)
                .collect()
        } else {
            let mut arr = Self::new();
            arr.append(&mut value.more);
            arr
        }
    }
}

impl<T> From<Vec<T>> for StaticVec<T> {
    fn from(mut value: Vec<T>) -> Self {
        let mut arr: Self = Default::default();
        arr.len = value.len();

        if arr.len <= arr.list.len() {
            for x in (0..arr.len).rev() {
                mem::replace(
                    arr.list.get_mut(x).unwrap(),
                    mem::MaybeUninit::new(value.pop().unwrap()),
                );
            }
        } else {
            arr.more = value;
        }

        arr
    }
}
