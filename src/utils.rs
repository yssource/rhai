//! Module containing various utility types and functions.
//!
//! # Safety
//!
//! The `StaticVec` type has some `unsafe` blocks to handle conversions between `MaybeUninit` and regular types.

use crate::stdlib::{
    any::TypeId,
    fmt,
    hash::{Hash, Hasher},
    iter::FromIterator,
    mem,
    mem::MaybeUninit,
    ops::{Drop, Index, IndexMut},
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
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
pub fn calc_fn_spec<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    num: usize,
    params: impl Iterator<Item = TypeId>,
) -> u64 {
    #[cfg(feature = "no_std")]
    let mut s: AHasher = Default::default();
    #[cfg(not(feature = "no_std"))]
    let mut s = DefaultHasher::new();

    // We always skip the first module
    modules.skip(1).for_each(|m| m.hash(&mut s));
    s.write(fn_name.as_bytes());
    s.write_usize(num);
    params.for_each(|t| t.hash(&mut s));
    s.finish()
}

/// A type to hold a number of values in static storage for no-allocation, quick access.
/// If too many items are stored, it converts into using a `Vec`.
///
/// This is essentially a knock-off of the [`staticvec`](https://crates.io/crates/staticvec) crate.
/// This simplified implementation here is to avoid pulling in another crate.
///
/// # Implementation
///
/// A `StaticVec` holds data in _either one_ of two storages: 1) a fixed-size array of `MAX_STATIC_VEC`
/// items, and 2) a dynamic `Vec`.  At any time, either one of them (or both) must be empty, depending on the
/// total number of items.
///
/// There is a `len` field containing the total number of items held by the `StaticVec`.
///
/// The fixed-size array (`list`) is not initialized (i.e. initialized with `MaybeUninit::uninit()`).
///
/// When `len <= MAX_STATIC_VEC`, all elements are stored in the fixed-size array.
/// Array slots `>= len` are `MaybeUninit::uninit()` while slots `< len` are considered actual data.
/// In this scenario, the `Vec` (`more`) is empty.
///
/// As soon as we try to push a new item into the `StaticVec` that makes the total number exceed
/// `MAX_STATIC_VEC`, all the items in the fixed-sized array are taken out, replaced with
/// `MaybeUninit::uninit()` (via `mem::replace`) and pushed into the `Vec`.
/// Then the new item is added to the `Vec`.
///
/// Therefore, if `len > MAX_STATIC_VEC`, then the fixed-size array (`list`) is considered
/// empty and uninitialized while all data resides in the `Vec` (`more`).
///
/// When popping an item off of the `StaticVec`, the reverse is true.  When `len = MAX_STATIC_VEC + 1`,
/// after popping the item, all the items residing in the `Vec` are moved back to the fixed-size array (`list`).
/// The `Vec` will then be empty.
///
/// Therefore, if `len <= MAX_STATIC_VEC`, data is in the fixed-size array (`list`).
/// Otherwise, data is in the `Vec` (`more`).
///
/// # Safety
///
/// This type uses some unsafe code (mainly for uninitialized/unused array slots) for efficiency.
//
// TODO - remove unsafe code
pub struct StaticVec<T> {
    /// Total number of values held.
    len: usize,
    /// Fixed-size storage for fast, no-allocation access.
    list: [MaybeUninit<T>; MAX_STATIC_VEC],
    /// Dynamic storage. For spill-overs.
    more: Vec<T>,
}

/// Maximum slots of fixed-size storage for a `StaticVec`.
/// 4 slots should be enough for most cases.
const MAX_STATIC_VEC: usize = 4;

impl<T> Drop for StaticVec<T> {
    fn drop(&mut self) {
        self.clear();
    }
}

impl<T> Default for StaticVec<T> {
    fn default() -> Self {
        Self {
            len: 0,
            list: unsafe { mem::MaybeUninit::uninit().assume_init() },
            more: Vec::new(),
        }
    }
}

impl<T: PartialEq> PartialEq for StaticVec<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len || self.more != other.more {
            return false;
        }

        if self.len > MAX_STATIC_VEC {
            return true;
        }

        unsafe {
            mem::transmute::<_, &[T; MAX_STATIC_VEC]>(&self.list)
                == mem::transmute::<_, &[T; MAX_STATIC_VEC]>(&other.list)
        }
    }
}

impl<T: Clone> Clone for StaticVec<T> {
    fn clone(&self) -> Self {
        let mut value: Self = Default::default();
        value.len = self.len;

        if self.is_fixed_storage() {
            for x in 0..self.len {
                let item: &T = unsafe { mem::transmute(self.list.get(x).unwrap()) };
                value.list[x] = MaybeUninit::new(item.clone());
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
    /// Create a new `StaticVec`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Empty the `StaticVec`.
    pub fn clear(&mut self) {
        if self.is_fixed_storage() {
            for x in 0..self.len {
                self.extract_from_list(x);
            }
        } else {
            self.more.clear();
        }
        self.len = 0;
    }
    /// Extract a `MaybeUninit` into a concrete initialized type.
    fn extract(value: MaybeUninit<T>) -> T {
        unsafe { value.assume_init() }
    }
    /// Extract an item from the fixed-size array, replacing it with `MaybeUninit::uninit()`.
    ///
    /// # Panics
    ///
    /// Panics if fixed-size storage is not used, or if the `index` is out of bounds.
    fn extract_from_list(&mut self, index: usize) -> T {
        if !self.is_fixed_storage() {
            panic!("not fixed storage in StaticVec");
        }
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }
        Self::extract(mem::replace(
            self.list.get_mut(index).unwrap(),
            MaybeUninit::uninit(),
        ))
    }
    /// Set an item into the fixed-size array.
    /// If `drop` is `true`, the original value is extracted then automatically dropped.
    ///
    /// # Panics
    ///
    /// Panics if fixed-size storage is not used, or if the `index` is out of bounds.
    fn set_into_list(&mut self, index: usize, value: T, drop: bool) {
        if !self.is_fixed_storage() {
            panic!("not fixed storage in StaticVec");
        }
        // Allow setting at most one slot to the right
        if index > self.len {
            panic!("index OOB in StaticVec");
        }
        let temp = mem::replace(self.list.get_mut(index).unwrap(), MaybeUninit::new(value));
        if drop {
            // Extract the original value - which will drop it automatically
            Self::extract(temp);
        }
    }
    /// Move item in the fixed-size array into the `Vec`.
    ///
    /// # Panics
    ///
    /// Panics if fixed-size storage is not used, or if the fixed-size storage is not full.
    fn move_fixed_into_vec(&mut self, num: usize) {
        if !self.is_fixed_storage() {
            panic!("not fixed storage in StaticVec");
        }
        if self.len != num {
            panic!("fixed storage is not full in StaticVec");
        }
        self.more.extend(
            self.list
                .iter_mut()
                .take(num)
                .map(|v| mem::replace(v, MaybeUninit::uninit()))
                .map(Self::extract),
        );
    }
    /// Is data stored in fixed-size storage?
    fn is_fixed_storage(&self) -> bool {
        self.len <= MAX_STATIC_VEC
    }
    /// Push a new value to the end of this `StaticVec`.
    pub fn push<X: Into<T>>(&mut self, value: X) {
        if self.len == MAX_STATIC_VEC {
            self.move_fixed_into_vec(MAX_STATIC_VEC);
            self.more.push(value.into());
        } else if self.is_fixed_storage() {
            self.set_into_list(self.len, value.into(), false);
        } else {
            self.more.push(value.into());
        }
        self.len += 1;
    }
    /// Insert a new value to this `StaticVec` at a particular position.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn insert<X: Into<T>>(&mut self, index: usize, value: X) {
        if index > self.len {
            panic!("index OOB in StaticVec");
        }

        if self.len == MAX_STATIC_VEC {
            self.move_fixed_into_vec(MAX_STATIC_VEC);
            self.more.insert(index, value.into());
        } else if self.is_fixed_storage() {
            // Move all items one slot to the right
            for x in (index..self.len).rev() {
                let orig_value = self.extract_from_list(x);
                self.set_into_list(x + 1, orig_value, false);
            }
            self.set_into_list(index, value.into(), false);
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
        if self.is_empty() {
            panic!("nothing to pop!");
        }

        let result = if self.is_fixed_storage() {
            self.extract_from_list(self.len - 1)
        } else {
            let value = self.more.pop().unwrap();

            // Move back to the fixed list
            if self.more.len() == MAX_STATIC_VEC {
                for index in (0..MAX_STATIC_VEC).rev() {
                    let item = self.more.pop().unwrap();
                    self.set_into_list(index, item, false);
                }
            }

            value
        };

        self.len -= 1;

        result
    }
    /// Remove a value from this `StaticVec` at a particular position.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn remove(&mut self, index: usize) -> T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        let result = if self.is_fixed_storage() {
            let value = self.extract_from_list(index);

            // Move all items one slot to the left
            for x in index..self.len - 1 {
                let orig_value = self.extract_from_list(x + 1);
                self.set_into_list(x, orig_value, false);
            }

            value
        } else {
            let value = self.more.remove(index);

            // Move back to the fixed list
            if self.more.len() == MAX_STATIC_VEC {
                for index in (0..MAX_STATIC_VEC).rev() {
                    let item = self.more.pop().unwrap();
                    self.set_into_list(index, item, false);
                }
            }

            value
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
    /// Panics if `index` is out of bounds.
    pub fn get(&self, index: usize) -> &T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        let list: &[T; MAX_STATIC_VEC] = unsafe { mem::transmute(&self.list) };

        if self.is_fixed_storage() {
            list.get(index).unwrap()
        } else {
            self.more.get(index).unwrap()
        }
    }
    /// Get a mutable reference to the item at a particular index.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.is_fixed_storage() {
            list.get_mut(index).unwrap()
        } else {
            self.more.get_mut(index).unwrap()
        }
    }
    /// Get an iterator to entries in the `StaticVec`.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        let list: &[T; MAX_STATIC_VEC] = unsafe { mem::transmute(&self.list) };

        if self.is_fixed_storage() {
            list[..self.len].iter()
        } else {
            self.more.iter()
        }
    }
    /// Get a mutable iterator to entries in the `StaticVec`.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.is_fixed_storage() {
            list[..self.len].iter_mut()
        } else {
            self.more.iter_mut()
        }
    }
}

impl<T: 'static> StaticVec<T> {
    /// Get a mutable iterator to entries in the `StaticVec`.
    pub fn into_iter(mut self) -> Box<dyn Iterator<Item = T>> {
        if self.is_fixed_storage() {
            let mut it = FixedStorageIterator {
                data: unsafe { mem::MaybeUninit::uninit().assume_init() },
                index: 0,
                limit: self.len,
            };

            for x in 0..self.len {
                it.data[x] = mem::replace(self.list.get_mut(x).unwrap(), MaybeUninit::uninit());
            }
            self.len = 0;

            Box::new(it)
        } else {
            Box::new(Vec::from(self).into_iter())
        }
    }
}

/// An iterator that takes control of the fixed-size storage of a `StaticVec` and returns its values.
struct FixedStorageIterator<T> {
    data: [MaybeUninit<T>; MAX_STATIC_VEC],
    index: usize,
    limit: usize,
}

impl<T> Iterator for FixedStorageIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.limit {
            None
        } else {
            self.index += 1;

            let value = mem::replace(
                self.data.get_mut(self.index - 1).unwrap(),
                MaybeUninit::uninit(),
            );

            unsafe { Some(value.assume_init()) }
        }
    }
}

impl<T: Default> StaticVec<T> {
    /// Get the item at a particular index, replacing it with the default.
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn take(&mut self, index: usize) -> T {
        if index >= self.len {
            panic!("index OOB in StaticVec");
        }

        mem::take(if self.is_fixed_storage() {
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

        if self.is_fixed_storage() {
            &list[..self.len]
        } else {
            &self.more[..]
        }
    }
}

impl<T> AsMut<[T]> for StaticVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        let list: &mut [T; MAX_STATIC_VEC] = unsafe { mem::transmute(&mut self.list) };

        if self.is_fixed_storage() {
            &mut list[..self.len]
        } else {
            &mut self.more[..]
        }
    }
}

impl<T> Index<usize> for StaticVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index)
    }
}

impl<T> IndexMut<usize> for StaticVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index)
    }
}

impl<T> From<StaticVec<T>> for Vec<T> {
    fn from(mut value: StaticVec<T>) -> Self {
        if value.len <= MAX_STATIC_VEC {
            value.move_fixed_into_vec(value.len);
        }
        value.len = 0;

        let mut arr = Self::new();
        arr.append(&mut value.more);
        arr
    }
}

impl<T> From<Vec<T>> for StaticVec<T> {
    fn from(mut value: Vec<T>) -> Self {
        let mut arr: Self = Default::default();
        arr.len = value.len();

        if arr.len <= MAX_STATIC_VEC {
            for x in (0..arr.len).rev() {
                arr.set_into_list(x, value.pop().unwrap(), false);
            }
        } else {
            arr.more = value;
        }

        arr
    }
}
