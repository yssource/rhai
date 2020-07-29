//! Module containing various utility types and functions.

use crate::fn_native::{shared_make_mut, shared_take, Shared};

use crate::stdlib::{
    any::TypeId,
    borrow::Borrow,
    boxed::Box,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    iter::FromIterator,
    ops::{Add, AddAssign, Deref},
    str::FromStr,
    string::{String, ToString},
};

#[cfg(not(feature = "no_std"))]
use crate::stdlib::collections::hash_map::DefaultHasher;

#[cfg(feature = "no_std")]
use ahash::AHasher;

use smallvec::SmallVec;

/// A hasher that only takes one single `u64` and returns it as a hash key.
///
/// # Panics
///
/// Panics when hashing any data type other than a `u64`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasher(u64);

impl Hasher for StraightHasher {
    fn finish(&self) -> u64 {
        self.0
    }
    fn write(&mut self, bytes: &[u8]) {
        let mut key = [0_u8; 8];
        key.copy_from_slice(&bytes[..8]); // Panics if fewer than 8 bytes
        self.0 = u64::from_le_bytes(key);
    }
}

impl StraightHasher {
    /// Create a `StraightHasher`.
    pub fn new() -> Self {
        Self(0)
    }
}

/// A hash builder for `StraightHasher`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasherBuilder;

impl BuildHasher for StraightHasherBuilder {
    type Hasher = StraightHasher;

    fn build_hasher(&self) -> Self::Hasher {
        StraightHasher::new()
    }
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

/// [INTERNALS] Alias to [`smallvec::SmallVec<[T; 4]>`](https://crates.io/crates/smallvec),
/// which is a specialized `Vec` backed by a small, fixed-size array when there are <= 4 items stored.
/// Exported under the `internals` feature only.
pub type StaticVec<T> = SmallVec<[T; 4]>;

/// The system immutable string type.
///
/// An `ImmutableString` wraps an `Rc<String>` (or `Arc<String>` under the `sync` feature)
/// so that it can be simply shared and not cloned.
///
/// # Examples
///
/// ```
/// use rhai::ImmutableString;
///
/// let s1: ImmutableString = "hello".into();
///
/// // No actual cloning of the string is involved below.
/// let s2 = s1.clone();
/// let s3 = s2.clone();
///
/// assert_eq!(s1, s2);
///
/// // Clones the underlying string (because it is already shared) and extracts it.
/// let mut s: String = s1.into_owned();
///
/// // Changing the clone has no impact on the previously shared version.
/// s.push_str(", world!");
///
/// // The old version still exists.
/// assert_eq!(s2, s3);
/// assert_eq!(s2.as_str(), "hello");
///
/// // Not equals!
/// assert_ne!(s2.as_str(), s.as_str());
/// assert_eq!(s, "hello, world!");
/// ```
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct ImmutableString(Shared<String>);

impl Deref for ImmutableString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<String> for ImmutableString {
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl Borrow<str> for ImmutableString {
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&str> for ImmutableString {
    fn from(value: &str) -> Self {
        Self(value.to_string().into())
    }
}
impl From<String> for ImmutableString {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<Box<String>> for ImmutableString {
    fn from(value: Box<String>) -> Self {
        Self(value.into())
    }
}

impl From<ImmutableString> for String {
    fn from(value: ImmutableString) -> Self {
        value.into_owned()
    }
}

impl FromStr for ImmutableString {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.to_string().into()))
    }
}

impl FromIterator<char> for ImmutableString {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl<'a> FromIterator<&'a char> for ImmutableString {
    fn from_iter<T: IntoIterator<Item = &'a char>>(iter: T) -> Self {
        Self(iter.into_iter().cloned().collect::<String>().into())
    }
}

impl<'a> FromIterator<&'a str> for ImmutableString {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl<'a> FromIterator<String> for ImmutableString {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl fmt::Display for ImmutableString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.0.as_str(), f)
    }
}

impl fmt::Debug for ImmutableString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.0.as_str(), f)
    }
}

impl Add for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs
        } else {
            self.make_mut().push_str(rhs.0.as_str());
            self
        }
    }
}

impl Add for &ImmutableString {
    type Output = ImmutableString;

    fn add(self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            self.clone()
        } else if self.is_empty() {
            rhs.clone()
        } else {
            let mut s = self.clone();
            s.make_mut().push_str(rhs.0.as_str());
            s
        }
    }
}

impl AddAssign<&ImmutableString> for ImmutableString {
    fn add_assign(&mut self, rhs: &ImmutableString) {
        if !rhs.is_empty() {
            if self.is_empty() {
                self.0 = rhs.0.clone();
            } else {
                self.make_mut().push_str(rhs.0.as_str());
            }
        }
    }
}

impl Add<&str> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: &str) -> Self::Output {
        if rhs.is_empty() {
            self
        } else {
            self.make_mut().push_str(rhs);
            self
        }
    }
}

impl Add<&str> for &ImmutableString {
    type Output = ImmutableString;

    fn add(self, rhs: &str) -> Self::Output {
        if rhs.is_empty() {
            self.clone()
        } else {
            let mut s = self.clone();
            s.make_mut().push_str(rhs);
            s
        }
    }
}

impl AddAssign<&str> for ImmutableString {
    fn add_assign(&mut self, rhs: &str) {
        if !rhs.is_empty() {
            self.make_mut().push_str(rhs);
        }
    }
}

impl Add<String> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: String) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs.into()
        } else {
            self.make_mut().push_str(&rhs);
            self
        }
    }
}

impl Add<String> for &ImmutableString {
    type Output = ImmutableString;

    fn add(self, rhs: String) -> Self::Output {
        if rhs.is_empty() {
            self.clone()
        } else if self.is_empty() {
            rhs.into()
        } else {
            let mut s = self.clone();
            s.make_mut().push_str(&rhs);
            s
        }
    }
}

impl Add<char> for ImmutableString {
    type Output = Self;

    fn add(mut self, rhs: char) -> Self::Output {
        self.make_mut().push(rhs);
        self
    }
}

impl Add<char> for &ImmutableString {
    type Output = ImmutableString;

    fn add(self, rhs: char) -> Self::Output {
        let mut s = self.clone();
        s.make_mut().push(rhs);
        s
    }
}

impl AddAssign<char> for ImmutableString {
    fn add_assign(&mut self, rhs: char) {
        self.make_mut().push(rhs);
    }
}

impl ImmutableString {
    /// Consume the `ImmutableString` and convert it into a `String`.
    /// If there are other references to the same string, a cloned copy is returned.
    pub fn into_owned(mut self) -> String {
        self.make_mut(); // Make sure it is unique reference
        shared_take(self.0) // Should succeed
    }
    /// Make sure that the `ImmutableString` is unique (i.e. no other outstanding references).
    /// Then return a mutable reference to the `String`.
    pub fn make_mut(&mut self) -> &mut String {
        shared_make_mut(&mut self.0)
    }
}
