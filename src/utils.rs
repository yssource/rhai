//! Module containing various utility types and functions.

use crate::fn_native::{shared_make_mut, shared_take, Shared};

use crate::stdlib::{
    any::TypeId,
    borrow::Borrow,
    boxed::Box,
    cmp::Ordering,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    iter::{empty, FromIterator},
    ops::{Add, AddAssign, Deref},
    str::FromStr,
    string::{String, ToString},
};

#[cfg(not(feature = "no_std"))]
use crate::stdlib::collections::hash_map::DefaultHasher;

#[cfg(feature = "no_std")]
use ahash::AHasher;

/// A hasher that only takes one single `u64` and returns it as a hash key.
///
/// # Panics
///
/// Panics when hashing any data type other than a `u64`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasher(u64);

impl Hasher for StraightHasher {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0
    }
    #[inline(always)]
    fn write(&mut self, bytes: &[u8]) {
        let mut key = [0_u8; 8];
        key.copy_from_slice(&bytes[..8]); // Panics if fewer than 8 bytes
        self.0 = u64::from_le_bytes(key);
    }
}

impl StraightHasher {
    /// Create a `StraightHasher`.
    #[inline(always)]
    pub fn new() -> Self {
        Self(0)
    }
}

/// A hash builder for `StraightHasher`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasherBuilder;

impl BuildHasher for StraightHasherBuilder {
    type Hasher = StraightHasher;

    #[inline(always)]
    fn build_hasher(&self) -> Self::Hasher {
        StraightHasher::new()
    }
}

/// _[INTERNALS]_ Calculate a `u64` hash key from a module-qualified function name and parameter types.
/// Exported under the `internals` feature only.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via `TypeId` values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
#[inline(always)]
pub fn calc_native_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    params: impl Iterator<Item = TypeId>,
) -> u64 {
    calc_fn_hash(modules, fn_name, None, params)
}

/// _[INTERNALS]_ Calculate a `u64` hash key from a module-qualified function name and the number of parameters,
/// but no parameter types.
/// Exported under the `internals` feature only.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via `TypeId` values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
#[inline(always)]
pub fn calc_script_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    num: usize,
) -> u64 {
    calc_fn_hash(modules, fn_name, Some(num), empty())
}

/// Calculate a `u64` hash key from a module-qualified function name and parameter types.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via `TypeId` values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
fn calc_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    num: Option<usize>,
    params: impl Iterator<Item = TypeId>,
) -> u64 {
    #[cfg(feature = "no_std")]
    let s: &mut AHasher = &mut Default::default();
    #[cfg(not(feature = "no_std"))]
    let s = &mut DefaultHasher::new();

    // We always skip the first module
    modules.skip(1).for_each(|m| m.hash(s));
    s.write(fn_name.as_bytes());
    let num = if let Some(num) = num {
        num
    } else {
        let mut count = 0;

        params.for_each(|t| {
            count += 1;
            t.hash(s);
        });

        count
    };
    s.write_usize(num);
    s.finish()
}

/// The system immutable string type.
///
/// An `ImmutableString` wraps an `Rc<String>` (or `Arc<String>` under the `sync` feature)
/// so that it can be simply shared and not cloned.
///
/// # Example
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

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<String> for ImmutableString {
    #[inline(always)]
    fn as_ref(&self) -> &String {
        &self.0
    }
}

impl Borrow<String> for ImmutableString {
    #[inline(always)]
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl Borrow<str> for ImmutableString {
    #[inline(always)]
    fn borrow(&self) -> &str {
        self.0.as_str()
    }
}

impl From<&str> for ImmutableString {
    #[inline(always)]
    fn from(value: &str) -> Self {
        Self(value.to_string().into())
    }
}
impl From<String> for ImmutableString {
    #[inline(always)]
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<Box<String>> for ImmutableString {
    #[inline(always)]
    fn from(value: Box<String>) -> Self {
        Self(value.into())
    }
}

impl From<ImmutableString> for String {
    #[inline(always)]
    fn from(value: ImmutableString) -> Self {
        value.into_owned()
    }
}

impl FromStr for ImmutableString {
    type Err = ();

    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(s.to_string().into()))
    }
}

impl FromIterator<char> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl<'a> FromIterator<&'a char> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = &'a char>>(iter: T) -> Self {
        Self(iter.into_iter().cloned().collect::<String>().into())
    }
}

impl<'a> FromIterator<&'a str> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl<'a> FromIterator<String> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<String>().into())
    }
}

impl fmt::Display for ImmutableString {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.0.as_str(), f)
    }
}

impl fmt::Debug for ImmutableString {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.0.as_str(), f)
    }
}

impl Add for ImmutableString {
    type Output = Self;

    #[inline]
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

    #[inline]
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
    #[inline]
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

impl AddAssign<ImmutableString> for ImmutableString {
    #[inline]
    fn add_assign(&mut self, rhs: ImmutableString) {
        if !rhs.is_empty() {
            if self.is_empty() {
                self.0 = rhs.0;
            } else {
                self.make_mut().push_str(rhs.0.as_str());
            }
        }
    }
}

impl Add<&str> for ImmutableString {
    type Output = Self;

    #[inline]
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

    #[inline]
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
    #[inline(always)]
    fn add_assign(&mut self, rhs: &str) {
        if !rhs.is_empty() {
            self.make_mut().push_str(rhs);
        }
    }
}

impl Add<String> for ImmutableString {
    type Output = Self;

    #[inline]
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

    #[inline]
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

    #[inline(always)]
    fn add(mut self, rhs: char) -> Self::Output {
        self.make_mut().push(rhs);
        self
    }
}

impl Add<char> for &ImmutableString {
    type Output = ImmutableString;

    #[inline(always)]
    fn add(self, rhs: char) -> Self::Output {
        let mut s = self.clone();
        s.make_mut().push(rhs);
        s
    }
}

impl AddAssign<char> for ImmutableString {
    #[inline(always)]
    fn add_assign(&mut self, rhs: char) {
        self.make_mut().push(rhs);
    }
}

impl<S: AsRef<str>> PartialEq<S> for ImmutableString {
    #[inline(always)]
    fn eq(&self, other: &S) -> bool {
        self.as_str().eq(other.as_ref())
    }
}

impl PartialEq<ImmutableString> for str {
    #[inline(always)]
    fn eq(&self, other: &ImmutableString) -> bool {
        self.eq(other.as_str())
    }
}

impl PartialEq<ImmutableString> for String {
    #[inline(always)]
    fn eq(&self, other: &ImmutableString) -> bool {
        self.eq(other.as_str())
    }
}

impl<S: AsRef<str>> PartialOrd<S> for ImmutableString {
    #[inline(always)]
    fn partial_cmp(&self, other: &S) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_ref())
    }
}

impl PartialOrd<ImmutableString> for str {
    #[inline(always)]
    fn partial_cmp(&self, other: &ImmutableString) -> Option<Ordering> {
        self.partial_cmp(other.as_str())
    }
}

impl PartialOrd<ImmutableString> for String {
    #[inline(always)]
    fn partial_cmp(&self, other: &ImmutableString) -> Option<Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl ImmutableString {
    /// Consume the `ImmutableString` and convert it into a `String`.
    /// If there are other references to the same string, a cloned copy is returned.
    #[inline(always)]
    pub fn into_owned(mut self) -> String {
        self.make_mut(); // Make sure it is unique reference
        shared_take(self.0) // Should succeed
    }
    /// Make sure that the `ImmutableString` is unique (i.e. no other outstanding references).
    /// Then return a mutable reference to the `String`.
    #[inline(always)]
    pub fn make_mut(&mut self) -> &mut String {
        shared_make_mut(&mut self.0)
    }
}
