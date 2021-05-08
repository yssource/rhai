//! Module containing various utility types and functions.

use crate::fn_native::{shared_make_mut, shared_take};
use crate::{Identifier, Shared, SmartString};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::TypeId,
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    iter::FromIterator,
    ops::{Add, AddAssign, Deref, Sub, SubAssign},
    str::FromStr,
};

/// A hasher that only takes one single [`u64`] and returns it as a hash key.
///
/// # Panics
///
/// Panics when hashing any data type other than a [`u64`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StraightHasher(u64);

impl Hasher for StraightHasher {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0
    }
    #[inline(always)]
    fn write(&mut self, bytes: &[u8]) {
        assert_eq!(bytes.len(), 8, "StraightHasher can only hash u64 values");

        let mut key = [0_u8; 8];
        key.copy_from_slice(bytes);

        self.0 = u64::from_ne_bytes(key);
    }
}

/// A hash builder for `StraightHasher`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasherBuilder;

impl BuildHasher for StraightHasherBuilder {
    type Hasher = StraightHasher;

    #[inline(always)]
    fn build_hasher(&self) -> Self::Hasher {
        StraightHasher(42)
    }
}

/// Create an instance of the default hasher.
#[inline(always)]
pub fn get_hasher() -> ahash::AHasher {
    Default::default()
}

/// _(INTERNALS)_ Calculate a [`u64`] hash key from a namespace-qualified function name
/// and the number of parameters, but no parameter types.
/// Exported under the `internals` feature only.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via [`TypeId`] values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
#[inline(always)]
pub fn calc_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: impl AsRef<str>,
    num: usize,
) -> u64 {
    let s = &mut get_hasher();

    // We always skip the first module
    let mut len = 0;
    modules
        .inspect(|_| len += 1)
        .skip(1)
        .for_each(|m| m.hash(s));
    len.hash(s);
    fn_name.as_ref().hash(s);
    num.hash(s);
    s.finish()
}

/// _(INTERNALS)_ Calculate a [`u64`] hash key from a list of parameter types.
/// Exported under the `internals` feature only.
///
/// Parameter types are passed in via [`TypeId`] values from an iterator.
#[inline(always)]
pub fn calc_fn_params_hash(params: impl Iterator<Item = TypeId>) -> u64 {
    let s = &mut get_hasher();
    let mut len = 0;
    params.inspect(|_| len += 1).for_each(|t| t.hash(s));
    len.hash(s);
    s.finish()
}

/// Combine two [`u64`] hashes by taking the XOR of them.
#[inline(always)]
pub(crate) fn combine_hashes(a: u64, b: u64) -> u64 {
    a ^ b
}

/// The system immutable string type.
///
/// An [`ImmutableString`] wraps an [`Rc`][std::rc::Rc]`<`[`String`]`>`
///  (or [`Arc`][std::sync::Arc]`<`[`String`]`>` under the `sync` feature)
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
#[derive(Clone, Eq, Ord, Hash, Default)]
pub struct ImmutableString(Shared<SmartString>);

impl Deref for ImmutableString {
    type Target = SmartString;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<SmartString> for ImmutableString {
    #[inline(always)]
    fn as_ref(&self) -> &SmartString {
        &self.0
    }
}

impl AsRef<str> for ImmutableString {
    #[inline(always)]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<SmartString> for ImmutableString {
    #[inline(always)]
    fn borrow(&self) -> &SmartString {
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
        Self(Into::<SmartString>::into(value).into())
    }
}
impl From<&String> for ImmutableString {
    #[inline(always)]
    fn from(value: &String) -> Self {
        Self(Into::<SmartString>::into(value).into())
    }
}
impl From<String> for ImmutableString {
    #[inline(always)]
    fn from(value: String) -> Self {
        Self(Into::<SmartString>::into(value).into())
    }
}
#[cfg(not(feature = "no_smartstring"))]
impl From<SmartString> for ImmutableString {
    #[inline(always)]
    fn from(value: SmartString) -> Self {
        Self(value.into())
    }
}
impl From<ImmutableString> for SmartString {
    #[inline(always)]
    fn from(mut value: ImmutableString) -> Self {
        std::mem::take(shared_make_mut(&mut value.0))
    }
}

impl FromStr for ImmutableString {
    type Err = ();

    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(Into::<SmartString>::into(s).into()))
    }
}

impl FromIterator<char> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<SmartString>().into())
    }
}

impl<'a> FromIterator<&'a char> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = &'a char>>(iter: T) -> Self {
        Self(iter.into_iter().cloned().collect::<SmartString>().into())
    }
}

impl<'a> FromIterator<&'a str> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<SmartString>().into())
    }
}

impl<'a> FromIterator<String> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<SmartString>().into())
    }
}

#[cfg(not(feature = "no_smartstring"))]
impl<'a> FromIterator<SmartString> for ImmutableString {
    #[inline(always)]
    fn from_iter<T: IntoIterator<Item = SmartString>>(iter: T) -> Self {
        Self(iter.into_iter().collect::<SmartString>().into())
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

impl AddAssign<String> for ImmutableString {
    #[inline(always)]
    fn add_assign(&mut self, rhs: String) {
        if !rhs.is_empty() {
            if self.is_empty() {
                self.0 = Into::<SmartString>::into(rhs).into();
            } else {
                self.make_mut().push_str(&rhs);
            }
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

impl Sub for ImmutableString {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs
        } else {
            self.replace(rhs.as_str(), "").into()
        }
    }
}

impl Sub for &ImmutableString {
    type Output = ImmutableString;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            self.clone()
        } else if self.is_empty() {
            rhs.clone()
        } else {
            self.replace(rhs.as_str(), "").into()
        }
    }
}

impl SubAssign<&ImmutableString> for ImmutableString {
    #[inline]
    fn sub_assign(&mut self, rhs: &ImmutableString) {
        if !rhs.is_empty() {
            if self.is_empty() {
                self.0 = rhs.0.clone();
            } else {
                self.0 = Into::<SmartString>::into(self.replace(rhs.as_str(), "")).into();
            }
        }
    }
}

impl SubAssign<ImmutableString> for ImmutableString {
    #[inline]
    fn sub_assign(&mut self, rhs: ImmutableString) {
        if !rhs.is_empty() {
            if self.is_empty() {
                self.0 = rhs.0;
            } else {
                self.0 = Into::<SmartString>::into(self.replace(rhs.as_str(), "")).into();
            }
        }
    }
}

impl Sub<String> for ImmutableString {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: String) -> Self::Output {
        if rhs.is_empty() {
            self
        } else if self.is_empty() {
            rhs.into()
        } else {
            self.replace(&rhs, "").into()
        }
    }
}

impl Sub<String> for &ImmutableString {
    type Output = ImmutableString;

    #[inline]
    fn sub(self, rhs: String) -> Self::Output {
        if rhs.is_empty() {
            self.clone()
        } else if self.is_empty() {
            rhs.into()
        } else {
            self.replace(&rhs, "").into()
        }
    }
}

impl SubAssign<String> for ImmutableString {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: String) {
        self.0 = Into::<SmartString>::into(self.replace(&rhs, "")).into();
    }
}

impl Sub<char> for ImmutableString {
    type Output = Self;

    #[inline(always)]
    fn sub(self, rhs: char) -> Self::Output {
        self.replace(rhs, "").into()
    }
}

impl Sub<char> for &ImmutableString {
    type Output = ImmutableString;

    #[inline(always)]
    fn sub(self, rhs: char) -> Self::Output {
        self.replace(rhs, "").into()
    }
}

impl SubAssign<char> for ImmutableString {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: char) {
        self.0 = Into::<SmartString>::into(self.replace(rhs, "")).into();
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
    /// Consume the [`ImmutableString`] and convert it into a [`String`].
    /// If there are other references to the same string, a cloned copy is returned.
    #[inline(always)]
    pub fn into_owned(mut self) -> String {
        self.make_mut(); // Make sure it is unique reference
        shared_take(self.0).into() // Should succeed
    }
    /// Make sure that the [`ImmutableString`] is unique (i.e. no other outstanding references).
    /// Then return a mutable reference to the [`SmartString`].
    #[inline(always)]
    pub(crate) fn make_mut(&mut self) -> &mut SmartString {
        shared_make_mut(&mut self.0)
    }
}

/// A factory of identifiers from text strings.
///
/// When [`SmartString`](https://crates.io/crates/smartstring) is used as [`Identifier`],
/// this just returns one because most identifiers in Rhai are short and ASCII-based.
///
/// When [`ImmutableString`] is used as [`Identifier`], this type acts as an interner which keeps a
/// collection of strings and returns shared instances, only creating a new string when it is not
/// yet interned.
#[derive(Debug, Clone, Default, Hash)]
pub struct IdentifierBuilder(
    #[cfg(feature = "no_smartstring")] std::collections::BTreeSet<Identifier>,
);

impl IdentifierBuilder {
    /// Get an identifier from a text string.
    #[inline(always)]
    pub fn get(&mut self, text: impl AsRef<str> + Into<Identifier>) -> Identifier {
        #[cfg(not(feature = "no_smartstring"))]
        return text.as_ref().into();

        #[cfg(feature = "no_smartstring")]
        return self.0.get(text.as_ref()).cloned().unwrap_or_else(|| {
            let s: Identifier = text.into();
            self.0.insert(s.clone());
            s
        });
    }
}
