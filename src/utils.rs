//! Module containing various utility types and functions.

use crate::fn_native::{shared_make_mut, shared_take};
use crate::stdlib::{
    any::TypeId,
    borrow::Borrow,
    boxed::Box,
    cmp::Ordering,
    collections::HashMap,
    fmt,
    fmt::{Debug, Display},
    hash::{BuildHasher, Hash, Hasher},
    iter::{empty, FromIterator},
    num::NonZeroU64,
    ops::{Add, AddAssign, Deref, DerefMut, Sub, SubAssign},
    str::FromStr,
    string::{String, ToString},
    vec::Vec,
};
use crate::Shared;

/// A hasher that only takes one single [`NonZeroU64`] and returns it as a hash key.
///
/// # Panics
///
/// Panics when hashing any data type other than a [`NonZeroU64`].
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct StraightHasher(NonZeroU64);

impl Hasher for StraightHasher {
    #[inline(always)]
    fn finish(&self) -> u64 {
        self.0.get()
    }
    #[inline(always)]
    fn write(&mut self, bytes: &[u8]) {
        assert_eq!(bytes.len(), 8, "StraightHasher can only hash u64 values");

        let mut key = [0_u8; 8];
        key.copy_from_slice(bytes);

        // HACK - If it so happens to hash directly to zero (OMG!) then change it to 42...
        self.0 = NonZeroU64::new(u64::from_ne_bytes(key))
            .unwrap_or_else(|| NonZeroU64::new(42).unwrap());
    }
}

/// A hash builder for `StraightHasher`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct StraightHasherBuilder;

impl BuildHasher for StraightHasherBuilder {
    type Hasher = StraightHasher;

    #[inline(always)]
    fn build_hasher(&self) -> Self::Hasher {
        StraightHasher(NonZeroU64::new(42).unwrap())
    }
}

/// Create an instance of the default hasher.
#[inline(always)]
pub fn get_hasher() -> ahash::AHasher {
    Default::default()
}

/// _(INTERNALS)_ Calculate a [`NonZeroU64`] hash key from a namespace-qualified function name and
/// parameter types.
/// Exported under the `internals` feature only.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via [`TypeId`] values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
#[inline(always)]
pub fn calc_native_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    params: impl Iterator<Item = TypeId>,
) -> Option<NonZeroU64> {
    calc_fn_hash(modules, fn_name, None, params)
}

/// _(INTERNALS)_ Calculate a [`NonZeroU64`] hash key from a namespace-qualified function name
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
pub fn calc_script_fn_hash<'a>(
    modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    num: usize,
) -> Option<NonZeroU64> {
    calc_fn_hash(modules, fn_name, Some(num), empty())
}

/// Calculate a [`NonZeroU64`] hash key from a namespace-qualified function name and parameter types.
///
/// Module names are passed in via `&str` references from an iterator.
/// Parameter types are passed in via [`TypeId`] values from an iterator.
///
/// # Note
///
/// The first module name is skipped.  Hashing starts from the _second_ module in the chain.
#[inline(always)]
fn calc_fn_hash<'a>(
    mut modules: impl Iterator<Item = &'a str>,
    fn_name: &str,
    num: Option<usize>,
    params: impl Iterator<Item = TypeId>,
) -> Option<NonZeroU64> {
    let s = &mut get_hasher();

    // Hash a boolean indicating whether the hash is namespace-qualified.
    modules.next().is_some().hash(s);
    // We always skip the first module
    modules.for_each(|m| m.hash(s));
    fn_name.hash(s);
    if let Some(num) = num {
        num.hash(s);
    } else {
        params.for_each(|t| t.hash(s));
    }
    // HACK - If it so happens to hash directly to zero (OMG!) then change it to 42...
    NonZeroU64::new(s.finish()).or_else(|| NonZeroU64::new(42))
}

/// Combine two [`NonZeroU64`] hashes by taking the XOR of them.
#[inline(always)]
pub(crate) fn combine_hashes(a: NonZeroU64, b: NonZeroU64) -> NonZeroU64 {
    // HACK - If it so happens to hash directly to zero (OMG!) then change it to 42...
    NonZeroU64::new(a.get() ^ b.get()).unwrap_or_else(|| NonZeroU64::new(42).unwrap())
}

/// _(INTERNALS)_ A type that wraps a [`HashMap`] and implements [`Hash`].
/// Exported under the `internals` feature only.
#[derive(Clone, Default)]
pub struct HashableHashMap<K, T, H: BuildHasher>(HashMap<K, T, H>);

impl<K, T, H: BuildHasher> From<HashMap<K, T, H>> for HashableHashMap<K, T, H> {
    fn from(value: HashMap<K, T, H>) -> Self {
        Self(value)
    }
}
impl<K, T, H: BuildHasher> AsRef<HashMap<K, T, H>> for HashableHashMap<K, T, H> {
    fn as_ref(&self) -> &HashMap<K, T, H> {
        &self.0
    }
}
impl<K, T, H: BuildHasher> AsMut<HashMap<K, T, H>> for HashableHashMap<K, T, H> {
    fn as_mut(&mut self) -> &mut HashMap<K, T, H> {
        &mut self.0
    }
}
impl<K, T, H: BuildHasher> Deref for HashableHashMap<K, T, H> {
    type Target = HashMap<K, T, H>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<K, T, H: BuildHasher> DerefMut for HashableHashMap<K, T, H> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl<K: Debug, T: Debug, H: BuildHasher> Debug for HashableHashMap<K, T, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
impl<K: Hash + Ord, T: Hash, H: BuildHasher> Hash for HashableHashMap<K, T, H> {
    fn hash<B: Hasher>(&self, state: &mut B) {
        let mut keys: Vec<_> = self.0.keys().collect();
        keys.sort();

        keys.into_iter().for_each(|key| {
            key.hash(state);
            self.0.get(&key).unwrap().hash(state);
        });
    }
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
impl From<&String> for ImmutableString {
    #[inline(always)]
    fn from(value: &String) -> Self {
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

impl Display for ImmutableString {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.0.as_str(), f)
    }
}

impl Debug for ImmutableString {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.0.as_str(), f)
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
        self.make_mut().push_str(&rhs);
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
                self.0 = self.replace(rhs.as_str(), "").into();
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
                self.0 = self.replace(rhs.as_str(), "").into();
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
        self.0 = self.replace(&rhs, "").into();
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
        self.0 = self.replace(rhs, "").into();
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
    /// Consume the [`ImmutableString`] and convert it into a [`String`].
    /// If there are other references to the same string, a cloned copy is returned.
    #[inline(always)]
    pub fn into_owned(mut self) -> String {
        self.make_mut(); // Make sure it is unique reference
        shared_take(self.0) // Should succeed
    }
    /// Make sure that the [`ImmutableString`] is unique (i.e. no other outstanding references).
    /// Then return a mutable reference to the [`String`].
    #[inline(always)]
    pub fn make_mut(&mut self) -> &mut String {
        shared_make_mut(&mut self.0)
    }
}
