//! Module that defines the [`Scope`] type representing a function call-stack scope.

use crate::dynamic::Variant;
use crate::stdlib::{borrow::Cow, boxed::Box, iter, string::String, vec::Vec};
use crate::{Dynamic, StaticVec};

/// Type of an entry in the Scope.
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum EntryType {
    /// Normal value.
    Normal,
    /// Immutable constant value.
    Constant,
}

impl EntryType {
    /// Is this entry constant?
    #[inline(always)]
    pub fn is_constant(&self) -> bool {
        match self {
            Self::Normal => false,
            Self::Constant => true,
        }
    }
}

/// Type containing information about the current scope.
/// Useful for keeping state between [`Engine`][crate::Engine] evaluation runs.
///
/// # Thread Safety
///
/// Currently, [`Scope`] is neither [`Send`] nor [`Sync`].
/// Turn on the `sync` feature to make it [`Send`] `+` [`Sync`].
///
/// # Example
///
/// ```
/// # fn main() -> Result<(), Box<rhai::EvalAltResult>> {
/// use rhai::{Engine, Scope};
///
/// let engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// my_scope.push("z", 40_i64);
///
/// engine.eval_with_scope::<()>(&mut my_scope, "let x = z + 1; z = 0;")?;
///
/// assert_eq!(engine.eval_with_scope::<i64>(&mut my_scope, "x + 1")?, 42);
///
/// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 41);
/// assert_eq!(my_scope.get_value::<i64>("z").unwrap(), 0);
/// # Ok(())
/// # }
/// ```
///
/// When searching for entries, newly-added entries are found before similarly-named but older entries,
/// allowing for automatic _shadowing_.
//
// # Implementation Notes
//
// [`Scope`] is implemented as three [`Vec`]'s of exactly the same length.  Variables data (name, type, etc.)
// is manually split into three equal-length arrays.  That's because variable names take up the most space,
// with [`Cow<str>`][Cow] being four words long, but in the vast majority of cases the name is NOT used to look up
// a variable's value.  Variable lookup is usually via direct index, by-passing the name altogether.
//
// Since [`Dynamic`] is reasonably small, packing it tightly improves cache locality when variables are accessed.
// The variable type is packed separately into another array because it is even smaller.
#[derive(Debug, Clone)]
pub struct Scope<'a> {
    /// Current value of the entry.
    values: Vec<Dynamic>,
    /// Type of the entry.
    types: Vec<EntryType>,
    /// (Name, aliases) of the entry. The list of aliases is Boxed because it occurs rarely.
    names: Vec<(Cow<'a, str>, Box<StaticVec<String>>)>,
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Self {
            values: Vec::with_capacity(16),
            types: Vec::with_capacity(16),
            names: Vec::with_capacity(16),
        }
    }
}

impl<'a> Scope<'a> {
    /// Create a new [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn new() -> Self {
        Default::default()
    }
    /// Empty the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(my_scope.contains("x"));
    /// assert_eq!(my_scope.len(), 1);
    /// assert!(!my_scope.is_empty());
    ///
    /// my_scope.clear();
    /// assert!(!my_scope.contains("x"));
    /// assert_eq!(my_scope.len(), 0);
    /// assert!(my_scope.is_empty());
    /// ```
    #[inline(always)]
    pub fn clear(&mut self) -> &mut Self {
        self.names.clear();
        self.types.clear();
        self.values.clear();
        self
    }
    /// Get the number of entries inside the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    /// assert_eq!(my_scope.len(), 0);
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.len(), 1);
    /// ```
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.values.len()
    }
    /// Is the [`Scope`] empty?
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    /// assert!(my_scope.is_empty());
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(!my_scope.is_empty());
    /// ```
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.values.len() == 0
    }
    /// Add (push) a new entry to the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn push(
        &mut self,
        name: impl Into<Cow<'a, str>>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Normal, Dynamic::from(value))
    }
    /// Add (push) a new [`Dynamic`] entry to the [`Scope`].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic,  Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_dynamic("x", Dynamic::from(42_i64));
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn push_dynamic(&mut self, name: impl Into<Cow<'a, str>>, value: Dynamic) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Normal, value)
    }
    /// Add (push) a new constant to the [`Scope`].
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an [`AST`][crate::AST].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_constant("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn push_constant(
        &mut self,
        name: impl Into<Cow<'a, str>>,
        value: impl Variant + Clone,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Constant, Dynamic::from(value))
    }
    /// Add (push) a new constant with a [`Dynamic`] value to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an [`AST`][crate::AST].
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push_constant_dynamic("x", Dynamic::from(42_i64));
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn push_constant_dynamic(
        &mut self,
        name: impl Into<Cow<'a, str>>,
        value: Dynamic,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Constant, value)
    }
    /// Add (push) a new entry with a [`Dynamic`] value to the [`Scope`].
    #[inline]
    pub(crate) fn push_dynamic_value(
        &mut self,
        name: impl Into<Cow<'a, str>>,
        entry_type: EntryType,
        value: Dynamic,
    ) -> &mut Self {
        self.names.push((name.into(), Box::new(Default::default())));
        self.types.push(entry_type);
        self.values.push(value.into());
        self
    }
    /// Truncate (rewind) the [`Scope`] to a previous size.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// my_scope.push("y", 123_i64);
    /// assert!(my_scope.contains("x"));
    /// assert!(my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 2);
    ///
    /// my_scope.rewind(1);
    /// assert!(my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 1);
    ///
    /// my_scope.rewind(0);
    /// assert!(!my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// assert_eq!(my_scope.len(), 0);
    /// assert!(my_scope.is_empty());
    /// ```
    #[inline(always)]
    pub fn rewind(&mut self, size: usize) -> &mut Self {
        self.names.truncate(size);
        self.types.truncate(size);
        self.values.truncate(size);
        self
    }
    /// Does the [`Scope`] contain the entry?
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert!(my_scope.contains("x"));
    /// assert!(!my_scope.contains("y"));
    /// ```
    #[inline(always)]
    pub fn contains(&self, name: &str) -> bool {
        self.names
            .iter()
            .rev() // Always search a Scope in reverse order
            .any(|(key, _)| name == key.as_ref())
    }
    /// Find an entry in the [`Scope`], starting from the last.
    #[inline(always)]
    pub(crate) fn get_index(&self, name: &str) -> Option<(usize, EntryType)> {
        self.names
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find_map(|(index, (key, _))| {
                if name == key.as_ref() {
                    Some((index, self.types[index]))
                } else {
                    None
                }
            })
    }
    /// Get the value of an entry in the [`Scope`], starting from the last.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    /// ```
    #[inline(always)]
    pub fn get_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        self.names
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (key, _))| name == key.as_ref())
            .and_then(|(index, _)| self.values[index].flatten_clone().try_cast())
    }
    /// Update the value of the named entry in the [`Scope`].
    ///
    /// Search starts backwards from the last, and only the first entry matching the specified name is updated.
    /// If no entry matching the specified name is found, a new one is added.
    ///
    /// # Panics
    ///
    /// Panics when trying to update the value of a constant.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::Scope;
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 42);
    ///
    /// my_scope.set_value("x", 0_i64);
    /// assert_eq!(my_scope.get_value::<i64>("x").unwrap(), 0);
    /// ```
    #[inline(always)]
    pub fn set_value(&mut self, name: &'a str, value: impl Variant + Clone) -> &mut Self {
        match self.get_index(name) {
            None => {
                self.push(name, value);
            }
            Some((_, EntryType::Constant)) => panic!("variable {} is constant", name),
            Some((index, EntryType::Normal)) => {
                *self.values.get_mut(index).unwrap() = Dynamic::from(value);
            }
        }
        self
    }
    /// Get a mutable reference to an entry in the [`Scope`].
    #[inline(always)]
    pub(crate) fn get_mut(&mut self, index: usize) -> (&mut Dynamic, EntryType) {
        (
            self.values.get_mut(index).expect("invalid index in Scope"),
            self.types[index],
        )
    }
    /// Update the access type of an entry in the [`Scope`].
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn add_entry_alias(&mut self, index: usize, alias: String) -> &mut Self {
        let entry = self.names.get_mut(index).expect("invalid index in Scope");
        if !entry.1.contains(&alias) {
            entry.1.push(alias);
        }
        self
    }
    /// Clone the [`Scope`], keeping only the last instances of each variable name.
    /// Shadowed variables are omitted in the copy.
    #[inline]
    pub(crate) fn clone_visible(&self) -> Self {
        let mut entries: Self = Default::default();

        self.names
            .iter()
            .enumerate()
            .rev()
            .for_each(|(index, (name, alias))| {
                if !entries.names.iter().any(|(key, _)| key == name) {
                    entries.names.push((name.clone(), alias.clone()));
                    entries.types.push(self.types[index]);
                    entries.values.push(self.values[index].clone());
                }
            });

        entries
    }
    /// Get an iterator to entries in the [`Scope`].
    #[inline(always)]
    #[allow(dead_code)]
    pub(crate) fn into_iter(
        self,
    ) -> impl Iterator<Item = (Cow<'a, str>, EntryType, Dynamic, Vec<String>)> {
        self.names
            .into_iter()
            .zip(self.types.into_iter().zip(self.values.into_iter()))
            .map(|((name, alias), (typ, value))| (name, typ, value, alias.to_vec()))
    }
    /// Get an iterator to entries in the [`Scope`].
    /// Shared values are flatten-cloned.
    ///
    /// # Example
    ///
    /// ```
    /// use rhai::{Dynamic, Scope};
    ///
    /// let mut my_scope = Scope::new();
    ///
    /// my_scope.push("x", 42_i64);
    /// my_scope.push_constant("foo", "hello".to_string());
    ///
    /// let mut iter = my_scope.iter();
    ///
    /// let (name, constant, value) = iter.next().unwrap();
    /// assert_eq!(name, "x");
    /// assert!(!constant);
    /// assert_eq!(value.cast::<i64>(), 42);
    ///
    /// let (name, constant, value) = iter.next().unwrap();
    /// assert_eq!(name, "foo");
    /// assert!(constant);
    /// assert_eq!(value.cast::<String>(), "hello");
    /// ```
    #[inline(always)]
    pub fn iter(&self) -> impl Iterator<Item = (&str, bool, Dynamic)> {
        self.iter_raw()
            .map(|(name, constant, value)| (name, constant, value.flatten_clone()))
    }
    /// Get an iterator to entries in the [`Scope`].
    /// Shared values are not expanded.
    #[inline(always)]
    pub fn iter_raw<'x: 'a>(&'x self) -> impl Iterator<Item = (&'a str, bool, &'x Dynamic)> + 'x {
        self.names
            .iter()
            .zip(self.types.iter().zip(self.values.iter()))
            .map(|((name, _), (typ, value))| (name.as_ref(), typ.is_constant(), value))
    }
}

impl<'a, K: Into<Cow<'a, str>>> iter::Extend<(K, EntryType, Dynamic)> for Scope<'a> {
    #[inline(always)]
    fn extend<T: IntoIterator<Item = (K, EntryType, Dynamic)>>(&mut self, iter: T) {
        iter.into_iter().for_each(|(name, typ, value)| {
            self.names.push((name.into(), Box::new(Default::default())));
            self.types.push(typ);
            self.values.push(value);
        });
    }
}
