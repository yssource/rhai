//! Module that defines the `Scope` type representing a function call-stack scope.

use crate::any::{Dynamic, Variant};
use crate::parser::{map_dynamic_to_expr, Expr};
use crate::token::Position;

use crate::stdlib::{borrow::Cow, boxed::Box, iter, string::String, vec::Vec};

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

/// An entry in the Scope.
#[derive(Debug, Clone)]
pub struct Entry<'a> {
    /// Name of the entry.
    pub name: Cow<'a, str>,
    /// Type of the entry.
    pub typ: EntryType,
    /// Current value of the entry.
    pub value: Dynamic,
    /// Alias of the entry.
    pub alias: Option<Box<String>>,
    /// A constant expression if the initial value matches one of the recognized types.
    pub expr: Option<Box<Expr>>,
}

/// Type containing information about the current scope.
/// Useful for keeping state between `Engine` evaluation runs.
///
/// # Thread Safety
///
/// Currently, `Scope` is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
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
#[derive(Debug, Clone, Default)]
pub struct Scope<'a>(Vec<Entry<'a>>);

impl<'a> Scope<'a> {
    /// Create a new Scope.
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

    /// Empty the Scope.
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
        self.0.clear();
        self
    }

    /// Get the number of entries inside the Scope.
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
        self.0.len()
    }

    /// Is the Scope empty?
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
        self.0.len() == 0
    }

    /// Add (push) a new entry to the Scope.
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
    pub fn push<K: Into<Cow<'a, str>>, T: Variant + Clone>(
        &mut self,
        name: K,
        value: T,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Normal, Dynamic::from(value), false)
    }

    /// Add (push) a new `Dynamic` entry to the Scope.
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
    pub fn push_dynamic<K: Into<Cow<'a, str>>>(&mut self, name: K, value: Dynamic) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Normal, value, false)
    }

    /// Add (push) a new constant to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an AST.
    ///
    /// However, in order to be used for optimization, constants must be in one of the recognized types:
    /// `INT` (default to `i64`, `i32` if `only_i32`), `f64`, `String`, `char` and `bool`.
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
    pub fn push_constant<K: Into<Cow<'a, str>>, T: Variant + Clone>(
        &mut self,
        name: K,
        value: T,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Constant, Dynamic::from(value), true)
    }

    /// Add (push) a new constant with a `Dynamic` value to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an AST.
    ///
    /// However, in order to be used for optimization, the `Dynamic` value must be in one of the
    /// recognized types:
    /// `INT` (default to `i64`, `i32` if `only_i32`), `f64`, `String`, `char` and `bool`.
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
    pub fn push_constant_dynamic<K: Into<Cow<'a, str>>>(
        &mut self,
        name: K,
        value: Dynamic,
    ) -> &mut Self {
        self.push_dynamic_value(name, EntryType::Constant, value, true)
    }

    /// Add (push) a new entry with a `Dynamic` value to the Scope.
    #[inline]
    pub(crate) fn push_dynamic_value<K: Into<Cow<'a, str>>>(
        &mut self,
        name: K,
        entry_type: EntryType,
        value: Dynamic,
        map_expr: bool,
    ) -> &mut Self {
        let expr = if map_expr {
            map_dynamic_to_expr(value.clone(), Position::none()).map(Box::new)
        } else {
            None
        };

        self.0.push(Entry {
            name: name.into(),
            typ: entry_type,
            alias: None,
            value: value.into(),
            expr,
        });

        self
    }

    /// Truncate (rewind) the Scope to a previous size.
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
        self.0.truncate(size);
        self
    }

    /// Does the scope contain the entry?
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
        self.0
            .iter()
            .rev() // Always search a Scope in reverse order
            .any(|Entry { name: key, .. }| name == key)
    }

    /// Find an entry in the Scope, starting from the last.
    #[inline(always)]
    pub(crate) fn get_index(&self, name: &str) -> Option<(usize, EntryType)> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find_map(|(index, Entry { name: key, typ, .. })| {
                if name == key {
                    Some((index, *typ))
                } else {
                    None
                }
            })
    }

    /// Get an entry in the Scope, starting from the last.
    #[inline(always)]
    pub(crate) fn get_entry(&self, name: &str) -> Option<&Entry> {
        self.0
            .iter()
            .rev()
            .find(|Entry { name: key, .. }| name == key)
    }

    /// Get the value of an entry in the Scope, starting from the last.
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
        self.get_entry(name)
            .and_then(|Entry { value, .. }| value.flatten_clone().try_cast())
    }

    /// Update the value of the named entry.
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
    pub fn set_value<T: Variant + Clone>(&mut self, name: &'a str, value: T) -> &mut Self {
        match self.get_index(name) {
            None => {
                self.push(name, value);
            }
            Some((_, EntryType::Constant)) => panic!("variable {} is constant", name),
            Some((index, EntryType::Normal)) => {
                self.0.get_mut(index).unwrap().value = Dynamic::from(value);
            }
        }
        self
    }

    /// Get a mutable reference to an entry in the Scope.
    #[inline(always)]
    pub(crate) fn get_mut(&mut self, index: usize) -> (&mut Dynamic, EntryType) {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");
        (&mut entry.value, entry.typ)
    }

    /// Update the access type of an entry in the Scope.
    #[cfg(not(feature = "no_module"))]
    #[inline(always)]
    pub(crate) fn set_entry_alias(&mut self, index: usize, alias: String) -> &mut Self {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");
        entry.alias = Some(Box::new(alias));
        self
    }

    /// Clone the Scope, keeping only the last instances of each variable name.
    /// Shadowed variables are omitted in the copy.
    #[inline]
    pub(crate) fn clone_visible(&self) -> Self {
        let mut entries: Vec<Entry> = Default::default();

        self.0.iter().rev().for_each(|entry| {
            if entries
                .iter()
                .find(|Entry { name, .. }| &entry.name == name)
                .is_none()
            {
                entries.push(entry.clone());
            }
        });

        Self(entries)
    }

    /// Get an iterator to entries in the Scope.
    #[inline(always)]
    pub(crate) fn into_iter(self) -> impl Iterator<Item = Entry<'a>> {
        self.0.into_iter()
    }

    /// Get an iterator to entries in the Scope in reverse order.
    #[inline(always)]
    pub(crate) fn to_iter(&self) -> impl Iterator<Item = &Entry> {
        self.0.iter().rev() // Always search a Scope in reverse order
    }

    /// Get an iterator to entries in the Scope.
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

    /// Get an iterator to entries in the Scope.
    /// Shared values are not expanded.
    #[inline(always)]
    pub fn iter_raw(&self) -> impl Iterator<Item = (&str, bool, &Dynamic)> {
        self.0.iter().map(
            |Entry {
                 name, typ, value, ..
             }| { (name.as_ref(), typ.is_constant(), value) },
        )
    }
}

impl<'a, K: Into<Cow<'a, str>>> iter::Extend<(K, EntryType, Dynamic)> for Scope<'a> {
    #[inline(always)]
    fn extend<T: IntoIterator<Item = (K, EntryType, Dynamic)>>(&mut self, iter: T) {
        self.0
            .extend(iter.into_iter().map(|(name, typ, value)| Entry {
                name: name.into(),
                typ,
                alias: None,
                value: value.into(),
                expr: None,
            }));
    }
}
