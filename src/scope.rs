//! Module that defines the `Scope` type representing a function call-stack scope.

use crate::any::{Any, Dynamic};
use crate::parser::{map_dynamic_to_expr, Expr, Position};

use crate::stdlib::{
    borrow::Cow,
    iter,
    string::{String, ToString},
    vec::Vec,
};

/// Type of an entry in the Scope.
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum EntryType {
    /// Normal value.
    Normal,
    /// Immutable constant value.
    Constant,
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
    /// A constant expression if the initial value matches one of the recognized types.
    pub expr: Option<Expr>,
}

/// Information about a particular entry in the Scope.
#[derive(Debug, Hash, Copy, Clone)]
pub(crate) struct EntryRef<'a> {
    pub name: &'a str,
    pub index: usize,
    pub typ: EntryType,
}

/// A type containing information about the current scope.
/// Useful for keeping state between `Engine` evaluation runs.
///
/// # Example
///
/// ```
/// # fn main() -> Result<(), rhai::EvalAltResult> {
/// use rhai::{Engine, Scope};
///
/// let mut engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// engine.eval_with_scope::<()>(&mut my_scope, "let x = 5;")?;
///
/// assert_eq!(engine.eval_with_scope::<i64>(&mut my_scope, "x + 1")?, 6);
/// # Ok(())
/// # }
/// ```
///
/// When searching for entries, newly-added entries are found before similarly-named but older entries,
/// allowing for automatic _shadowing_.
///
/// Currently, `Scope` is neither `Send` nor `Sync`. Turn on the `sync` feature to make it `Send + Sync`.
pub struct Scope<'a>(Vec<Entry<'a>>);

impl<'a> Scope<'a> {
    /// Create a new Scope.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Empty the Scope.
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Get the number of entries inside the Scope.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Is the Scope empty?
    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    /// Add (push) a new entry to the Scope.
    pub fn push<K: Into<Cow<'a, str>>, T: Any + Clone>(&mut self, name: K, value: T) {
        self.push_dynamic_value(name, EntryType::Normal, value.into_dynamic(), false);
    }

    /// Add (push) a new `Dynamic` entry to the Scope.
    pub fn push_dynamic<K: Into<Cow<'a, str>>>(&mut self, name: K, value: Dynamic) {
        self.push_dynamic_value(name, EntryType::Normal, value, false);
    }

    /// Add (push) a new constant to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an AST.
    /// However, in order to be used for optimization, constants must be in one of the recognized types:
    /// `INT` (default to `i64`, `i32` if `only_i32`), `f64`, `String`, `char` and `bool`.
    pub fn push_constant<K: Into<Cow<'a, str>>, T: Any + Clone>(&mut self, name: K, value: T) {
        self.push_dynamic_value(name, EntryType::Constant, value.into_dynamic(), true);
    }

    /// Add (push) a new constant with a `Dynamic` value to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an AST.
    /// However, in order to be used for optimization, the `Dynamic` value must be in one of the
    /// recognized types:
    /// `INT` (default to `i64`, `i32` if `only_i32`), `f64`, `String`, `char` and `bool`.
    pub fn push_constant_dynamic<K: Into<Cow<'a, str>>>(&mut self, name: K, value: Dynamic) {
        self.push_dynamic_value(name, EntryType::Constant, value, true);
    }

    /// Add (push) a new entry with a `Dynamic` value to the Scope.
    pub(crate) fn push_dynamic_value<K: Into<Cow<'a, str>>>(
        &mut self,
        name: K,
        entry_type: EntryType,
        value: Dynamic,
        map_expr: bool,
    ) {
        let (expr, value) = if map_expr {
            map_dynamic_to_expr(value, Position::none())
        } else {
            (None, value)
        };

        self.0.push(Entry {
            name: name.into(),
            typ: entry_type,
            value,
            expr,
        });
    }

    /// Truncate (rewind) the Scope to a previous size.
    pub fn rewind(&mut self, size: usize) {
        self.0.truncate(size);
    }

    /// Does the scope contain the entry?
    pub fn contains(&self, key: &str) -> bool {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .any(|(_, Entry { name, .. })| name == key)
    }

    /// Find an entry in the Scope, starting from the last.
    pub(crate) fn get(&self, key: &str) -> Option<(EntryRef, Dynamic)> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, Entry { name, .. })| name == key)
            .map(
                |(
                    i,
                    Entry {
                        name, typ, value, ..
                    },
                )| {
                    (
                        EntryRef {
                            name: name.as_ref(),
                            index: i,
                            typ: *typ,
                        },
                        value.clone(),
                    )
                },
            )
    }

    /// Get the value of an entry in the Scope, starting from the last.
    pub fn get_value<T: Any + Clone>(&self, key: &str) -> Option<T> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, Entry { name, .. })| name == key)
            .and_then(|(_, Entry { value, .. })| value.downcast_ref::<T>())
            .map(T::clone)
    }

    /// Get a mutable reference to an entry in the Scope.
    pub(crate) fn get_mut(&mut self, key: EntryRef) -> &mut Dynamic {
        let entry = self.0.get_mut(key.index).expect("invalid index in Scope");
        assert_eq!(entry.typ, key.typ, "entry type not matched");

        // assert_ne!(
        //     entry.typ,
        //     EntryType::Constant,
        //     "get mut of constant entry"
        // );
        assert_eq!(entry.name, key.name, "incorrect key at Scope entry");

        &mut entry.value
    }

    /// Get a mutable reference to an entry in the Scope and downcast it to a specific type
    pub(crate) fn get_mut_by_type<T: Any + Clone>(&mut self, key: EntryRef) -> &mut T {
        self.get_mut(key)
            .downcast_mut::<T>()
            .expect("wrong type cast")
    }

    /// Get an iterator to entries in the Scope.
    pub fn iter(&self) -> impl Iterator<Item = &Entry> {
        self.0.iter().rev() // Always search a Scope in reverse order
    }
}

impl Default for Scope<'_> {
    fn default() -> Self {
        Scope::new()
    }
}

impl<'a, K> iter::Extend<(K, EntryType, Dynamic)> for Scope<'a>
where
    K: Into<Cow<'a, str>>,
{
    fn extend<T: IntoIterator<Item = (K, EntryType, Dynamic)>>(&mut self, iter: T) {
        self.0
            .extend(iter.into_iter().map(|(name, typ, value)| Entry {
                name: name.into(),
                typ,
                value,
                expr: None,
            }));
    }
}
