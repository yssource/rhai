//! Module that defines the `Scope` type representing a function call-stack scope.

use crate::any::{Any, Dynamic};
use crate::parser::{map_dynamic_to_expr, Expr, Position};

use std::borrow::Cow;

/// Type of a variable in the Scope.
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum VariableType {
    /// Normal variable.
    Normal,
    /// Immutable constant value.
    Constant,
}

/// An entry in the Scope.
pub struct ScopeEntry<'a> {
    /// Name of the variable.
    pub name: Cow<'a, str>,
    /// Type of the variable.
    pub var_type: VariableType,
    /// Current value of the variable.
    pub value: Dynamic,
    /// A constant expression if the initial value matches one of the recognized types.
    pub expr: Option<Expr>,
}

/// A type containing information about the current scope.
/// Useful for keeping state between `Engine` evaluation runs.
///
/// # Example
///
/// ```rust
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
/// When searching for variables, newly-added variables are found before similarly-named but older variables,
/// allowing for automatic _shadowing_ of variables.
pub struct Scope<'a>(Vec<ScopeEntry<'a>>);

impl<'a> Scope<'a> {
    /// Create a new Scope.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Empty the Scope.
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Get the number of variables inside the Scope.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Add (push) a new variable to the Scope.
    pub fn push<K: Into<Cow<'a, str>>, T: Any>(&mut self, name: K, value: T) {
        let value = value.into_dynamic();

        // Map into constant expressions
        //let (expr, value) = map_dynamic_to_expr(value, Position::none());

        self.0.push(ScopeEntry {
            name: name.into(),
            var_type: VariableType::Normal,
            value,
            expr: None,
        });
    }

    /// Add (push) a new constant to the Scope.
    ///
    /// Constants are immutable and cannot be assigned to.  Their values never change.
    /// Constants propagation is a technique used to optimize an AST.
    /// However, in order to be used for optimization, constants must be in one of the recognized types:
    /// `INT` (default to `i64`, `i32` if `only_i32`), `f64`, `String`, `char` and `bool`.
    pub fn push_constant<K: Into<Cow<'a, str>>, T: Any>(&mut self, name: K, value: T) {
        let value = value.into_dynamic();

        // Map into constant expressions
        let (expr, value) = map_dynamic_to_expr(value, Position::none());

        self.0.push(ScopeEntry {
            name: name.into(),
            var_type: VariableType::Constant,
            value,
            expr,
        });
    }

    /// Add (push) a new variable with a `Dynamic` value to the Scope.
    pub(crate) fn push_dynamic<K: Into<Cow<'a, str>>>(
        &mut self,
        name: K,
        var_type: VariableType,
        value: Dynamic,
    ) {
        let (expr, value) = map_dynamic_to_expr(value, Position::none());

        self.0.push(ScopeEntry {
            name: name.into(),
            var_type,
            value,
            expr,
        });
    }

    /// Remove (pop) the last variable from the Scope.
    pub fn pop(&mut self) -> Option<(String, VariableType, Dynamic)> {
        self.0.pop().map(
            |ScopeEntry {
                 name,
                 var_type,
                 value,
                 ..
             }| (name.to_string(), var_type, value),
        )
    }

    /// Truncate (rewind) the Scope to a previous size.
    pub fn rewind(&mut self, size: usize) {
        self.0.truncate(size);
    }

    /// Find a variable in the Scope, starting from the last.
    pub fn get(&self, key: &str) -> Option<(usize, &str, VariableType, Dynamic)> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, ScopeEntry { name, .. })| name == key)
            .map(
                |(
                    i,
                    ScopeEntry {
                        name,
                        var_type,
                        value,
                        ..
                    },
                )| (i, name.as_ref(), *var_type, value.clone()),
            )
    }

    /// Get the value of a variable in the Scope, starting from the last.
    pub fn get_value<T: Any + Clone>(&self, key: &str) -> Option<T> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, ScopeEntry { name, .. })| name == key)
            .and_then(|(_, ScopeEntry { value, .. })| value.downcast_ref::<T>())
            .map(T::clone)
    }

    /// Get a mutable reference to a variable in the Scope.
    pub(crate) fn get_mut(&mut self, name: &str, index: usize) -> &mut Dynamic {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");

        assert_ne!(
            entry.var_type,
            VariableType::Constant,
            "get mut of constant variable"
        );
        assert_eq!(entry.name, name, "incorrect key at Scope entry");

        &mut entry.value
    }

    /// Get a mutable reference to a variable in the Scope and downcast it to a specific type
    #[cfg(not(feature = "no_index"))]
    pub(crate) fn get_mut_by_type<T: Any + Clone>(&mut self, name: &str, index: usize) -> &mut T {
        self.get_mut(name, index)
            .downcast_mut::<T>()
            .expect("wrong type cast")
    }

    /// Get an iterator to variables in the Scope.
    pub fn iter(&self) -> impl Iterator<Item = &ScopeEntry> {
        self.0.iter().rev() // Always search a Scope in reverse order
    }
}

impl<'a, K> std::iter::Extend<(K, VariableType, Dynamic)> for Scope<'a>
where
    K: Into<Cow<'a, str>>,
{
    fn extend<T: IntoIterator<Item = (K, VariableType, Dynamic)>>(&mut self, iter: T) {
        self.0
            .extend(iter.into_iter().map(|(name, var_type, value)| ScopeEntry {
                name: name.into(),
                var_type,
                value,
                expr: None,
            }));
    }
}
