//! Module that defines the `Scope` type representing a function call-stack scope.

use crate::any::{Any, Dynamic};

use std::borrow::Cow;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum VariableType {
    Normal,
    Constant,
}

/// A type containing information about current scope.
/// Useful for keeping state between `Engine` runs.
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
pub struct Scope<'a>(Vec<(Cow<'a, str>, VariableType, Dynamic)>);

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
    pub fn push<K: Into<Cow<'a, str>>, T: Any>(&mut self, key: K, value: T) {
        self.0
            .push((key.into(), VariableType::Normal, Box::new(value)));
    }

    /// Add (push) a new constant to the Scope.
    pub fn push_constant<K: Into<Cow<'a, str>>, T: Any>(&mut self, key: K, value: T) {
        self.0
            .push((key.into(), VariableType::Constant, Box::new(value)));
    }

    /// Add (push) a new variable with a `Dynamic` value to the Scope.
    pub(crate) fn push_dynamic<K: Into<Cow<'a, str>>>(
        &mut self,
        key: K,
        val_type: VariableType,
        value: Dynamic,
    ) {
        self.0.push((key.into(), val_type, value));
    }

    /// Remove (pop) the last variable from the Scope.
    pub fn pop(&mut self) -> Option<(String, VariableType, Dynamic)> {
        self.0
            .pop()
            .map(|(key, var_type, value)| (key.to_string(), var_type, value))
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
            .find(|(_, (name, _, _))| name == key)
            .map(|(i, (name, var_type, value))| (i, name.as_ref(), *var_type, value.clone()))
    }

    /// Get the value of a variable in the Scope, starting from the last.
    pub fn get_value<T: Any + Clone>(&self, key: &str) -> Option<T> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, (name, _, _))| name == key)
            .and_then(|(_, (_, _, value))| value.downcast_ref::<T>())
            .map(|value| value.clone())
    }

    /// Get a mutable reference to a variable in the Scope.
    pub(crate) fn get_mut(&mut self, key: &str, index: usize) -> &mut Dynamic {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");

        assert_ne!(
            entry.1,
            VariableType::Constant,
            "get mut of constant variable"
        );
        assert_eq!(entry.0, key, "incorrect key at Scope entry");

        &mut entry.2
    }

    /// Get a mutable reference to a variable in the Scope and downcast it to a specific type
    #[cfg(not(feature = "no_index"))]
    pub(crate) fn get_mut_by_type<T: Any + Clone>(&mut self, key: &str, index: usize) -> &mut T {
        self.get_mut(key, index)
            .downcast_mut::<T>()
            .expect("wrong type cast")
    }

    /// Get an iterator to variables in the Scope.
    pub fn iter(&self) -> impl Iterator<Item = (&str, VariableType, &Dynamic)> {
        self.0
            .iter()
            .rev() // Always search a Scope in reverse order
            .map(|(key, var_type, value)| (key.as_ref(), *var_type, value))
    }

    /*
    /// Get a mutable iterator to variables in the Scope.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut Dynamic)> {
        self.0
            .iter_mut()
            .rev() // Always search a Scope in reverse order
            .map(|(key, value)| (key.as_ref(), value))
    }
    */
}

impl<'a, K> std::iter::Extend<(K, VariableType, Dynamic)> for Scope<'a>
where
    K: Into<Cow<'a, str>>,
{
    fn extend<T: IntoIterator<Item = (K, VariableType, Dynamic)>>(&mut self, iter: T) {
        self.0.extend(
            iter.into_iter()
                .map(|(key, var_type, value)| (key.into(), var_type, value)),
        );
    }
}
