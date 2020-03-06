use crate::any::{Any, Dynamic};

/// A type containing information about current scope.
/// Useful for keeping state between `Engine` runs
///
/// # Example
///
/// ```rust
/// use rhai::{Engine, Scope};
///
/// let mut engine = Engine::new();
/// let mut my_scope = Scope::new();
///
/// assert!(engine.eval_with_scope::<()>(&mut my_scope, "let x = 5;").is_ok());
/// assert_eq!(engine.eval_with_scope::<i64>(&mut my_scope, "x + 1").unwrap(), 6);
/// ```
///
/// When searching for variables, newly-added variables are found before similarly-named but older variables,
/// allowing for automatic _shadowing_ of variables.
pub struct Scope(Vec<(String, Dynamic)>);

impl Scope {
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
    pub fn push<T: Any>(&mut self, key: String, value: T) {
        self.0.push((key, Box::new(value)));
    }

    /// Add (push) a new variable to the Scope.
    pub(crate) fn push_dynamic(&mut self, key: String, value: Dynamic) {
        self.0.push((key, value));
    }

    /// Remove (pop) the last variable from the Scope.
    pub fn pop(&mut self) -> Option<(String, Dynamic)> {
        self.0.pop()
    }

    /// Truncate (rewind) the Scope to a previous size.
    pub fn rewind(&mut self, size: usize) {
        self.0.truncate(size);
    }

    /// Find a variable in the Scope, starting from the last.
    pub fn get(&self, key: &str) -> Option<(usize, String, Dynamic)> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, (name, _))| name == key)
            .map(|(i, (name, value))| (i, name.clone(), value.clone()))
    }

    /// Get the value of a variable in the Scope, starting from the last.
    pub fn get_value<T: Any + Clone>(&self, key: &str) -> Option<T> {
        self.0
            .iter()
            .enumerate()
            .rev() // Always search a Scope in reverse order
            .find(|(_, (name, _))| name == key)
            .and_then(|(_, (_, value))| value.downcast_ref::<T>())
            .map(|value| value.clone())
    }

    /// Get a mutable reference to a variable in the Scope.
    pub(crate) fn get_mut(&mut self, key: &str, index: usize) -> &mut Dynamic {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");

        assert_eq!(entry.0, key, "incorrect key at Scope entry");

        &mut entry.1
    }

    /// Get a mutable reference to a variable in the Scope and downcast it to a specific type
    pub(crate) fn get_mut_by_type<T: Any + Clone>(&mut self, key: &str, index: usize) -> &mut T {
        self.get_mut(key, index)
            .downcast_mut::<T>()
            .expect("wrong type cast")
    }

    /// Get an iterator to variables in the Scope.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &Dynamic)> {
        self.0
            .iter()
            .rev() // Always search a Scope in reverse order
            .map(|(key, value)| (key.as_str(), value))
    }

    /// Get a mutable iterator to variables in the Scope.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut Dynamic)> {
        self.0
            .iter_mut()
            .rev() // Always search a Scope in reverse order
            .map(|(key, value)| (key.as_str(), value))
    }
}

impl std::iter::Extend<(String, Dynamic)> for Scope {
    fn extend<T: IntoIterator<Item = (String, Dynamic)>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}
