use crate::any::{Any, Dynamic};

/// A type containing information about current scope.
/// Useful for keeping state between `Engine` runs
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
/// Between runs, `Engine` only remembers functions when not using own `Scope`.

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
            .rev()
            .find(|(_, (n, _))| n == key)
            .map(|(i, (n, v))| (i, n.clone(), v.clone()))
    }

    /// Get the value of a variable in the Scope, starting from the last.
    pub fn get_value<T: Any + Clone>(&self, key: &str) -> Option<T> {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (n, _))| n == key)
            .map(|(_, (_, v))| v.downcast_ref() as Option<&T>)
            .map(|v| v.unwrap().clone())
    }

    /// Get a mutable reference to a variable in the Scope.
    pub(crate) fn get_mut(&mut self, key: &str, index: usize) -> &mut Dynamic {
        let entry = self.0.get_mut(index).expect("invalid index in Scope");

        if entry.0 != key {
            panic!("incorrect key at Scope entry");
        }

        &mut entry.1
    }

    /// Get an iterator to variables in the Scope.
    pub fn iter(&self) -> std::slice::Iter<(String, Dynamic)> {
        self.0.iter()
    }

    /// Get a mutable iterator to variables in the Scope.
    pub(crate) fn iter_mut(&mut self) -> std::slice::IterMut<(String, Dynamic)> {
        self.0.iter_mut()
    }
}

impl std::iter::Extend<(String, Dynamic)> for Scope {
    fn extend<T: IntoIterator<Item = (String, Dynamic)>>(&mut self, iter: T) {
        self.0.extend(iter);
    }
}
