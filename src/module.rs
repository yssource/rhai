//! Module defining external-loaded modules for Rhai.

use crate::any::Dynamic;

use crate::stdlib::{
    collections::HashMap,
    ops::{Deref, DerefMut},
    string::String,
};

/// An imported module.
///
/// Not available under the `no_module` feature.
#[derive(Debug, Clone)]
pub struct Module(HashMap<String, Dynamic>);

impl Module {
    /// Create a new module.
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl Deref for Module {
    type Target = HashMap<String, Dynamic>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Module {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
