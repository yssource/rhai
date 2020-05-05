//! Module defining external-loaded modules for Rhai.

use crate::any::{Dynamic, Variant};
use crate::engine::{FnAny, FunctionsLib};
use crate::result::EvalAltResult;
use crate::token::Position;
use crate::utils::StaticVec;

use crate::stdlib::{collections::HashMap, fmt, string::String};

/// An imported module, which may contain variables, sub-modules,
/// external Rust functions, and script-defined functions.
///
/// Not available under the `no_module` feature.
#[derive(Default)]
pub struct Module {
    /// Sub-modules.
    modules: HashMap<String, Module>,
    /// Module variables, including sub-modules.
    variables: HashMap<String, Dynamic>,
    /// External Rust functions.
    functions: HashMap<u64, Box<FnAny>>,
    /// Script-defined functions.
    lib: FunctionsLib,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<module {:?}, functions={}, lib={}>",
            self.variables,
            self.functions.len(),
            self.lib.len()
        )
    }
}

impl Clone for Module {
    fn clone(&self) -> Self {
        // `Module` implements `Clone` so it can fit inside a `Dynamic`
        // but we should never actually clone it.
        unimplemented!()
    }
}

impl Module {
    /// Create a new module.
    pub fn new() -> Self {
        Default::default()
    }

    /// Does a variable exist in the module?
    pub fn contains_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Get the value of a module variable.
    pub fn get_variable_value<T: Variant + Clone>(&self, name: &str) -> Option<T> {
        self.get_variable(name).and_then(|v| v.try_cast::<T>())
    }

    /// Get a module variable.
    pub fn get_variable(&self, name: &str) -> Option<Dynamic> {
        self.variables.get(name).cloned()
    }

    /// Get a mutable reference to a module variable.
    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut Dynamic> {
        self.variables.get_mut(name)
    }

    /// Set a variable into the module.
    ///
    /// If there is an existing variable of the same name, it is replaced.
    pub fn set_variable<K: Into<String>, T: Into<Dynamic>>(&mut self, name: K, value: T) {
        self.variables.insert(name.into(), value.into());
    }

    /// Get a mutable reference to a modules-qualified variable.
    pub(crate) fn get_qualified_variable_mut(
        &mut self,
        name: &str,
        modules: &StaticVec<(String, Position)>,
        pos: Position,
    ) -> Result<&mut Dynamic, Box<EvalAltResult>> {
        Ok(self
            .get_qualified_module_mut(modules)?
            .get_variable_mut(name)
            .ok_or_else(|| Box::new(EvalAltResult::ErrorVariableNotFound(name.into(), pos)))?)
    }

    /// Does a sub-module exist in the module?
    pub fn contains_sub_module(&self, name: &str) -> bool {
        self.modules.contains_key(name)
    }

    /// Get a sub-module.
    pub fn get_sub_module(&self, name: &str) -> Option<&Module> {
        self.modules.get(name)
    }

    /// Get a mutable reference to a sub-module.
    pub fn get_sub_module_mut(&mut self, name: &str) -> Option<&mut Module> {
        self.modules.get_mut(name)
    }

    /// Set a sub-module into the module.
    ///
    /// If there is an existing sub-module of the same name, it is replaced.
    pub fn set_sub_module<K: Into<String>>(&mut self, name: K, sub_module: Module) {
        self.modules.insert(name.into(), sub_module.into());
    }

    /// Get a mutable reference to a modules chain.
    /// The first module is always skipped and assumed to be the same as `self`.
    pub(crate) fn get_qualified_module_mut(
        &mut self,
        modules: &StaticVec<(String, Position)>,
    ) -> Result<&mut Module, Box<EvalAltResult>> {
        let mut drain = modules.iter();
        drain.next().unwrap(); // Skip first module

        let mut module = self;

        for (id, id_pos) in drain {
            module = module
                .get_sub_module_mut(id)
                .ok_or_else(|| Box::new(EvalAltResult::ErrorModuleNotFound(id.into(), *id_pos)))?;
        }

        Ok(module)
    }
}
