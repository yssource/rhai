//! Global runtime state.

use crate::func::{CallableFunction, IteratorFn};
use crate::{Identifier, Module, Shared, StaticVec};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::TypeId,
    fmt,
    iter::{FromIterator, Rev, Zip},
    marker::PhantomData,
};

/// _(internals)_ A stack of imported [modules][Module] plus mutable global runtime states.
/// Exported under the `internals` feature only.
//
// # Implementation Notes
//
// This implementation splits the module names from the shared modules to improve data locality.
// Most usage will be looking up a particular key from the list and then getting the module that
// corresponds to that key.
#[derive(Clone)]
pub struct GlobalRuntimeState<'a> {
    /// Stack of module names.
    //
    // We cannot use Cow<str> here because `eval` may load a [module][Module] and
    // the module name will live beyond the AST of the eval script text.
    keys: StaticVec<Identifier>,
    /// Stack of imported [modules][Module].
    modules: StaticVec<Shared<Module>>,
    /// Source of the current context.
    /// No source if the string is empty.
    pub source: Identifier,
    /// Number of operations performed.
    pub num_operations: u64,
    /// Number of modules loaded.
    pub num_modules_loaded: usize,
    /// Function call hashes to index getters and setters.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    fn_hash_indexing: (u64, u64),
    /// Embedded [module][Module] resolver.
    #[cfg(not(feature = "no_module"))]
    pub embedded_module_resolver: Option<Shared<crate::module::resolvers::StaticModuleResolver>>,
    /// Cache of globally-defined constants.
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    constants:
        Option<Shared<crate::Locked<std::collections::BTreeMap<Identifier, crate::Dynamic>>>>,
    /// Take care of the lifetime parameter.
    dummy: PhantomData<&'a ()>,
}

impl Default for GlobalRuntimeState<'_> {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalRuntimeState<'_> {
    /// Create a new [`GlobalRuntimeState`].
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            keys: StaticVec::new_const(),
            modules: StaticVec::new_const(),
            source: Identifier::new_const(),
            num_operations: 0,
            num_modules_loaded: 0,
            #[cfg(not(feature = "no_module"))]
            embedded_module_resolver: None,
            #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
            fn_hash_indexing: (0, 0),
            #[cfg(not(feature = "no_module"))]
            #[cfg(not(feature = "no_function"))]
            constants: None,
            dummy: PhantomData::default(),
        }
    }
    /// Get the length of the stack of globally-imported [modules][Module].
    #[inline(always)]
    #[must_use]
    pub fn num_imports(&self) -> usize {
        self.keys.len()
    }
    /// Get the globally-imported [module][Module] at a particular index.
    #[inline(always)]
    #[must_use]
    pub fn get_shared_import(&self, index: usize) -> Option<Shared<Module>> {
        self.modules.get(index).cloned()
    }
    /// Get a mutable reference to the globally-imported [module][Module] at a particular index.
    #[allow(dead_code)]
    #[inline(always)]
    #[must_use]
    pub(crate) fn get_shared_import_mut(&mut self, index: usize) -> Option<&mut Shared<Module>> {
        self.modules.get_mut(index)
    }
    /// Get the index of a globally-imported [module][Module] by name.
    #[inline]
    #[must_use]
    pub fn find_import(&self, name: &str) -> Option<usize> {
        let len = self.keys.len();

        self.keys.iter().rev().enumerate().find_map(|(i, key)| {
            if key == name {
                Some(len - 1 - i)
            } else {
                None
            }
        })
    }
    /// Push an imported [module][Module] onto the stack.
    #[inline(always)]
    pub fn push_import(&mut self, name: impl Into<Identifier>, module: impl Into<Shared<Module>>) {
        self.keys.push(name.into());
        self.modules.push(module.into());
    }
    /// Truncate the stack of globally-imported [modules][Module] to a particular length.
    #[inline(always)]
    pub fn truncate_imports(&mut self, size: usize) {
        self.keys.truncate(size);
        self.modules.truncate(size);
    }
    /// Get an iterator to the stack of globally-imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    #[inline]
    pub fn iter_imports(&self) -> impl Iterator<Item = (&str, &Module)> {
        self.keys
            .iter()
            .rev()
            .zip(self.modules.iter().rev())
            .map(|(name, module)| (name.as_str(), module.as_ref()))
    }
    /// Get an iterator to the stack of globally-imported [modules][Module] in reverse order.
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn iter_imports_raw(&self) -> impl Iterator<Item = (&Identifier, &Shared<Module>)> {
        self.keys.iter().rev().zip(self.modules.iter().rev())
    }
    /// Get an iterator to the stack of globally-imported [modules][Module] in forward order.
    #[allow(dead_code)]
    #[inline]
    pub(crate) fn scan_imports_raw(&self) -> impl Iterator<Item = (&Identifier, &Shared<Module>)> {
        self.keys.iter().zip(self.modules.iter())
    }
    /// Does the specified function hash key exist in the stack of globally-imported [modules][Module]?
    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub fn contains_qualified_fn(&self, hash: u64) -> bool {
        self.modules.iter().any(|m| m.contains_qualified_fn(hash))
    }
    /// Get the specified function via its hash key from the stack of globally-imported [modules][Module].
    #[inline]
    #[must_use]
    pub fn get_qualified_fn(&self, hash: u64) -> Option<(&CallableFunction, Option<&str>)> {
        self.modules
            .iter()
            .rev()
            .find_map(|m| m.get_qualified_fn(hash).map(|f| (f, m.id())))
    }
    /// Does the specified [`TypeId`][std::any::TypeId] iterator exist in the stack of
    /// globally-imported [modules][Module]?
    #[allow(dead_code)]
    #[inline]
    #[must_use]
    pub fn contains_iter(&self, id: TypeId) -> bool {
        self.modules.iter().any(|m| m.contains_qualified_iter(id))
    }
    /// Get the specified [`TypeId`][std::any::TypeId] iterator from the stack of globally-imported
    /// [modules][Module].
    #[inline]
    #[must_use]
    pub fn get_iter(&self, id: TypeId) -> Option<IteratorFn> {
        self.modules
            .iter()
            .rev()
            .find_map(|m| m.get_qualified_iter(id))
    }
    /// Get a mutable reference to the cache of globally-defined constants.
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    #[must_use]
    pub(crate) fn constants_mut<'a>(
        &'a mut self,
    ) -> Option<
        impl std::ops::DerefMut<Target = std::collections::BTreeMap<Identifier, crate::Dynamic>> + 'a,
    > {
        if let Some(ref global_constants) = self.constants {
            Some(crate::func::native::shared_write_lock(global_constants))
        } else {
            None
        }
    }
    /// Set a constant into the cache of globally-defined constants.
    #[cfg(not(feature = "no_module"))]
    #[cfg(not(feature = "no_function"))]
    pub(crate) fn set_constant(&mut self, name: impl Into<Identifier>, value: crate::Dynamic) {
        if self.constants.is_none() {
            let dict: crate::Locked<_> = std::collections::BTreeMap::new().into();
            self.constants = Some(dict.into());
        }

        crate::func::native::shared_write_lock(self.constants.as_mut().expect("`Some`"))
            .insert(name.into(), value);
    }
    /// Get the pre-calculated index getter hash.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    #[must_use]
    pub(crate) fn hash_idx_get(&mut self) -> u64 {
        if self.fn_hash_indexing != (0, 0) {
            self.fn_hash_indexing.0
        } else {
            let n1 = crate::calc_fn_hash(crate::engine::FN_IDX_GET, 2);
            let n2 = crate::calc_fn_hash(crate::engine::FN_IDX_SET, 3);
            self.fn_hash_indexing = (n1, n2);
            n1
        }
    }
    /// Get the pre-calculated index setter hash.
    #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
    #[must_use]
    pub(crate) fn hash_idx_set(&mut self) -> u64 {
        if self.fn_hash_indexing != (0, 0) {
            self.fn_hash_indexing.1
        } else {
            let n1 = crate::calc_fn_hash(crate::engine::FN_IDX_GET, 2);
            let n2 = crate::calc_fn_hash(crate::engine::FN_IDX_SET, 3);
            self.fn_hash_indexing = (n1, n2);
            n2
        }
    }
}

impl IntoIterator for GlobalRuntimeState<'_> {
    type Item = (Identifier, Shared<Module>);
    type IntoIter =
        Zip<Rev<smallvec::IntoIter<[Identifier; 3]>>, Rev<smallvec::IntoIter<[Shared<Module>; 3]>>>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.keys
            .into_iter()
            .rev()
            .zip(self.modules.into_iter().rev())
    }
}

impl<K: Into<Identifier>, M: Into<Shared<Module>>> FromIterator<(K, M)> for GlobalRuntimeState<'_> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (K, M)>>(iter: T) -> Self {
        let mut lib = Self::new();
        lib.extend(iter);
        lib
    }
}

impl<K: Into<Identifier>, M: Into<Shared<Module>>> Extend<(K, M)> for GlobalRuntimeState<'_> {
    #[inline]
    fn extend<T: IntoIterator<Item = (K, M)>>(&mut self, iter: T) {
        iter.into_iter().for_each(|(k, m)| {
            self.keys.push(k.into());
            self.modules.push(m.into());
        })
    }
}

impl fmt::Debug for GlobalRuntimeState<'_> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("GlobalRuntimeState");

        f.field("imports", &self.keys.iter().zip(self.modules.iter()))
            .field("source", &self.source)
            .field("num_operations", &self.num_operations)
            .field("num_modules_loaded", &self.num_modules_loaded);

        #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
        f.field("fn_hash_indexing", &self.fn_hash_indexing);

        #[cfg(not(feature = "no_module"))]
        f.field("embedded_module_resolver", &self.embedded_module_resolver);

        #[cfg(not(feature = "no_module"))]
        #[cfg(not(feature = "no_function"))]
        f.field("constants", &self.constants);

        f.finish()
    }
}
