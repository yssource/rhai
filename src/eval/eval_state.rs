//! Evaluation state.

use crate::func::call::FnResolutionCache;
use crate::StaticVec;
use std::collections::BTreeMap;
use std::marker::PhantomData;
#[cfg(feature = "no_std")]
use std::prelude::v1::*;

/// _(internals)_ A type that holds all the current states of the [`Engine`][crate::Engine].
/// Exported under the `internals` feature only.
#[derive(Debug, Clone)]
pub struct EvalState<'a> {
    /// Force a [`Scope`][crate::Scope] search by name.
    ///
    /// Normally, access to variables are parsed with a relative offset into the
    /// [`Scope`][crate::Scope] to avoid a lookup.
    ///
    /// In some situation, e.g. after running an `eval` statement, or after a custom syntax
    /// statement, subsequent offsets may become mis-aligned.
    ///
    /// When that happens, this flag is turned on.
    pub always_search_scope: bool,
    /// Level of the current scope.
    ///
    /// The global (root) level is zero, a new block (or function call) is one level higher, and so on.
    pub scope_level: usize,
    /// Stack of function resolution caches.
    fn_resolution_caches: StaticVec<FnResolutionCache>,
    /// Take care of the lifetime parameter
    dummy: PhantomData<&'a ()>,
}

impl EvalState<'_> {
    /// Create a new [`EvalState`].
    #[inline(always)]
    #[must_use]
    pub fn new() -> Self {
        Self {
            always_search_scope: false,
            scope_level: 0,
            fn_resolution_caches: StaticVec::new_const(),
            dummy: PhantomData::default(),
        }
    }
    /// Get the number of function resolution cache(s) in the stack.
    #[inline(always)]
    #[must_use]
    pub fn fn_resolution_caches_len(&self) -> usize {
        self.fn_resolution_caches.len()
    }
    /// Get a mutable reference to the current function resolution cache.
    #[inline]
    #[must_use]
    pub fn fn_resolution_cache_mut(&mut self) -> &mut FnResolutionCache {
        if self.fn_resolution_caches.is_empty() {
            // Push a new function resolution cache if the stack is empty
            self.push_fn_resolution_cache();
        }
        self.fn_resolution_caches.last_mut().unwrap()
    }
    /// Push an empty function resolution cache onto the stack and make it current.
    #[allow(dead_code)]
    #[inline(always)]
    pub fn push_fn_resolution_cache(&mut self) {
        self.fn_resolution_caches.push(BTreeMap::new());
    }
    /// Rewind the function resolution caches stack to a particular size.
    #[inline(always)]
    pub fn rewind_fn_resolution_caches(&mut self, len: usize) {
        self.fn_resolution_caches.truncate(len);
    }
}
