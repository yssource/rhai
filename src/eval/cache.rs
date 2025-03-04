//! System caches.

use crate::func::CallableFunction;
use crate::{Identifier, StaticVec};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{collections::BTreeMap, marker::PhantomData};

/// _(internals)_ An entry in a function resolution cache.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone)]
pub struct FnResolutionCacheEntry {
    /// Function.
    pub func: CallableFunction,
    /// Optional source.
    pub source: Option<Box<Identifier>>,
}

/// _(internals)_ A function resolution cache.
/// Exported under the `internals` feature only.
///
/// [`FnResolutionCacheEntry`] is [`Box`]ed in order to pack as many entries inside a single B-Tree
/// level as possible.
pub type FnResolutionCache = BTreeMap<u64, Option<FnResolutionCacheEntry>>;

/// _(internals)_ A type containing system-wide caches.
/// Exported under the `internals` feature only.
///
/// The following caches are contained inside this type:
/// * A stack of [function resolution caches][FnResolutionCache]
#[derive(Debug, Clone)]
pub struct Caches<'a> {
    /// Stack of [function resolution caches][FnResolutionCache].
    fn_resolution: StaticVec<FnResolutionCache>,
    /// Take care of the lifetime parameter.
    dummy: PhantomData<&'a ()>,
}

impl Caches<'_> {
    /// Create an empty [`Caches`].
    #[inline(always)]
    #[must_use]
    pub const fn new() -> Self {
        Self {
            fn_resolution: StaticVec::new_const(),
            dummy: PhantomData,
        }
    }
    /// Get the number of function resolution cache(s) in the stack.
    #[inline(always)]
    #[must_use]
    pub fn fn_resolution_caches_len(&self) -> usize {
        self.fn_resolution.len()
    }
    /// Get a mutable reference to the current function resolution cache.
    #[inline]
    #[must_use]
    pub fn fn_resolution_cache_mut(&mut self) -> &mut FnResolutionCache {
        if self.fn_resolution.is_empty() {
            // Push a new function resolution cache if the stack is empty
            self.push_fn_resolution_cache();
        }
        self.fn_resolution.last_mut().unwrap()
    }
    /// Push an empty function resolution cache onto the stack and make it current.
    #[allow(dead_code)]
    #[inline(always)]
    pub fn push_fn_resolution_cache(&mut self) {
        self.fn_resolution.push(BTreeMap::new());
    }
    /// Rewind the function resolution caches stack to a particular size.
    #[inline(always)]
    pub fn rewind_fn_resolution_caches(&mut self, len: usize) {
        self.fn_resolution.truncate(len);
    }
}
