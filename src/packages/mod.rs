//! This module contains all built-in _packages_ available to Rhai, plus facilities to define custom packages.

use crate::engine::{FnAny, IteratorFn};

use crate::stdlib::{any::TypeId, boxed::Box, collections::HashMap, rc::Rc, sync::Arc, vec::Vec};

mod arithmetic;
mod array_basic;
mod iter_basic;
mod logic;
mod map_basic;
mod math_basic;
mod pkg_core;
mod pkg_std;
mod string_basic;
mod string_more;
mod time_basic;
mod utils;

pub use arithmetic::ArithmeticPackage;
#[cfg(not(feature = "no_index"))]
pub use array_basic::BasicArrayPackage;
pub use iter_basic::BasicIteratorPackage;
pub use logic::LogicPackage;
#[cfg(not(feature = "no_object"))]
pub use map_basic::BasicMapPackage;
pub use math_basic::BasicMathPackage;
pub use pkg_core::CorePackage;
pub use pkg_std::StandardPackage;
pub use string_basic::BasicStringPackage;
pub use string_more::MoreStringPackage;
#[cfg(not(feature = "no_std"))]
pub use time_basic::BasicTimePackage;

pub use utils::*;

/// Trait that all packages must implement.
pub trait Package {
    /// Create a new instance of a package.
    fn new() -> Self;

    /// Register all the functions in a package into a store.
    fn init(lib: &mut PackageStore);

    /// Retrieve the generic package library from this package.
    fn get(&self) -> PackageLibrary;
}

/// Type to store all functions in the package.
#[derive(Default)]
pub struct PackageStore {
    /// All functions, keyed by a hash created from the function name and parameter types.
    pub functions: HashMap<u64, Box<FnAny>>,

    /// All iterator functions, keyed by the type producing the iterator.
    pub type_iterators: HashMap<TypeId, Box<IteratorFn>>,
}

impl PackageStore {
    /// Create a new `PackageStore`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Get an iterator over the keys of the functions in the `PackageStore`.
    pub fn function_keys(&self) -> impl Iterator<Item = &u64> {
        self.functions.keys()
    }
    /// Get an iterator over the `TypeId` of the type iterators in the `PackageStore`.
    pub fn type_iterator_keys(&self) -> impl Iterator<Item = &TypeId> {
        self.type_iterators.keys()
    }
    /// Does the specified function hash key exist in the `PackageStore`?
    pub fn contains_function(&self, hash: u64) -> bool {
        self.functions.contains_key(&hash)
    }
    /// Get specified function via its hash key.
    pub fn get_function(&self, hash: u64) -> Option<&Box<FnAny>> {
        self.functions.get(&hash)
    }
    /// Does the specified TypeId iterator exist in the `PackageStore`?
    pub fn contains_iterator(&self, id: TypeId) -> bool {
        self.type_iterators.contains_key(&id)
    }
    /// Get the specified TypeId iterator.
    pub fn get_iterator(&self, id: TypeId) -> Option<&Box<IteratorFn>> {
        self.type_iterators.get(&id)
    }
}

/// Type which `Rc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(not(feature = "sync"))]
pub type PackageLibrary = Rc<PackageStore>;

/// Type which `Arc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(feature = "sync")]
pub type PackageLibrary = Arc<PackageStore>;

#[derive(Default)]
/// Type containing a collection of `PackageLibrary` instances.
/// All function and type iterator keys in the loaded packages are indexed for fast access.
pub(crate) struct PackagesCollection {
    /// Collection of `PackageLibrary` instances.
    packages: Vec<PackageLibrary>,
    /// Index of all function keys, pointing to the offset in `packages`.
    function_keys: HashMap<u64, usize>,
    /// Index of all type iterator `TypeId`'s, pointing to the offset in `packages`.
    iterator_types: HashMap<TypeId, usize>,
}

impl PackagesCollection {
    /// Add a `PackageLibrary` into the `PackagesCollection`.
    pub fn push(&mut self, package: PackageLibrary) {
        let index = self.packages.len();
        package.function_keys().for_each(|&hash| {
            self.function_keys.insert(hash, index);
        });
        package.type_iterator_keys().for_each(|&id| {
            self.iterator_types.insert(id, index);
        });
        self.packages.push(package);
    }
    /// Does the specified function hash key exist in the `PackagesCollection`?
    pub fn contains_function(&self, hash: u64) -> bool {
        self.function_keys.contains_key(&hash)
    }
    /// Get specified function via its hash key.
    pub fn get_function(&self, hash: u64) -> Option<&Box<FnAny>> {
        self.function_keys
            .get(&hash)
            .and_then(|&index| self.packages[index].functions.get(&hash))
    }
    /// Does the specified TypeId iterator exist in the `PackagesCollection`?
    pub fn contains_iterator(&self, id: TypeId) -> bool {
        self.iterator_types.contains_key(&id)
    }
    /// Get the specified TypeId iterator.
    pub fn get_iterator(&self, id: TypeId) -> Option<&Box<IteratorFn>> {
        self.iterator_types
            .get(&id)
            .and_then(|&index| self.packages[index].type_iterators.get(&id))
    }
}
