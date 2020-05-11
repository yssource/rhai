//! This module contains all built-in _packages_ available to Rhai, plus facilities to define custom packages.

use crate::fn_native::{IteratorFn, NativeCallable};

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

const NUM_NATIVE_FUNCTIONS: usize = 512;

/// Trait that all packages must implement.
pub trait Package {
    /// Register all the functions in a package into a store.
    fn init(lib: &mut PackageStore);

    /// Retrieve the generic package library from this package.
    fn get(&self) -> PackageLibrary;
}

/// Type to store all functions in the package.
pub struct PackageStore {
    /// All functions, keyed by a hash created from the function name and parameter types.
    pub functions: HashMap<u64, Box<dyn NativeCallable>>,

    /// All iterator functions, keyed by the type producing the iterator.
    pub type_iterators: HashMap<TypeId, Box<IteratorFn>>,
}

impl PackageStore {
    /// Create a new `PackageStore`.
    pub fn new() -> Self {
        Default::default()
    }
    /// Does the specified function hash key exist in the `PackageStore`?
    pub fn contains_function(&self, hash: u64) -> bool {
        self.functions.contains_key(&hash)
    }
    /// Get specified function via its hash key.
    pub fn get_function(&self, hash: u64) -> Option<&Box<dyn NativeCallable>> {
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

impl Default for PackageStore {
    fn default() -> Self {
        Self {
            functions: HashMap::with_capacity(NUM_NATIVE_FUNCTIONS),
            type_iterators: HashMap::with_capacity(4),
        }
    }
}

/// Type which `Rc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(not(feature = "sync"))]
pub type PackageLibrary = Rc<PackageStore>;

/// Type which `Arc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(feature = "sync")]
pub type PackageLibrary = Arc<PackageStore>;

/// Type containing a collection of `PackageLibrary` instances.
/// All function and type iterator keys in the loaded packages are indexed for fast access.
#[derive(Clone, Default)]
pub(crate) struct PackagesCollection {
    /// Collection of `PackageLibrary` instances.
    packages: Vec<PackageLibrary>,
}

impl PackagesCollection {
    /// Add a `PackageLibrary` into the `PackagesCollection`.
    pub fn push(&mut self, package: PackageLibrary) {
        // Later packages override previous ones.
        self.packages.insert(0, package);
    }
    /// Does the specified function hash key exist in the `PackagesCollection`?
    pub fn contains_function(&self, hash: u64) -> bool {
        self.packages.iter().any(|p| p.contains_function(hash))
    }
    /// Get specified function via its hash key.
    pub fn get_function(&self, hash: u64) -> Option<&Box<dyn NativeCallable>> {
        self.packages
            .iter()
            .map(|p| p.get_function(hash))
            .find(|f| f.is_some())
            .flatten()
    }
    /// Does the specified TypeId iterator exist in the `PackagesCollection`?
    pub fn contains_iterator(&self, id: TypeId) -> bool {
        self.packages.iter().any(|p| p.contains_iterator(id))
    }
    /// Get the specified TypeId iterator.
    pub fn get_iterator(&self, id: TypeId) -> Option<&Box<IteratorFn>> {
        self.packages
            .iter()
            .map(|p| p.get_iterator(id))
            .find(|f| f.is_some())
            .flatten()
    }
}
