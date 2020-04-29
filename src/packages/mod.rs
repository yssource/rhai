//! This module contains all built-in _packages_ available to Rhai, plus facilities to define custom packages.

use crate::engine::{FnAny, IteratorFn};

use crate::stdlib::{any::TypeId, boxed::Box, collections::HashMap, rc::Rc, sync::Arc};

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
pub struct PackageStore {
    /// All functions, keyed by a hash created from the function name and parameter types.
    pub functions: HashMap<u64, Box<FnAny>>,

    /// All iterator functions, keyed by the type producing the iterator.
    pub type_iterators: HashMap<TypeId, Box<IteratorFn>>,
}

impl PackageStore {
    /// Create a new `PackageStore`.
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            type_iterators: HashMap::new(),
        }
    }
}

/// Type which `Rc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(not(feature = "sync"))]
pub type PackageLibrary = Rc<PackageStore>;

/// Type which `Arc`-wraps a `PackageStore` to facilitate sharing library instances.
#[cfg(feature = "sync")]
pub type PackageLibrary = Arc<PackageStore>;
