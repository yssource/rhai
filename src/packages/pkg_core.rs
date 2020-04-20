use super::arithmetic::ArithmeticPackage;
use super::create_new_package;
use super::iter_basic::BasicIteratorPackage;
use super::logic::LogicPackage;
use super::string_basic::BasicStringPackage;
use super::{Package, PackageLibrary, PackageLibraryStore};

use crate::stdlib::ops::Deref;

pub struct CorePackage(PackageLibrary);

impl Deref for CorePackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for CorePackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn init(lib: &mut PackageLibraryStore) {
        ArithmeticPackage::init(lib);
        LogicPackage::init(lib);
        BasicStringPackage::init(lib);
        BasicIteratorPackage::init(lib);
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }
}
