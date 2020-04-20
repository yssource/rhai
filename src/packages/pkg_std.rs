use super::array_basic::BasicArrayPackage;
use super::map_basic::BasicMapPackage;
use super::math_basic::BasicMathPackage;
use super::pkg_core::CorePackage;
use super::string_more::MoreStringPackage;
use super::time_basic::BasicTimePackage;
use super::{create_new_package, Package, PackageLibrary, PackageLibraryStore};

use crate::stdlib::ops::Deref;

pub struct StandardPackage(PackageLibrary);

impl Deref for StandardPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for StandardPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn init(lib: &mut PackageLibraryStore) {
        CorePackage::init(lib);
        BasicMathPackage::init(lib);
        BasicArrayPackage::init(lib);
        BasicMapPackage::init(lib);
        BasicTimePackage::init(lib);
        MoreStringPackage::init(lib);
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }
}
