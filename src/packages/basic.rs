use super::{
    create_new_package, reg_binary, reg_binary_mut, reg_unary, Package, PackageLibrary,
    PackageLibraryStore,
};

use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::ops::Deref;

pub struct BasicPackage(PackageLibrary);

impl Deref for BasicPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for BasicPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }

    fn init(lib: &mut PackageLibraryStore) {}
}
