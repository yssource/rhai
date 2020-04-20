use super::{
    create_new_package, reg_binary, reg_binary_mut, reg_unary_mut, Package, PackageLibrary,
    PackageLibraryStore,
};

use crate::any::Dynamic;
use crate::engine::Map;
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::ops::Deref;

pub struct BasicMapPackage(PackageLibrary);

impl Deref for BasicMapPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for BasicMapPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }

    fn init(lib: &mut PackageLibraryStore) {
        // Register map functions
        #[cfg(not(feature = "no_object"))]
        {
            reg_binary_mut(
                lib,
                "has",
                |map: &mut Map, prop: String| map.contains_key(&prop),
                map,
            );
            reg_unary_mut(lib, "len", |map: &mut Map| map.len() as INT, map);
            reg_unary_mut(lib, "clear", |map: &mut Map| map.clear(), map);
            reg_binary_mut(
                lib,
                "remove",
                |x: &mut Map, name: String| x.remove(&name).unwrap_or_else(|| Dynamic::from_unit()),
                map,
            );
            reg_binary_mut(
                lib,
                "mixin",
                |map1: &mut Map, map2: Map| {
                    map2.into_iter().for_each(|(key, value)| {
                        map1.insert(key, value);
                    });
                },
                map,
            );
            reg_binary(
                lib,
                "+",
                |mut map1: Map, map2: Map| {
                    map2.into_iter().for_each(|(key, value)| {
                        map1.insert(key, value);
                    });
                    map1
                },
                map,
            );
        }
    }
}
