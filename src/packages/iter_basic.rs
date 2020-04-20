use super::{
    create_new_package, reg_binary, reg_trinary, reg_unary_mut, Package, PackageLibrary,
    PackageLibraryStore,
};

use crate::any::{Dynamic, Union, Variant};
use crate::engine::{Array, Map};
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::{
    any::TypeId,
    ops::{Add, Deref, Range},
};

// Register range function
fn reg_range<T: Variant + Clone>(lib: &mut PackageLibraryStore)
where
    Range<T>: Iterator<Item = T>,
{
    lib.1.insert(
        TypeId::of::<Range<T>>(),
        Box::new(|source: &Dynamic| {
            Box::new(
                source
                    .downcast_ref::<Range<T>>()
                    .cloned()
                    .unwrap()
                    .map(|x| x.into_dynamic()),
            ) as Box<dyn Iterator<Item = Dynamic>>
        }),
    );
}

// Register range function with step
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct StepRange<T>(T, T, T)
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd;

impl<T> Iterator for StepRange<T>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.0 < self.1 {
            let v = self.0.clone();
            self.0 = &v + &self.2;
            Some(v)
        } else {
            None
        }
    }
}

fn reg_step<T>(lib: &mut PackageLibraryStore)
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
    StepRange<T>: Iterator<Item = T>,
{
    lib.1.insert(
        TypeId::of::<StepRange<T>>(),
        Box::new(|source: &Dynamic| {
            Box::new(
                source
                    .downcast_ref::<StepRange<T>>()
                    .cloned()
                    .unwrap()
                    .map(|x| x.into_dynamic()),
            ) as Box<dyn Iterator<Item = Dynamic>>
        }),
    );
}

pub struct BasicIteratorPackage(PackageLibrary);

impl Deref for BasicIteratorPackage {
    type Target = PackageLibrary;

    fn deref(&self) -> &PackageLibrary {
        &self.0
    }
}

impl Package for BasicIteratorPackage {
    fn new() -> Self {
        let mut pkg = create_new_package();
        Self::init(&mut pkg);
        Self(pkg.into())
    }

    fn get(&self) -> PackageLibrary {
        self.0.clone()
    }

    fn init(lib: &mut PackageLibraryStore) {
        #[cfg(not(feature = "no_index"))]
        {
            // Register array iterator
            lib.1.insert(
                TypeId::of::<Array>(),
                Box::new(|a: &Dynamic| {
                    Box::new(a.downcast_ref::<Array>().unwrap().clone().into_iter())
                        as Box<dyn Iterator<Item = Dynamic>>
                }),
            );
        }

        // Register map access functions
        #[cfg(not(feature = "no_object"))]
        {
            fn map_get_keys(map: &mut Map) -> Vec<Dynamic> {
                map.iter()
                    .map(|(k, _)| Dynamic(Union::Str(Box::new(k.to_string()))))
                    .collect::<Vec<_>>()
            }
            fn map_get_values(map: &mut Map) -> Vec<Dynamic> {
                map.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>()
            }

            #[cfg(not(feature = "no_index"))]
            reg_unary_mut(lib, "keys", map_get_keys, map);

            #[cfg(not(feature = "no_index"))]
            reg_unary_mut(lib, "values", map_get_values, map);
        }

        fn get_range<T>(from: T, to: T) -> Range<T> {
            from..to
        }

        reg_range::<INT>(lib);
        reg_binary(lib, "range", get_range::<INT>, map);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            macro_rules! reg_range {
                ($self:expr, $x:expr, $( $y:ty ),*) => (
                    $(
                        reg_range::<$y>($self);
                        reg_binary($self, $x, get_range::<$y>, map);
                    )*
                )
            }

            reg_range!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }

        reg_step::<INT>(lib);
        reg_trinary(lib, "range", StepRange::<INT>, map);

        #[cfg(not(feature = "only_i32"))]
        #[cfg(not(feature = "only_i64"))]
        {
            macro_rules! reg_step {
                ($self:expr, $x:expr, $( $y:ty ),*) => (
                    $(
                        reg_step::<$y>($self);
                        reg_trinary($self, $x, StepRange::<$y>, map);
                    )*
                )
            }

            reg_step!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
        }
    }
}
