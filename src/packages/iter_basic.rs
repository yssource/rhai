use super::{reg_binary, reg_trinary, reg_unary_mut, PackageStore};

use crate::any::{Dynamic, Union, Variant};
use crate::def_package;
use crate::engine::{Array, Map};
use crate::fn_register::map_dynamic as map;
use crate::parser::INT;

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    ops::{Add, Range},
};

// Register range function
fn reg_range<T: Variant + Clone>(lib: &mut PackageStore)
where
    Range<T>: Iterator<Item = T>,
{
    lib.type_iterators.insert(
        TypeId::of::<Range<T>>(),
        Box::new(|source: Dynamic| {
            Box::new(source.cast::<Range<T>>().map(|x| x.into_dynamic()))
                as Box<dyn Iterator<Item = Dynamic>>
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

fn reg_step<T>(lib: &mut PackageStore)
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
    StepRange<T>: Iterator<Item = T>,
{
    lib.type_iterators.insert(
        TypeId::of::<StepRange<T>>(),
        Box::new(|source: Dynamic| {
            Box::new(source.cast::<StepRange<T>>().map(|x| x.into_dynamic()))
                as Box<dyn Iterator<Item = Dynamic>>
        }),
    );
}

def_package!(crate:BasicIteratorPackage:"Basic range iterators.", lib, {
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
});
