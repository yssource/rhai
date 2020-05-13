use crate::any::{Dynamic, Variant};
use crate::def_package;
use crate::module::{FuncReturn, Module};
use crate::parser::INT;

use crate::stdlib::{
    any::TypeId,
    boxed::Box,
    ops::{Add, Range},
};

// Register range function
fn reg_range<T: Variant + Clone>(lib: &mut Module)
where
    Range<T>: Iterator<Item = T>,
{
    lib.set_iter(
        TypeId::of::<Range<T>>(),
        Box::new(|source| {
            Box::new(source.cast::<Range<T>>().map(|x| x.into_dynamic()))
                as Box<dyn Iterator<Item = Dynamic>>
        }),
    );
}

fn get_range<T: Variant + Clone>(from: T, to: T) -> FuncReturn<Range<T>> {
    Ok(from..to)
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

fn reg_step<T>(lib: &mut Module)
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
    StepRange<T>: Iterator<Item = T>,
{
    lib.set_iter(
        TypeId::of::<StepRange<T>>(),
        Box::new(|source| {
            Box::new(source.cast::<StepRange<T>>().map(|x| x.into_dynamic()))
                as Box<dyn Iterator<Item = Dynamic>>
        }),
    );
}

fn get_step_range<T>(from: T, to: T, step: T) -> FuncReturn<StepRange<T>>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    Ok(StepRange::<T>(from, to, step))
}

def_package!(crate:BasicIteratorPackage:"Basic range iterators.", lib, {
    reg_range::<INT>(lib);
    lib.set_fn_2("range", get_range::<INT>);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        macro_rules! reg_range {
            ($lib:expr, $x:expr, $( $y:ty ),*) => (
                $(
                    reg_range::<$y>($lib);
                    $lib.set_fn_2($x, get_range::<$y>);
                )*
            )
        }

        reg_range!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
    }

    reg_step::<INT>(lib);
    lib.set_fn_3("range", get_step_range::<INT>);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        macro_rules! reg_step {
            ($lib:expr, $x:expr, $( $y:ty ),*) => (
                $(
                    reg_step::<$y>($lib);
                    $lib.set_fn_3($x, get_step_range::<$y>);
                )*
            )
        }

        reg_step!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64, i128, u128);
    }
});
