use crate::dynamic::Variant;
use crate::stdlib::{
    boxed::Box,
    ops::{Add, Range},
};
use crate::{def_package, EvalAltResult, INT};

fn get_range<T: Variant + Clone>(from: T, to: T) -> Result<Range<T>, Box<EvalAltResult>> {
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

fn get_step_range<T>(from: T, to: T, step: T) -> Result<StepRange<T>, Box<EvalAltResult>>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    Ok(StepRange::<T>(from, to, step))
}

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
macro_rules! reg_range {
    ($lib:expr, $x:expr, $( $y:ty ),*) => (
        $(
            $lib.set_iterator::<Range<$y>>();
            let hash = $lib.set_fn_2($x, get_range::<$y>);
            $lib.update_fn_param_names(hash, &[concat!("from: ", stringify!($y)), concat!("to: ", stringify!($y))]);
        )*
    )
}

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
macro_rules! reg_step {
    ($lib:expr, $x:expr, $( $y:ty ),*) => (
        $(
            $lib.set_iterator::<StepRange<$y>>();
            let hash = $lib.set_fn_3($x, get_step_range::<$y>);
            $lib.update_fn_param_names(hash, &[concat!("from: ", stringify!($y)), concat!("to: ", stringify!($y)), concat!("step: ", stringify!($y))]);
        )*
    )
}

def_package!(crate:BasicIteratorPackage:"Basic range iterators.", lib, {
    lib.set_iterator::<Range<INT>>();
    let hash = lib.set_fn_2("range", get_range::<INT>);
    lib.update_fn_param_names(hash, &["from: INT", "to: INT"]);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_range!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_range!(lib, "range", i128, u128);
        }
    }

    lib.set_iterator::<StepRange<INT>>();
    let hash = lib.set_fn_3("range", get_step_range::<INT>);
    lib.update_fn_param_names(hash, &["from: INT", "to: INT", "step: INT"]);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_step!(lib, "range", i8, u8, i16, u16, i32, i64, u32, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_step!(lib, "range", i128, u128);
        }
    }
});
