use crate::dynamic::Variant;
use crate::stdlib::{
    boxed::Box,
    ops::{Add, Range},
};
use crate::{def_package, EvalAltResult, Position, INT};

fn get_range<T: Variant + Clone>(from: T, to: T) -> Result<Range<T>, Box<EvalAltResult>> {
    Ok(from..to)
}

// Register range function with step
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct StepRange<T>(T, T, T)
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd;

impl<T> StepRange<T>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    pub fn new(from: T, to: T, step: T) -> Result<Self, Box<EvalAltResult>> {
        if &from + &step == from {
            Err(Box::new(EvalAltResult::ErrorArithmetic(
                "invalid step value".to_string(),
                Position::NONE,
            )))
        } else {
            Ok(Self(from, to, step))
        }
    }
}

impl<T> Iterator for StepRange<T>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.0 == self.1 {
            None
        } else if self.0 < self.1 {
            let v = self.0.clone();
            let n = self.0.add(&self.2);
            self.0 = if n >= self.1 { self.1.clone() } else { n };
            Some(v)
        } else {
            let v = self.0.clone();
            let n = self.0.add(&self.2);
            self.0 = if n <= self.1 { self.1.clone() } else { n };
            Some(v)
        }
    }
}

fn get_step_range<T>(from: T, to: T, step: T) -> Result<StepRange<T>, Box<EvalAltResult>>
where
    for<'a> &'a T: Add<&'a T, Output = T>,
    T: Variant + Clone + PartialOrd,
{
    StepRange::<T>::new(from, to, step)
}

macro_rules! reg_range {
    ($lib:expr, $x:expr, $( $y:ty ),*) => (
        $(
            $lib.set_iterator::<Range<$y>>();
            let hash = $lib.set_fn_2($x, get_range::<$y>);
            $lib.update_fn_metadata(hash, [
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);
        )*
    )
}

macro_rules! reg_stepped_range {
    ($lib:expr, $x:expr, $( $y:ty ),*) => (
        $(
            $lib.set_iterator::<StepRange<$y>>();
            let hash = $lib.set_fn_3($x, get_step_range::<$y>);
            $lib.update_fn_metadata(hash, [
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("step: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);
        )*
    )
}

def_package!(crate:BasicIteratorPackage:"Basic range iterators.", lib, {
    reg_range!(lib, "range", INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_range!(lib, "range", i8, u8, i16, u16, i32, u32, i64, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_range!(lib, "range", i128, u128);
        }
    }

    reg_stepped_range!(lib, "range", INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_stepped_range!(lib, "range", i8, u8, i16, u16, i32, u32, i64, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_stepped_range!(lib, "range", i128, u128);
        }
    }
});
