use crate::dynamic::Variant;
use crate::stdlib::{boxed::Box, ops::Range};
use crate::{def_package, EvalAltResult, INT};

#[cfg(not(feature = "unchecked"))]
use crate::stdlib::string::ToString;

#[cfg(not(feature = "unchecked"))]
use num_traits::{CheckedAdd as Add, CheckedSub as Sub};

#[cfg(feature = "unchecked")]
use crate::stdlib::ops::{Add, Sub};

fn get_range<T: Variant + Clone>(from: T, to: T) -> Result<Range<T>, Box<EvalAltResult>> {
    Ok(from..to)
}

// Register range function with step
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
struct StepRange<T>(T, T, T)
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>;

impl<T> StepRange<T>
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    pub fn new(from: T, to: T, step: T) -> Result<Self, Box<EvalAltResult>> {
        #[cfg(not(feature = "unchecked"))]
        if let Some(r) = from.checked_add(&step) {
            if r == from {
                return EvalAltResult::ErrorInFunctionCall(
                    "range".to_string(),
                    "".to_string(),
                    Box::new(EvalAltResult::ErrorArithmetic(
                        "step value cannot be zero".to_string(),
                        crate::Position::NONE,
                    )),
                    crate::Position::NONE,
                )
                .into();
            }
        }

        Ok(Self(from, to, step))
    }
}

impl<T> Iterator for StepRange<T>
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.0 == self.1 {
            None
        } else if self.0 < self.1 {
            #[cfg(not(feature = "unchecked"))]
            let diff1 = if let Some(diff) = self.1.checked_sub(&self.0) {
                diff
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let diff1 = self.1 - self.0;

            let v = self.0;

            #[cfg(not(feature = "unchecked"))]
            let n = if let Some(num) = self.0.checked_add(&self.2) {
                num
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let n = self.0 + self.2;

            #[cfg(not(feature = "unchecked"))]
            let diff2 = if let Some(diff) = self.1.checked_sub(&n) {
                diff
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let diff2 = self.1 - n;

            if diff2 >= diff1 {
                None
            } else {
                self.0 = if n >= self.1 { self.1 } else { n };
                Some(v)
            }
        } else {
            #[cfg(not(feature = "unchecked"))]
            let diff1 = if let Some(diff) = self.0.checked_sub(&self.1) {
                diff
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let diff1 = self.0 - self.1;

            let v = self.0;

            #[cfg(not(feature = "unchecked"))]
            let n = if let Some(num) = self.0.checked_add(&self.2) {
                num
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let n = self.0 + self.2;

            #[cfg(not(feature = "unchecked"))]
            let diff2 = if let Some(diff) = n.checked_sub(&self.1) {
                diff
            } else {
                return None;
            };
            #[cfg(feature = "unchecked")]
            let diff2 = n - self.1;

            if diff2 >= diff1 {
                None
            } else {
                self.0 = if n <= self.1 { self.1 } else { n };
                Some(v)
            }
        }
    }
}

fn get_step_range<T>(from: T, to: T, step: T) -> Result<StepRange<T>, Box<EvalAltResult>>
where
    T: Variant + Copy + PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    StepRange::<T>::new(from, to, step)
}

macro_rules! reg_range {
    ($lib:ident | $x:expr => $( $y:ty ),*) => {
        $(
            $lib.set_iterator::<Range<$y>>();
            let hash = $lib.set_native_fn($x, get_range::<$y>);
            $lib.update_fn_metadata(hash, &[
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);
        )*
    };
    ($lib:ident | step $x:expr => $( $y:ty ),*) => {
        $(
            $lib.set_iterator::<StepRange<$y>>();
            let hash = $lib.set_native_fn($x, get_step_range::<$y>);
            $lib.update_fn_metadata(hash, &[
                    concat!("from: ", stringify!($y)),
                    concat!("to: ", stringify!($y)),
                    concat!("step: ", stringify!($y)),
                    concat!("Iterator<Item=", stringify!($y), ">")
            ]);
        )*
    };
}

def_package!(crate:BasicIteratorPackage:"Basic range iterators.", lib, {
    reg_range!(lib | "range" => INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_range!(lib | "range" => i8, u8, i16, u16, i32, u32, i64, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_range!(lib | "range" => i128, u128);
        }
    }

    reg_range!(lib | step "range" => INT);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_range!(lib | step "range" => i8, u8, i16, u16, i32, u32, i64, u64);

        if cfg!(not(target_arch = "wasm32")) {
            reg_range!(lib | step "range" => i128, u128);
        }
    }

    #[cfg(feature = "decimal")]
    {
        use rust_decimal::{
            prelude::{One, Zero},
            Decimal,
        };

        #[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
        struct StepDecimalRange(Decimal, Decimal, Decimal);

        impl StepDecimalRange {
            pub fn new(from: Decimal, to: Decimal, step: Decimal) -> Result<Self, Box<EvalAltResult>> {
                #[cfg(not(feature = "unchecked"))]
                if step.is_zero() {
                    use crate::stdlib::string::ToString;

                    return EvalAltResult::ErrorInFunctionCall("range".to_string(), "".to_string(),
                        Box::new(EvalAltResult::ErrorArithmetic("step value cannot be zero".to_string(), crate::Position::NONE)),
                        crate::Position::NONE,
                    ).into();
                }

                Ok(Self(from, to, step))
            }
        }

        impl Iterator for StepDecimalRange {
            type Item = Decimal;

            fn next(&mut self) -> Option<Decimal> {
                if self.0 == self.1 {
                    None
                } else if self.0 < self.1 {
                    #[cfg(not(feature = "unchecked"))]
                    if self.2.is_sign_negative() {
                        return None;
                    }

                    let v = self.0;
                    let n = self.0 + self.2;

                    self.0 = if n >= self.1 { self.1 } else { n };
                    Some(v)
                } else {
                    #[cfg(not(feature = "unchecked"))]
                    if self.2.is_sign_positive() {
                        return None;
                    }

                    let v = self.0;
                    let n = self.0 + self.2;

                    self.0 = if n <= self.1 { self.1 } else { n };
                    Some(v)
                }
            }
        }

        impl crate::stdlib::iter::FusedIterator for StepDecimalRange {}

        lib.set_iterator::<StepDecimalRange>();

        let hash = lib.set_native_fn("range", |from, to| StepDecimalRange::new(from, to, Decimal::one()));
        lib.update_fn_metadata(hash, &["from: Decimal", "to: Decimal", "Iterator<Item=Decimal>"]);

        let hash = lib.set_native_fn("range", |from, to, step| StepDecimalRange::new(from, to, step));
        lib.update_fn_metadata(hash, &["from: Decimal", "to: Decimal", "step: Decimal", "Iterator<Item=Decimal>"]);
    }
});
