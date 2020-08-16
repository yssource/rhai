#![cfg(not(feature = "no_std"))]

#[cfg(feature = "no_float")]
#[cfg(not(feature = "unchecked"))]
use super::math_basic::MAX_INT;

use crate::def_package;
use crate::plugin::*;
use crate::result::EvalAltResult;

#[cfg(not(feature = "no_float"))]
use crate::parser::FLOAT;

#[cfg(feature = "no_float")]
use crate::parser::INT;

#[cfg(feature = "no_float")]
#[cfg(not(feature = "unchecked"))]
use crate::token::Position;

#[cfg(not(target_arch = "wasm32"))]
use crate::stdlib::time::Instant;

#[cfg(target_arch = "wasm32")]
use instant::Instant;

def_package!(crate:BasicTimePackage:"Basic timing utilities.", lib, {
    // Register date/time functions
    lib.combine(exported_module!(time_functions));

    #[cfg(not(feature = "no_object"))]
    lib.set_getter_fn("elapsed", time_functions::elapsed);
});

#[export_module]
mod time_functions {
    pub fn timestamp() -> Instant {
        Instant::now()
    }
    #[rhai_fn(return_raw)]
    pub fn elapsed(timestamp: &mut Instant) -> Result<Dynamic, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_float"))]
        {
            Ok((timestamp.elapsed().as_secs_f64() as FLOAT).into())
        }

        #[cfg(feature = "no_float")]
        {
            let seconds = timestamp.elapsed().as_secs();

            #[cfg(not(feature = "unchecked"))]
            if seconds > (MAX_INT as u64) {
                return EvalAltResult::ErrorArithmetic(
                    format!("Integer overflow for timestamp.elapsed: {}", seconds),
                    Position::none(),
                )
                .into();
            }

            Ok((seconds as INT).into())
        }
    }

    #[rhai_fn(return_raw, name = "-")]
    fn time_diff(ts1: Instant, ts2: Instant) -> Result<Dynamic, Box<EvalAltResult>> {
        #[cfg(not(feature = "no_float"))]
        {
            Ok(if ts2 > ts1 {
                -(ts2 - ts1).as_secs_f64() as FLOAT
            } else {
                (ts1 - ts2).as_secs_f64() as FLOAT
            }
            .into())
        }

        #[cfg(feature = "no_float")]
        if ts2 > ts1 {
            let seconds = (ts2 - ts1).as_secs();

            #[cfg(not(feature = "unchecked"))]
            if seconds > (MAX_INT as u64) {
                return EvalAltResult::ErrorArithmetic(
                    format!("Integer overflow for timestamp duration: -{}", seconds),
                    Position::none(),
                )
                .into();
            }

            Ok(Dynamic::from(-(seconds as INT)))
        } else {
            let seconds = (ts1 - ts2).as_secs();

            #[cfg(not(feature = "unchecked"))]
            if seconds > (MAX_INT as u64) {
                return EvalAltResult::ErrorArithmetic(
                    format!("Integer overflow for timestamp duration: {}", seconds),
                    Position::none(),
                )
                .into();
            }

            Ok((seconds as INT).into())
        }
    }

    #[rhai_fn(name = "==")]
    pub fn eq(x: Instant, y: Instant) -> bool {
        x == y
    }
    #[rhai_fn(name = "!=")]
    pub fn ne(x: Instant, y: Instant) -> bool {
        x != y
    }
    #[rhai_fn(name = "<")]
    pub fn lt(x: Instant, y: Instant) -> bool {
        x < y
    }
    #[rhai_fn(name = "<=")]
    pub fn lte(x: Instant, y: Instant) -> bool {
        x <= y
    }
    #[rhai_fn(name = ">")]
    pub fn gt(x: Instant, y: Instant) -> bool {
        x > y
    }
    #[rhai_fn(name = ">=")]
    pub fn gte(x: Instant, y: Instant) -> bool {
        x >= y
    }
}
