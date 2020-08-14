#![cfg(not(feature = "no_std"))]

#[cfg(feature = "no_float")]
#[cfg(not(feature = "unchecked"))]
use super::math_basic::MAX_INT;

use crate::def_package;
use crate::engine::make_getter;
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
    set_exported_fn!(lib, "timestamp", create_timestamp);
    set_exported_fn!(lib, "elapsed", elapsed);

    #[cfg(not(feature = "no_object"))]
    set_exported_fn!(lib, make_getter("elapsed"), elapsed);

    set_exported_fn!(lib, "-", time_diff);

    //lib.combine(exported_module!(time_compare));

    lib.set_fn_2("<", |x:Instant, y:Instant| Ok(x < y));
    lib.set_fn_2("<=", |x:Instant, y:Instant| Ok(x <= y));
    lib.set_fn_2(">", |x:Instant, y:Instant| Ok(x > y));
    lib.set_fn_2(">=", |x:Instant, y:Instant| Ok(x >= y));
    lib.set_fn_2("==", |x:Instant, y:Instant| Ok(x == y));
    lib.set_fn_2("!=", |x:Instant, y:Instant| Ok(x != y));
});

#[export_fn]
fn create_timestamp() -> Instant {
    Instant::now()
}

#[cfg(not(feature = "no_float"))]
#[export_fn]
fn elapsed(timestamp: &mut Instant) -> FLOAT {
    timestamp.elapsed().as_secs_f64() as FLOAT
}

#[cfg(feature = "no_float")]
#[export_fn(return_raw)]
fn elapsed(timestamp: &mut Instant) -> Result<Dynamic, Box<EvalAltResult>> {
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

#[cfg(not(feature = "no_float"))]
#[export_fn]
fn time_diff(ts1: Instant, ts2: Instant) -> FLOAT {
    if ts2 > ts1 {
        -(ts2 - ts1).as_secs_f64() as FLOAT
    } else {
        (ts1 - ts2).as_secs_f64() as FLOAT
    }
}

#[cfg(feature = "no_float")]
#[export_fn(return_raw)]
fn time_diff(ts1: Instant, ts2: Instant) -> Result<Dynamic, Box<EvalAltResult>> {
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

        Ok(-(seconds as INT).into())
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

#[export_module]
mod time_compare {
    pub fn eq(x: Instant, y: Instant) -> bool {
        x == y
    }
    pub fn ne(x: Instant, y: Instant) -> bool {
        x != y
    }
    pub fn lt(x: Instant, y: Instant) -> bool {
        x < y
    }
    pub fn lte(x: Instant, y: Instant) -> bool {
        x <= y
    }
    pub fn gt(x: Instant, y: Instant) -> bool {
        x > y
    }
    pub fn gte(x: Instant, y: Instant) -> bool {
        x >= y
    }
}
