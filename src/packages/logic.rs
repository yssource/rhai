#![allow(non_snake_case)]

use crate::def_package;
use crate::fn_call::run_builtin_binary_op;
use crate::plugin::*;

#[cfg(feature = "decimal")]
use rust_decimal::Decimal;

#[cfg(any(
    not(feature = "no_float"),
    all(not(feature = "only_i32"), not(feature = "only_i64"))
))]
macro_rules! gen_cmp_functions {
    ($root:ident => $($arg_type:ident),+) => {
        mod $root { $(pub mod $arg_type {
            use super::super::*;

            #[export_module]
            pub mod functions {
                #[rhai_fn(name = "<")] pub fn lt(x: $arg_type, y: $arg_type) -> bool { x < y }
                #[rhai_fn(name = "<=")] pub fn lte(x: $arg_type, y: $arg_type) -> bool { x <= y }
                #[rhai_fn(name = ">")] pub fn gt(x: $arg_type, y: $arg_type) -> bool { x > y }
                #[rhai_fn(name = ">=")] pub fn gte(x: $arg_type, y: $arg_type) -> bool { x >= y }
                #[rhai_fn(name = "==")] pub fn eq(x: $arg_type, y: $arg_type) -> bool { x == y }
                #[rhai_fn(name = "!=")] pub fn ne(x: $arg_type, y: $arg_type) -> bool { x != y }
            }
        })* }
    };
}

#[cfg(any(
    not(feature = "no_float"),
    all(not(feature = "only_i32"), not(feature = "only_i64"))
))]
macro_rules! reg_functions {
    ($mod_name:ident += $root:ident ; $($arg_type:ident),+) => { $(
        combine_with_exported_module!($mod_name, "logic", $root::$arg_type::functions);
    )* }
}

def_package!(crate:LogicPackage:"Logical operators.", lib, {
    combine_with_exported_module!(lib, "logic", logic_functions);

    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers; i8, u8, i16, u16, i32, u32, u64);

        #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
        reg_functions!(lib += num_128; i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    {
        #[cfg(not(feature = "f32_float"))]
        reg_functions!(lib += float; f32);
        combine_with_exported_module!(lib, "f32", f32_functions);

        #[cfg(feature = "f32_float")]
        reg_functions!(lib += float; f64);
        combine_with_exported_module!(lib, "f64", f64_functions);
    }

    #[cfg(feature = "decimal")]
    {
        reg_functions!(lib += decimal; Decimal);
        combine_with_exported_module!(lib, "decimal", decimal_functions);
    }

    set_exported_fn!(lib, "!", not);
});

// Logic operators
#[export_fn]
fn not(x: bool) -> bool {
    !x
}

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
gen_cmp_functions!(numbers => i8, u8, i16, u16, i32, u32, u64);

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
gen_cmp_functions!(num_128 => i128, u128);

#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "f32_float"))]
gen_cmp_functions!(float => f32);

#[cfg(not(feature = "no_float"))]
#[cfg(feature = "f32_float")]
gen_cmp_functions!(float => f64);

#[cfg(feature = "decimal")]
gen_cmp_functions!(decimal => Decimal);

#[export_module]
mod logic_functions {
    fn is_numeric(type_id: TypeId) -> bool {
        let result = type_id == TypeId::of::<u8>()
            || type_id == TypeId::of::<u16>()
            || type_id == TypeId::of::<u32>()
            || type_id == TypeId::of::<u64>()
            || type_id == TypeId::of::<i8>()
            || type_id == TypeId::of::<i16>()
            || type_id == TypeId::of::<i32>()
            || type_id == TypeId::of::<i64>();

        #[cfg(not(any(target_arch = "wasm32", target_arch = "wasm64")))]
        let result = result || type_id == TypeId::of::<u128>() || type_id == TypeId::of::<i128>();

        #[cfg(not(feature = "no_float"))]
        let result = result || type_id == TypeId::of::<f32>() || type_id == TypeId::of::<f64>();

        #[cfg(feature = "decimal")]
        let result = result || type_id == TypeId::of::<rust_decimal::Decimal>();

        result
    }

    #[rhai_fn(
        name = "==",
        name = "!=",
        name = ">",
        name = ">=",
        name = "<",
        name = "<=",
        return_raw
    )]
    pub fn cmp(
        ctx: NativeCallContext,
        x: Dynamic,
        y: Dynamic,
    ) -> Result<Dynamic, Box<EvalAltResult>> {
        let type_x = x.type_id();
        let type_y = y.type_id();

        if type_x != type_y && is_numeric(type_x) && is_numeric(type_y) {
            // Disallow comparisons between different number types
        } else if let Some(x) = run_builtin_binary_op(ctx.fn_name(), &x, &y)? {
            return Ok(x);
        }

        Err(Box::new(EvalAltResult::ErrorFunctionNotFound(
            format!(
                "{} ({}, {})",
                ctx.fn_name(),
                ctx.engine().map_type_name(x.type_name()),
                ctx.engine().map_type_name(y.type_name())
            ),
            Position::NONE,
        )))
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod f32_functions {
    use crate::INT;

    #[rhai_fn(name = "==")]
    pub fn eq_if(x: INT, y: f32) -> bool {
        (x as f32) == (y as f32)
    }
    #[rhai_fn(name = "==")]
    pub fn eq_fi(x: f32, y: INT) -> bool {
        (x as f32) == (y as f32)
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_if(x: INT, y: f32) -> bool {
        (x as f32) != (y as f32)
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_fi(x: f32, y: INT) -> bool {
        (x as f32) != (y as f32)
    }
    #[rhai_fn(name = ">")]
    pub fn gt_if(x: INT, y: f32) -> bool {
        (x as f32) > (y as f32)
    }
    #[rhai_fn(name = ">")]
    pub fn gt_fi(x: f32, y: INT) -> bool {
        (x as f32) > (y as f32)
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_if(x: INT, y: f32) -> bool {
        (x as f32) >= (y as f32)
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_fi(x: f32, y: INT) -> bool {
        (x as f32) >= (y as f32)
    }
    #[rhai_fn(name = "<")]
    pub fn lt_if(x: INT, y: f32) -> bool {
        (x as f32) < (y as f32)
    }
    #[rhai_fn(name = "<")]
    pub fn lt_fi(x: f32, y: INT) -> bool {
        (x as f32) < (y as f32)
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_if(x: INT, y: f32) -> bool {
        (x as f32) <= (y as f32)
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_fi(x: f32, y: INT) -> bool {
        (x as f32) <= (y as f32)
    }
}

#[cfg(not(feature = "no_float"))]
#[export_module]
mod f64_functions {
    use crate::INT;

    #[rhai_fn(name = "==")]
    pub fn eq_if(x: INT, y: f64) -> bool {
        (x as f64) == (y as f64)
    }
    #[rhai_fn(name = "==")]
    pub fn eq_fi(x: f64, y: INT) -> bool {
        (x as f64) == (y as f64)
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_if(x: INT, y: f64) -> bool {
        (x as f64) != (y as f64)
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_fi(x: f64, y: INT) -> bool {
        (x as f64) != (y as f64)
    }
    #[rhai_fn(name = ">")]
    pub fn gt_if(x: INT, y: f64) -> bool {
        (x as f64) > (y as f64)
    }
    #[rhai_fn(name = ">")]
    pub fn gt_fi(x: f64, y: INT) -> bool {
        (x as f64) > (y as f64)
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_if(x: INT, y: f64) -> bool {
        (x as f64) >= (y as f64)
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_fi(x: f64, y: INT) -> bool {
        (x as f64) >= (y as f64)
    }
    #[rhai_fn(name = "<")]
    pub fn lt_if(x: INT, y: f64) -> bool {
        (x as f64) < (y as f64)
    }
    #[rhai_fn(name = "<")]
    pub fn lt_fi(x: f64, y: INT) -> bool {
        (x as f64) < (y as f64)
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_if(x: INT, y: f64) -> bool {
        (x as f64) <= (y as f64)
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_fi(x: f64, y: INT) -> bool {
        (x as f64) <= (y as f64)
    }
}

#[cfg(feature = "decimal")]
#[export_module]
mod decimal_functions {
    use crate::INT;
    use rust_decimal::Decimal;

    #[rhai_fn(name = "==")]
    pub fn eq_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) == y
    }
    #[rhai_fn(name = "==")]
    pub fn eq_fi(x: Decimal, y: INT) -> bool {
        x == Decimal::from(y)
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) != y
    }
    #[rhai_fn(name = "!=")]
    pub fn neq_fi(x: Decimal, y: INT) -> bool {
        x != Decimal::from(y)
    }
    #[rhai_fn(name = ">")]
    pub fn gt_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) > y
    }
    #[rhai_fn(name = ">")]
    pub fn gt_fi(x: Decimal, y: INT) -> bool {
        x > Decimal::from(y)
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) >= y
    }
    #[rhai_fn(name = ">=")]
    pub fn gte_fi(x: Decimal, y: INT) -> bool {
        x >= Decimal::from(y)
    }
    #[rhai_fn(name = "<")]
    pub fn lt_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) < y
    }
    #[rhai_fn(name = "<")]
    pub fn lt_fi(x: Decimal, y: INT) -> bool {
        x < Decimal::from(y)
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_if(x: INT, y: Decimal) -> bool {
        Decimal::from(x) <= y
    }
    #[rhai_fn(name = "<=")]
    pub fn lte_fi(x: Decimal, y: INT) -> bool {
        x <= Decimal::from(y)
    }
}
