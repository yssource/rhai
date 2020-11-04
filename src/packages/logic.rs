use crate::def_package;
use crate::plugin::*;

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
                #[rhai_fn(name = "<")]
                pub fn lt(x: $arg_type, y: $arg_type) -> bool {
                    x < y
                }
                #[rhai_fn(name = "<=")]
                pub fn lte(x: $arg_type, y: $arg_type) -> bool {
                    x <= y
                }
                #[rhai_fn(name = ">")]
                pub fn gt(x: $arg_type, y: $arg_type) -> bool {
                    x > y
                }
                #[rhai_fn(name = ">=")]
                pub fn gte(x: $arg_type, y: $arg_type) -> bool {
                    x >= y
                }
                #[rhai_fn(name = "==")]
                pub fn eq(x: $arg_type, y: $arg_type) -> bool {
                    x == y
                }
                #[rhai_fn(name = "!=")]
                pub fn ne(x: $arg_type, y: $arg_type) -> bool {
                    x != y
                }
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
    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += numbers; i8, u8, i16, u16, i32, u32, u64);

        #[cfg(not(target_arch = "wasm32"))]
        reg_functions!(lib += num_128; i128, u128);
    }

    #[cfg(not(feature = "no_float"))]
    {
        #[cfg(not(feature = "f32_float"))]
        reg_functions!(lib += float; f32);

        #[cfg(feature = "f32_float")]
        reg_functions!(lib += float; f64);
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
#[cfg(not(target_arch = "wasm32"))]
gen_cmp_functions!(num_128 => i128, u128);

#[cfg(not(feature = "no_float"))]
#[cfg(not(feature = "f32_float"))]
gen_cmp_functions!(float => f32);

#[cfg(not(feature = "no_float"))]
#[cfg(feature = "f32_float")]
gen_cmp_functions!(float => f64);
