use crate::def_package;
use crate::plugin::*;

macro_rules! gen_cmp_functions {
    ($op_name:tt = $op_fn:ident ( $($arg_type:ident),+ ) -> $return_type:ident) => {
        pub mod $op_fn { $(
            pub mod $arg_type {
                use crate::plugin::*;

                pub const OP_NAME: &'static str = $op_name;

                #[export_fn]
                pub fn cmp_func(x: $arg_type, y: $arg_type) -> $return_type {
                    super::super::super::$op_fn(x, y)
                }
            }
        )* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $root:ident :: $op_name:ident ( $($arg_type:ident),+ )) => {
        $(set_exported_fn!($mod_name, $root::$op_name::$arg_type::OP_NAME, $root::$op_name::$arg_type::cmp_func);)*
    }
}

def_package!(crate:LogicPackage:"Logical operators.", lib, {
    #[cfg(not(feature = "only_i32"))]
    #[cfg(not(feature = "only_i64"))]
    {
        reg_functions!(lib += cmp::lt(i8, u8, i16, u16, i32, u32, u64));
        reg_functions!(lib += cmp::lte(i8, u8, i16, u16, i32, u32, u64));
        reg_functions!(lib += cmp::gt(i8, u8, i16, u16, i32, u32, u64));
        reg_functions!(lib += cmp::gte(i8, u8, i16, u16, i32, u32, u64));
        reg_functions!(lib += cmp::eq(i8, u8, i16, u16, i32, u32, u64));
        reg_functions!(lib += cmp::ne(i8, u8, i16, u16, i32, u32, u64));

        #[cfg(not(target_arch = "wasm32"))]
        {
            reg_functions!(lib += cmp_128::lt(i128, u128));
            reg_functions!(lib += cmp_128::lte(i128, u128));
            reg_functions!(lib += cmp_128::gt(i128, u128));
            reg_functions!(lib += cmp_128::gte(i128, u128));
            reg_functions!(lib += cmp_128::eq(i128, u128));
            reg_functions!(lib += cmp_128::ne(i128, u128));
        }
    }

    #[cfg(not(feature = "no_float"))]
    {
        reg_functions!(lib += cmp_float::lt(f32));
        reg_functions!(lib += cmp_float::lte(f32));
        reg_functions!(lib += cmp_float::gt(f32));
        reg_functions!(lib += cmp_float::gte(f32));
        reg_functions!(lib += cmp_float::eq(f32));
        reg_functions!(lib += cmp_float::ne(f32));
    }

    set_exported_fn!(lib, "!", not);
});

// Comparison operators
#[inline(always)]
pub fn lt<T: PartialOrd>(x: T, y: T) -> bool {
    x < y
}
#[inline(always)]
pub fn lte<T: PartialOrd>(x: T, y: T) -> bool {
    x <= y
}
#[inline(always)]
pub fn gt<T: PartialOrd>(x: T, y: T) -> bool {
    x > y
}
#[inline(always)]
pub fn gte<T: PartialOrd>(x: T, y: T) -> bool {
    x >= y
}
#[inline(always)]
pub fn eq<T: PartialEq>(x: T, y: T) -> bool {
    x == y
}
#[inline(always)]
pub fn ne<T: PartialEq>(x: T, y: T) -> bool {
    x != y
}

// Logic operators
#[export_fn]
#[inline(always)]
fn not(x: bool) -> bool {
    !x
}

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
mod cmp {
    gen_cmp_functions!("<" = lt(i8, u8, i16, u16, i32, u32, u64) -> bool);
    gen_cmp_functions!("<=" = lte(i8, u8, i16, u16, i32, u32, u64) -> bool);
    gen_cmp_functions!(">" = gt(i8, u8, i16, u16, i32, u32, u64) -> bool);
    gen_cmp_functions!(">=" = gte(i8, u8, i16, u16, i32, u32, u64) -> bool);
    gen_cmp_functions!("==" = eq(i8, u8, i16, u16, i32, u32, u64) -> bool);
    gen_cmp_functions!("!=" = ne(i8, u8, i16, u16, i32, u32, u64) -> bool);
}

#[cfg(not(feature = "only_i32"))]
#[cfg(not(feature = "only_i64"))]
#[cfg(not(target_arch = "wasm32"))]
mod cmp_128 {
    gen_cmp_functions!("<" = lt(i128, u128) -> bool);
    gen_cmp_functions!("<=" = lte(i128, u128) -> bool);
    gen_cmp_functions!(">" = gt(i128, u128) -> bool);
    gen_cmp_functions!(">=" = gte(i128, u128) -> bool);
    gen_cmp_functions!("==" = eq(i128, u128) -> bool);
    gen_cmp_functions!("!=" = ne(i128, u128) -> bool);
}

#[cfg(not(feature = "no_float"))]
mod cmp_float {
    gen_cmp_functions!("<" = lt(f32) -> bool);
    gen_cmp_functions!("<=" = lte(f32) -> bool);
    gen_cmp_functions!(">" = gt(f32) -> bool);
    gen_cmp_functions!(">=" = gte(f32) -> bool);
    gen_cmp_functions!("==" = eq(f32) -> bool);
    gen_cmp_functions!("!=" = ne(f32) -> bool);
}
