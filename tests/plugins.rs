#![cfg(not(any(feature = "no_index", feature = "no_module")))]

use rhai::plugin::*;
use rhai::{Engine, EvalAltResult, INT};

#[export_module]
mod special_array_package {
    use rhai::{Array, INT};

    pub fn len(array: &mut Array, mul: INT) -> INT {
        (array.len() as INT) * mul
    }
}

macro_rules! gen_unary_functions {
    ($op_name:ident = $op_fn:ident ( $($arg_type:ident),+ ) -> $return_type:ident) => {
        mod $op_name { $(
            pub mod $arg_type {
                use super::super::*;

                #[export_fn]
                pub fn single(x: $arg_type) -> $return_type {
                    super::super::$op_fn(x)
                }
            }
        )* }
    }
}

macro_rules! reg_functions {
    ($mod_name:ident += $op_name:ident :: $func:ident ( $($arg_type:ident),+ )) => {
        $(register_exported_fn!($mod_name, stringify!($op_name), $op_name::$arg_type::$func);)*
    }
}

fn make_greeting<T: std::fmt::Display>(n: T) -> String {
    format!("{} kitties", n)
}

gen_unary_functions!(greet = make_greeting(INT, bool, char) -> String);

#[test]
fn test_plugins_package() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    let m = exported_module!(special_array_package);
    engine.load_package(m);

    reg_functions!(engine += greet::single(INT, bool, char));

    assert_eq!(engine.eval::<INT>("let a = [1, 2, 3]; len(a, 2)")?, 6);
    assert_eq!(
        engine.eval::<String>("let a = [1, 2, 3]; greet(len(a, 2))")?,
        "6 kitties"
    );

    Ok(())
}
