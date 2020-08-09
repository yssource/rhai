#![cfg(not(any(feature = "no_index", feature = "no_module")))]

use rhai::plugin::*;
use rhai::{Engine, EvalAltResult, INT, Module};

macro_rules! generate_adds {
    ($($type_names:ident),+) => {
        $(
        pub mod $type_names {
            use rhai::plugin::*;
            #[export_fn]
            pub fn add(x: $type_names, y: $type_names) -> $type_names {
                x + y
            }
        }
        )*
    }
}

macro_rules! register_adds_in_bulk {
    ($mod_name:ident, $($type_names:ident),+) => {
        $(
            {
                let type_str = stringify!($type_names);
                register_exported_fn!($mod_name,
                                      format!("add_{}", type_str),
                                      crate::$type_names::add);
            }
        )*
    }
}

generate_adds!(i8, i16, i32, i64);

#[test]
fn test_generated_adds() -> Result<(), Box<EvalAltResult>> {
    let mut engine = Engine::new();

    let mut m = Module::new();
    register_adds_in_bulk!(m, i8, i16, i32, i64);

    engine.load_package(m);

    #[cfg(feature = "only_i32")]
    assert_eq!(engine.eval::<INT>("let a = 0; add_i32(a, 1)")?, 1);
    #[cfg(not(feature = "only_i32"))]
    assert_eq!(engine.eval::<INT>("let a = 0; add_i64(a, 1)")?, 1);

    Ok(())
}
