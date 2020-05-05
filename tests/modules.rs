#![cfg(not(feature = "no_module"))]
use rhai::{EvalAltResult, Module, Scope, INT};

#[test]
fn test_module() {
    let mut module = Module::new();
    module.set_variable("kitty", 42 as INT);

    assert!(module.contains_variable("kitty"));
    assert_eq!(module.get_variable_value::<INT>("kitty").unwrap(), 42);
}

#[test]
fn test_sub_module() {
    let mut module = Module::new();

    let mut sub_module = Module::new();

    let mut sub_module2 = Module::new();
    sub_module2.set_variable("kitty", 42 as INT);

    sub_module.set_sub_module("world", sub_module2);
    module.set_sub_module("hello", sub_module);

    assert!(module.contains_sub_module("hello"));
    let m = module.get_sub_module("hello").unwrap();

    assert!(m.contains_sub_module("world"));
    let m2 = m.get_sub_module("world").unwrap();

    assert!(m2.contains_variable("kitty"));
    assert_eq!(m2.get_variable_value::<INT>("kitty").unwrap(), 42);
}
