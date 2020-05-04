use rhai::{EvalAltResult, Scope, SubScope, INT};

#[test]
#[cfg(not(feature = "no_import"))]
fn test_sub_scope() {
    let mut my_scope = Scope::new();

    let mut sub_scope = SubScope::new();
    sub_scope.insert("x".to_string(), (42 as INT).into());

    my_scope.push_sub_scope("my_plugin", sub_scope);

    let s = my_scope.find_sub_scope("my_plugin").unwrap();
    assert_eq!(*s.get("x").unwrap().downcast_ref::<INT>().unwrap(), 42);
}
