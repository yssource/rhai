use rhai::{plugin::*, Engine, Scope};

#[export_module]
pub mod general_kenobi {
    /// Returns a string where `hello there `
    /// is repeated `n` times.
    pub fn hello_there(n: i64) -> String {
        use std::convert::TryInto;
        "hello there ".repeat(n.try_into().unwrap())
    }
}

fn main() {
    let mut engine = Engine::new();
    let mut scope = Scope::new();

    // This variable will also show up in the definitions,
    // since it will be part of the scope.
    scope.push("hello_there", "hello there");

    #[cfg(not(feature = "no_module"))]
    engine.register_static_module("general_kenobi", exported_module!(general_kenobi).into());

    // Custom operators also show up in definitions.
    #[cfg(not(feature = "no_custom_syntax"))]
    {
        engine.register_custom_operator("minus", 100).unwrap();
        engine.register_fn("minus", |a: i64, b: i64| a - b);
    }

    engine
        .eval_with_scope::<()>(
            &mut scope,
            r#"
hello_there = general_kenobi::hello_there(4 minus 2);
"#,
        )
        .unwrap();

    engine
        .definitions_with_scope(&scope)
        .write_to_dir("examples/definitions/.rhai/definitions")
        .unwrap();
}
