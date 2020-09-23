Custom Type Getters and Setters
==============================

{{#include ../links.md}}

A custom type can also expose members by registering `get` and/or `set` functions.

Getters and setters each take a `&mut` reference to the first parameter.

Getters and setters are disabled when the [`no_object`] feature is used.

| `Engine` API          | Description                                       |       Return Value of Function        |
| --------------------- | ------------------------------------------------- | :-----------------------------------: |
| `register_get`        | Register a getter                                 |                 _Any_                 |
| `register_set`        | Register a setter                                 |                _None_                 |
| `register_get_set`    | Short-hand to register both a getter and a setter |                _None_                 |
| `register_get_result` | Register a getter                                 | `Result<Dynamic, Box<EvalAltResult>>` |
| `register_set_result` | Register a setter                                 |   `Result<(), Box<EvalAltResult>>`    |


Cannot Override Object Maps
--------------------------

Getters and setters are only intended for [custom types].

Any getter or setter function registered for [object maps] is simply ignored because
the get/set calls will be interpreted as properties on the [object maps].


Examples
--------

```rust
#[derive(Clone)]
struct TestStruct {
    field: String
}

impl TestStruct {
    // Remember &mut must be used even for getters
    fn get_field(&mut self) -> String {
        self.field.clone()
    }

    fn set_field(&mut self, new_val: &str) {
        self.field = new_val.to_string();
    }

    fn new() -> Self {
        TestStruct { field: "hello" }
    }
}

let mut engine = Engine::new();

    engine
        .register_type::<TestStruct>()
        .register_get_set("xyz", TestStruct::get_field, TestStruct::set_field)
        .register_fn("new_ts", TestStruct::new);

let result = engine.eval::<String>(r#"let a = new_ts(); a.xyz = "42"; a.xyz"#)?;

println!("Answer: {}", result);                     // prints 42
```

**IMPORTANT: Rhai does NOT support normal references (i.e. `&T`) as parameters.**
