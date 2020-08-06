Custom Type Getters and Setters
==============================

{{#include ../links.md}}

A custom type can also expose members by registering `get` and/or `set` functions.

Getters and setters each take a `&mut` reference to the first parameter.

```rust
#[derive(Clone)]
struct TestStruct {
    field: String
}

impl TestStruct {
    // Returning a 'String' is OK - Rhai converts it into 'ImmutableString'
    fn get_field(&mut self) -> String {
        self.field.clone()
    }

    // Remember Rhai uses 'ImmutableString' or '&str' instead of 'String'
    fn set_field(&mut self, new_val: ImmutableString) {
        // Get a 'String' from an 'ImmutableString'
        self.field = (*new_val).clone();
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

// Return result can be 'String' - Rhai will automatically convert it from 'ImmutableString'
let result = engine.eval::<String>(r#"let a = new_ts(); a.xyz = "42"; a.xyz"#)?;

println!("Answer: {}", result);                     // prints 42
```

**IMPORTANT: Rhai does NOT support normal references (i.e. `&T`) as parameters.**
