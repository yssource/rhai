Serialization and Deserialization of `Dynamic` with `serde`
=========================================================

{{#include ../links.md}}

Rhai's [`Dynamic`] type supports serialization and deserialization by [`serde`](https://crates.io/crates/serde)
via the [`serde`][features] feature.

A [`Dynamic`] can be seamlessly converted to and from a type that implements `serde::Serialize` and/or
`serde::Deserialize`.


Serialization
-------------

Serialization by [`serde`](https://crates.io/crates/serde) is not yet implemented.

It is simple to serialize a Rust type to `JSON` via `serde`, then use [`Engine::parse_json`]({{rootUrl}}/language/json.md) to convert
it into an [object map].


Deserialization
---------------

The function `rhai::de::from_dynamic` automatically converts a [`Dynamic`] value into any Rust type
that implements `serde::Deserialize`.

In particular, [object maps] are converted into Rust `struct`'s (or any type that is marked as
a `serde` map) while [arrays] are converted into Rust `Vec`'s (or any type that is marked
as a `serde` sequence).

```rust
use rhai::{Engine, Dynamic};
use rhai::de::from_dynamic;

#[derive(Debug, serde::Deserialize)]
struct Point {
    x: f64,
    y: f64
}

#[derive(Debug, serde::Deserialize)]
struct MyStruct {
    a: i64,
    b: Vec<String>,
    c: bool,
    d: Point
}

let engine = Engine::new();

let result: Dynamic = engine.eval(r#"
            #{
                a: 42,
                b: [ "hello", "world" ],
                c: true,
                d: #{ x: 123.456, y: 999.0 }
            }
        "#)?;

// Convert the 'Dynamic' object map into 'MyStruct'
let x: MyStruct = from_dynamic(&result)?;
```
