Working With Rust Enums
=======================

{{#include ../links.md}}

Enums in Rust are typically used with _pattern matching_.  Rhai is dynamic, so although
it integrates with Rust enum variants just fine (treated transparently as [custom types]),
it is impossible (short of registering a complete API) to distinguish between individual
enum variants or to extract internal data from them.


Switch Through Arrays
---------------------

An easy way to work with Rust enums is through exposing the internal data of each enum variant
as an [array], usually with the name of the variant as the first item:

```rust
use rhai::{Engine, Array};

#[derive(Debug, Clone)]
enum MyEnum {
    Foo,
    Bar(i64),
    Baz(String, bool)
}

impl MyEnum {
    fn get_enum_data(&mut self) -> Array {
        match self {
            Self::Foo => vec![
                "Foo".into()
            ] as Array,
            Self::Bar(num) => vec![
                "Bar".into(), (*num).into()
            ] as Array,
            Self::Baz(name, option) => vec![
                "Baz".into(), name.clone().into(), (*option).into()
            ] as Array
        }
    }
}

engine
    .register_type_with_name::<MyEnum>("MyEnum")
    .register_get("enum_data", MyEnum::get_enum_data);
```

Then it is a simple matter to match an enum via the `switch` expression:

```c
// Assume 'value' = 'MyEnum::Baz("hello", true)'
// 'get_data' creates a variable-length array with 'MyEnum' data
let x = switch value.enum_data {
    ["Foo"] => 1,
    ["Bar", 42] => 2,
    ["Bar", 123] => 3,
    ["Baz", "hello", false] => 4,
    ["Baz", "hello", true] => 5,
    _ => 9
};

x == 5;
```
