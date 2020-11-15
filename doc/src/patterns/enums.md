Working With Rust Enums
=======================

{{#include ../links.md}}

Enums in Rust are typically used with _pattern matching_.  Rhai is dynamic, so although
it integrates with Rust enum variants just fine (treated transparently as [custom types]),
it is impossible (short of registering a complete API) to distinguish between individual
enum variants or to extract internal data from them.


Simulate an Enum API
--------------------

```rust
use rhai::{Engine, RegisterFn, Dynamic, EvalAltResult};
use rhai::plugin::*;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum MyEnum {
    Foo,
    Bar(i64),
    Baz(String, bool)
}

// Create a plugin module with functions constructing the 'MyEnum' variants
#[export_module]
pub mod MyEnumModule {
    // 'MyEnum' variants
    pub const Foo: &MyEnum = MyEnum::Foo;
    pub fn Bar(value: i64) -> MyEnum { MyEnum::Bar(value) }
    pub fn Baz(val1: String, val2: bool) -> MyEnum { MyEnum::Baz(val1, val2) }
}

let mut engine = Engine::new();

// Register API for 'MyEnum'
engine
    // Register enum custom type
    .register_type_with_name::<MyEnum>("MyEnum")
    // Register access to fields
    .register_get("type", |a: &mut MyEnum| match a {
        MyEnum::Foo => "Foo".to_string(),
        MyEnum::Bar(_) => "Bar".to_string(),
        MyEnum::Baz(_, _) => "Baz".to_string()
    })
    .register_get("field_0", |a: &mut MyEnum| match a {
        MyEnum::Foo => Dynamic::UNIT,
        MyEnum::Bar(x) => Dynamic::from(x),
        MyEnum::Baz(x, _) => Dynamic::from(x)
    })
    .register_get("field_1", |a: &mut MyEnum| match a {
        MyEnum::Foo | MyEnum::Bar(_) => Dynamic::UNIT,
        MyEnum::Baz(_, x) => Dynamic::from(x)
    })
    // Register printing
    .register_fn("to_string", |a: &mut MyEnum| format!("{:?}", a))
    .register_fn("print", |a: &mut MyEnum| format!("{:?}", a))
    .register_fn("debug", |a: &mut MyEnum| format!("{:?}", a))
    .register_fn("+", |s: &str, a: MyEnum| format!("{}{:?}", s, a))
    .register_fn("+", |a: &mut MyEnum, s: &str| format!("{:?}", a).push_str(s))
    .register_fn("+=", |s: &mut ImmutableString, a: MyEnum| s += a.to_string())
    // Register '==' and '!=' operators
    .register_fn("==", |a: &mut MyEnum, b: MyEnum| a == &b)
    .register_fn("!=", |a: &mut MyEnum, b: MyEnum| a != &b)
    // Register array functions
    .register_fn("push", |list: &mut Array, item: MyEnum| list.push(Dynamic::from(item)))
    .register_fn("+=", |list: &mut Array, item: MyEnum| list.push(Dynamic::from(item)))
    .register_fn("insert", |list: &mut Array, position: i64, item: MyEnum| {
        if position <= 0 {
            list.insert(0, Dynamic::from(item));
        } else if (position as usize) >= list.len() - 1 {
            list.push(item);
        } else {
            list.insert(position as usize, Dynamic::from(item));
        }
    }).register_fn("pad", |list: &mut Array, len: i64, item: MyEnum| {
        if len as usize > list.len() { list.resize(len as usize, item); }
    })
    // Load the module as the module namespace "MyEnum"
    .register_module("MyEnum", exported_module!(MyEnumModule));
```

Instead of registering all these manually, it is often convenient to wrap them up into
a [custom package] that can be loaded into any [`Engine`].

With this API in place, working with enums will be almost the same as in Rust:

```rust
let x = MyEnum::Foo;

let y = MyEnum::Bar(42);

let z = MyEnum::Baz("hello", true);

x == MyEnum::Foo;

y != MyEnum::Bar(0);

// Detect enum types

x.type == "Foo";

y.type == "Bar";

z.type == "Baz";

// Extract enum fields

y.field_0 == 42;

y.field_1 == ();

z.field_0 == "hello";

z.field_1 == true;
```


Use `switch` Through Arrays
---------------------------

Since enums are internally treated as [custom types], they are not _literals_ and cannot be
used as a match case in `switch` expressions.  This is quite a limitation because the equivalent
`match` statement is commonly used in Rust to work with enums.

One way to work with Rust enums in a `switch` expression is through exposing the internal data
of each enum variant as an [array], usually with the name of the variant as the first item:

```rust
use rhai::Array;

engine.register_get("enum_data", |x: &mut Enum} {
    match x {
        Enum::Foo => vec!["Foo".into()] as Array,
        Enum::Bar(value) => vec!["Bar".into(), (*value).into()] as Array,
        Enum::Baz(val1, val2) => vec![
            "Baz".into(), val1.clone().into(), (*val2).into()
        ] as Array
    }
});
```

Then it is a simple matter to match an enum via the `switch` expression:

```c
// Assume 'value' = 'MyEnum::Baz("hello", true)'
// 'enum_data' creates a variable-length array with 'MyEnum' data
let x = switch value.enum_data {
    ["Foo"] => 1,
    ["Bar", 42] => 2,
    ["Bar", 123] => 3,
    ["Baz", "hello", false] => 4,
    ["Baz", "hello", true] => 5,
    _ => 9
};

x == 5;

// Which is essentially the same as:
let x = switch [value.type, value.field_0, value.field_1] {
    ["Foo", (), ()] => 1,
    ["Bar", 42, ()] => 2,
    ["Bar", 123, ()] => 3,
    ["Baz", "hello", false] => 4,
    ["Baz", "hello", true] => 5,
    _ => 9
}
```
