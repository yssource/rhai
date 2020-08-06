Register a Custom Type and its Methods
=====================================

{{#include ../links.md}}

Rhai works seamlessly with _any_ complex Rust type.  The type can be registered with the `Engine`, as below.

Support for custom types can be turned off via the [`no_object`] feature.

```rust
use rhai::{Engine, EvalAltResult};
use rhai::RegisterFn;                       // remember 'RegisterFn' is needed

#[derive(Clone)]
struct TestStruct {
    field: i64
}

impl TestStruct {
    fn update(&mut self) {
        self.field += 41;
    }

    fn new() -> Self {
        TestStruct { field: 1 }
    }
}

let mut engine = Engine::new();

engine
    .register_type::<TestStruct>()          // most API's can be chained up
    .register_fn("update", TestStruct::update)
    .register_fn("new_ts", TestStruct::new);

let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

println!("result: {}", result.field);       // prints 42
```

Register a Custom Type
---------------------

A custom type must implement `Clone` as this allows the [`Engine`] to pass by value.

Notice that the custom type needs to be _registered_ using `Engine::register_type`.

```rust
#[derive(Clone)]
struct TestStruct {
    field: i64
}

impl TestStruct {
    fn update(&mut self) {      // methods take &mut as first parameter
        self.field += 41;
    }

    fn new() -> Self {
        TestStruct { field: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();
```

Methods on The Custom Type
-------------------------

To use native custom types, methods and functions in Rhai scripts, simply register them
using one of the `Engine::register_XXX` API.

Below, the `update` and `new` methods are registered using `Engine::register_fn`.

```rust
engine
    .register_fn("update", TestStruct::update)  // registers 'update(&mut TestStruct)'
    .register_fn("new_ts", TestStruct::new);    // registers 'new()'
```

***Note**: Rhai follows the convention that methods of custom types take a `&mut` first parameter
so that invoking methods can update the types. All other parameters in Rhai are passed by value (i.e. clones).*

**IMPORTANT: Rhai does NOT support normal references (i.e. `&T`) as parameters.**

Use the Custom Type in Scripts
-----------------------------

The custom type is then ready for use in scripts.  Scripts can see the functions and methods registered earlier.
Get the evaluation result back out just as before, this time casting to the custom type:

```rust
let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

println!("result: {}", result.field);               // prints 42
```

Method-Call Style vs. Function-Call Style
----------------------------------------

Any function with a first argument that is a `&mut` reference can be used
as method calls because internally they are the same thing: methods on a type is
implemented as a functions taking a `&mut` first argument.
This design is similar to Rust.

```rust
fn foo(ts: &mut TestStruct) -> i64 {
    ts.field
}

engine.register_fn("foo", foo);         // register a Rust native function

let result = engine.eval::<i64>(
    "let x = new_ts(); x.foo()"         // 'foo' can be called like a method on 'x'
)?;

println!("result: {}", result);         // prints 1
```

Under [`no_object`], however, the _method_ style of function calls
(i.e. calling a function as an object-method) is no longer supported.

```rust
// Below is a syntax error under 'no_object' because 'clear' cannot be called in method style.
let result = engine.eval::<()>("let x = [1, 2, 3]; x.clear()")?;
```

`type_of()` a Custom Type
-------------------------

[`type_of()`] works fine with custom types and returns the name of the type.

If `Engine::register_type_with_name` is used to register the custom type
with a special "pretty-print" name, [`type_of()`] will return that name instead.

```rust
engine
    .register_type::<TestStruct>()
    .register_fn("new_ts", TestStruct::new);

let x = new_ts();
x.type_of() == "path::to::module::TestStruct";

engine
    .register_type_with_name::<TestStruct>("Hello")
    .register_fn("new_ts", TestStruct::new);

let x = new_ts();
x.type_of() == "Hello";
```
