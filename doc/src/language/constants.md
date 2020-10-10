Constants
=========

{{#include ../links.md}}

Constants can be defined using the `const` keyword and are immutable.

Constants follow the same naming rules as [variables].

```rust
const x = 42;

print(x * 2);       // prints 84

x = 123;            // <- syntax error: cannot assign to constant
```

```rust
const x;            // 'x' is a constant '()'

const x = 40 + 2;   // 'x' is a constant 42
```


Manually Add Constant into Custom Scope
--------------------------------------

It is possible to add a constant into a custom [`Scope`] so it'll be available to scripts
running with that [`Scope`].

When added to a custom [`Scope`], a constant can hold any value, not just a literal value.

It is very useful to have a constant value hold a [custom type], which essentially acts
as a [_singleton_](../patterns/singleton.md).

```rust
use rhai::{Engine, Scope};

struct TestStruct(i64);                                     // custom type

let engine = Engine::new()
    .register_type_with_name::<TestStruct>("TestStruct")    // register custom type
    .register_get_set("value",
        |obj: &mut TestStruct| obj.0,                       // property getter
        |obj: &mut TestStruct, value: i64| obj.0 = value    // property setter
    );

let mut scope = Scope::new();                               // create custom scope

scope.push_constant("MY_NUMBER", TestStruct(123_i64));      // add constant variable

engine.consume_with_scope(&mut scope, r"
    MY_NUMBER.value = 42;                                   // constant objects can be modified
    print(MY_NUMBER.value);                                 // prints 42
")?;
```


Constants Can be Modified, Just Not Reassigned
---------------------------------------------

A custom type stored as a constant can be modified via its registered API -
being a constant only prevents it from being re-assigned or operated upon by Rhai;
mutating it via a Rust function is still allowed.
