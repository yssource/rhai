Get Function Signatures
=======================

{{#include ../links.md}}


`Engine::gen_fn_signatures`
--------------------------

As part of a _reflections_ API, `Engine::gen_fn_signatures` returns a list of function signatures
(`Vec<String>`), each corresponding to a particular function available to that [`Engine`] instance.

Functions from the following sources are included, in order:

1) Functions registered into the global namespace via the `Engine::register_XXX` API,
2) Functions in global sub-modules registered via [`Engine::register_module`]({{rootUrl}}/rust/modules/create.md),
3) Functions in registered [packages] (optional)

Included are both native Rust as well as script-defined functions (except [`private`] ones).


Function Metadata
-----------------

Beware, however, that not all function signatures contain parameters and return value information.

### `Engine::register_XXX`

For instance, functions registered via `Engine::register_XXX` contain no information on
the names of parameter and their actual types because Rust simply does not make such metadata
available natively. The return type is also undetermined.

A function registered under the name 'foo' with three parameters and unknown return type:

> `foo(_, _, _)`

An operator function - again, unknown parameters and return type.
Notice that function names do not need to be valid identifiers.

> `+(_, _)`

A [property setter][getters/setters] - again, unknown parameters and return type.
Notice that function names do not need to be valid identifiers.
In this case, the first parameter should be '&mut T' of the custom type and the return value is '()':

> `set$prop(_, _, _)`

### Script-defined functions

Script-defined [function] signatures contain parameter names. Since all parameters, as well as
the return value, are [`Dynamic`] the types are simply not shown.

A script-defined function always takes dynamic arguments, and the return type is also dynamic:

> `foo(x, y, z)`

probably defined as:

```rust
fn foo(x, y, z) {
    ...
}
```

is the same as:

> `foo(x: Dynamic, y: Dynamic, z: Dynamic) -> Result<Dynamic, Box<EvalAltResult>>`

### Plugin functions

Functions defined in [plugin modules] are the best.  They contain all the metadata
describing the functions.

A plugin function `merge`:

> `merge(list: &mut MyStruct<i64>, num: usize, name: &str) -> Option<bool>`

Notice that function names do not need to be valid identifiers.

An operator defined as a [fallible function] in a [plugin module] via `#[rhai_fn(name="+=", return_raw)]`
returns `Result<bool, Box<EvalAltResult>>`:

> `+=(list: &mut MyStruct<i64>, num: usize, name: &str) -> Result<bool, Box<EvalAltResult>>`

A [property getter][getters/setters] defined in a [plugin module]:

> `get$prop(obj: &mut MyStruct<i64>) -> String`
