Export a Rust Module to Rhai
============================

{{#include ../links.md}}


When applied to a Rust module, the `#[export_module]` attribute generates the necessary
code and metadata to allow Rhai access to its public (i.e. marked `pub`) functions, constants
and sub-modules.

This code is exactly what would need to be written by hand to achieve the same goal,
and is custom fit to each exported item.

This Rust module can then either be loaded into an [`Engine`] as a normal [module] or
registered as a [custom package]. This is done by using the `exported_module!` macro.


`#[export_module]` and `exported_module!`
----------------------------------------

Apply `#[export_module]` onto a Rust module to register automatically construct a Rhai [module],
which can then be loaded into an [`Engine`].

All `pub` functions become registered functions, all `pub` constants become [module] constant variables,
and all sub-modules become Rhai sub-modules.

```rust
use rhai::plugins::*;       // a "prelude" import for macros

#[export_module]
mod my_module {
    // This constant will be registered as the constant variable 'SOME_NUMBER'.
    // Ignored when loaded as a package.
    pub const SOME_NUMBER: i64 = 42;

    // This function will be registered as 'greet'.
    pub fn greet(name: &str) -> String {
        format!("hello, {}!", name)
    }
    // This function will be registered as 'get_num'.
    pub fn get_num() -> i64 {
        mystic_number()
    }
    // This function will be registered as 'increment'.
    pub fn increment(num: &mut i64) {
        *num += 1;
    }
    // This function is not 'pub', so NOT registered.
    fn mystic_number() -> i64 {
        42
    }

    // This sub-module is ignored when loaded as a package.
    pub mod my_sub_module {
        // This function is ignored when loaded as a package.
        // Otherwise it is a valid registered function under a sub-module.
        pub fn get_info() -> String {
            "hello".to_string()
        }
    }
}
```

The simplest way to load this into an [`Engine`] is to use the `load_package` method on the exported module:

```rust
fn main() {
    let mut engine = Engine::new();

    // The macro call creates the Rhai module.
    let module = exported_module!(my_module);

    // A module can simply be loaded, registering all public functions.
    engine.load_package(module);
}
```

The functions contained within the module definition (i.e. `greet`, `get_num` and `increment`)
are automatically registered into the [`Engine`] when `Engine::load_package` is called.

```rust
let x = greet("world");
x == "hello, world!";

let x = greet(get_num().to_string());
x == "hello, 42!";

let x = get_num();
x == 42;

increment(x);
x == 43;
```

Notice that, when using a [module] as a [package], only functions registered at the _top level_
can be accessed.  Variables as well as sub-modules are ignored.

Using this directly as a Rhai module is almost the same, except that a [module resolver] must
be used to serve the module, and the module is loaded via `import` statements.
See the [module] section for more information.


Function Overloading and Operators
---------------------------------

Operators and overloaded functions can be specified via applying the `#[rhai_fn(name = "...")]`
attribute to individual functions.

The text string given as the `name` parameter to `#[rhai_fn]` is used to register the function with
the [`Engine`], disregarding the actual name of the function.

With `#[rhai_fn(name = "...")]`, multiple functions may be registered under the same name in Rhai,
so long as they have different parameters.

Operators (which require function names that are not valid for Rust) can also be registered this way.

Registering the same function name with the same parameter types will cause a parsing error.

```rust
use rhai::plugins::*;       // a "prelude" import for macros

#[export_module]
mod my_module {
    // This is the '+' operator for 'MyType'.
    #[rhai_fn(name = "+")]
    pub fn add(obj: &mut MyType, value: i64) {
        obj.prop += value;
    }
    // This function is 'calc (i64)'.
    #[rhai_fn(name = "calc")]
    pub fn calc_with_default(num: i64) -> i64 {
        ...
    }
    // This function is 'calc (i64, bool)'.
    #[rhai_fn(name = "calc")]
    pub fn calc_with_option(num: i64, option: bool) -> i64 {
        ...
    }
}
```


Getters, Setters and Indexers
-----------------------------

Functions can be marked as [getters/setters] and [indexers] for [custom types] via the `#[rhai_fn]`
attribute, which is applied on a function level.

```rust
use rhai::plugins::*;       // a "prelude" import for macros

#[export_module]
mod my_module {
    // This is a normal function 'greet'.
    pub fn greet(name: &str) -> String {
        format!("hello, {}!", name)
    }
    // This is a getter for 'MyType::prop'.
    #[rhai_fn(get = "prop")]
    pub fn get_prop(obj: &mut MyType) -> i64 {
        obj.prop
    }
    // This is a setter for 'MyType::prop'.
    #[rhai_fn(set = "prop")]
    pub fn set_prop(obj: &mut MyType, value: i64) {
        obj.prop = value;
    }
    // This is an index getter for 'MyType'.
    #[rhai_fn(index_get)]
    pub fn get_index(obj: &mut MyType, index: i64) -> bool {
        obj.list[index]
    }
    // This is an index setter for 'MyType'.
    #[rhai_fn(index_set)]
    pub fn get_index(obj: &mut MyType, index: i64, state: bool) {
        obj.list[index] = state;
    }
}
```


Multiple Registrations
----------------------

Parameters to the `#[rhai_fn(...)]` attribute can be applied multiple times.

This is especially useful for the `name = "..."`, `get = "..."` and `set = "..."` parameters
to give multiple alternative names to the same function.

```rust
use rhai::plugins::*;       // a "prelude" import for macros

#[export_module]
mod my_module {
    // This function can be called in five ways
    #[rhai_fn(name = "get_prop_value", name = "prop", name = "+", set = "prop", index_get)]
    pub fn prop_function(obj: &mut MyType, index: i64) -> i64 {
        obj.prop[index]
    }
}
```

The above function can be called in five ways:

| Parameter for `#[rhai_fn(...)]` |      Type       | Call style                                    |
| ------------------------------- | :-------------: | --------------------------------------------- |
| `name = "get_prop_value"`       | method function | `get_prop_value(x, 0)`, `x.get_prop_value(0)` |
| `name = "prop"`                 | method function | `prop(x, 0)`, `x.prop(0)`                     |
| `name = "+"`                    |    operator     | `x + 42`                                      |
| `set = "prop"`                  |     setter      | `x.prop = 42`                                 |
| `index_get`                     |  index getter   | `x[0]`                                        |


Fallible Functions
------------------

To register [fallible functions] (i.e. functions that may return errors), apply the
`#[rhai_fn(return_raw)]` attribute on functions that return `Result<Dynamic, Box<EvalAltResult>>`.

A syntax error is generated if the function with `#[rhai_fn(return_raw)]` does not
have the appropriate return type.

```rust
use rhai::plugins::*;       // a "prelude" import for macros

#[export_module]
mod my_module {
    // This overloads the '/' operator for i64.
    #[rhai_fn(name = "/", return_raw)]
    pub fn double_and_divide(x: i64, y: i64) -> Result<Dynamic, Box<EvalAltResult>> {
        if y == 0 {
            Err("Division by zero!".into())
        } else {
            let result = (x * 2) / y;
            Ok(result.into())
        }
    }
}
```


`#[export_module]` Parameters
----------------------------

Parameters can be applied to the `#[export_module]` attribute to override its default behavior.

| Parameter               | Description                                                                                                                    |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| _none_                  | exports only public (i.e. `pub`) functions                                                                                     |
| `export_all`            | exports all functions (including private, non-`pub` functions); use `#[rhai_fn(skip)]` on individual functions to avoid export |
| `export_prefix = "..."` | exports functions (including private, non-`pub` functions) with names starting with a specific prefix                          |


Inner Attributes
----------------

Inner attributes can be applied to the inner items of a module to tweak the export process.

`#[rhai_fn]` is applied to functions, while `#[rhai_mod]` is applied to sub-modules.

Parameters should be set on inner attributes to specify the desired behavior.

| Attribute Parameter | Use with                    | Apply to                                                 | Description                                            |
| ------------------- | --------------------------- | -------------------------------------------------------- | ------------------------------------------------------ |
| `skip`              | `#[rhai_fn]`, `#[rhai_mod]` | function or sub-module                                   | do not export this function/sub-module                 |
| `name = "..."`      | `#[rhai_fn]`, `#[rhai_mod]` | function or sub-module                                   | registers function/sub-module under the specified name |
| `get = "..."`       | `#[rhai_fn]`                | function with `&mut` first parameter                     | registers a getter for the named property              |
| `set = "..."`       | `#[rhai_fn]`                | function with `&mut` first parameter                     | registers a setter for the named property              |
| `index_get`         | `#[rhai_fn]`                | function with `&mut` first parameter                     | registers an index getter                              |
| `index_set`         | `#[rhai_fn]`                | function with `&mut` first parameter                     | registers an index setter                              |
| `return_raw`        | `#[rhai_fn]`                | function returning `Result<Dynamic, Box<EvalAltResult>>` | marks this as a [fallible function]                    |
