Rhai - Embedded Scripting for Rust
=================================

![GitHub last commit](https://img.shields.io/github/last-commit/jonathandturner/rhai)
[![Travis (.org)](https://img.shields.io/travis/jonathandturner/rhai)](http://travis-ci.org/jonathandturner/rhai)
[![license](https://img.shields.io/github/license/jonathandturner/rhai)](https://github.com/license/jonathandturner/rhai)
[![crates.io](https://img.shields.io/crates/v/rhai.svg)](https::/crates.io/crates/rhai/)
![crates.io](https://img.shields.io/crates/d/rhai)
[![API Docs](https://docs.rs/rhai/badge.svg)](https://docs.rs/rhai/)

Rhai is an embedded scripting language and evaluation engine for Rust that gives a safe and easy way to add scripting to any application.

Rhai's current features set:

* `no-std` support
* Easy integration with Rust functions and data types, supporting getter/setter methods
* Easily call a script-defined function from Rust
* Fairly efficient (1 million iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~0.6 sec debug/~3 sec release for script runner app)
* Easy-to-use language similar to JS+Rust
* Support for overloaded functions
* Compiled script is optimized for repeat evaluations
* Very few additional dependencies (right now only [`num-traits`](https://crates.io/crates/num-traits/)
  to do checked arithmetic operations); for [`no_std`] builds, a number of additional dependencies are
  pulled in to provide for functionalities that used to be in `std`.

**Note:** Currently, the version is 0.11.0, so the language and API's may change before they stabilize.

Installation
------------

Install the Rhai crate by adding this line to `dependencies`:

```toml
[dependencies]
rhai = "0.11.0"
```

Use the latest released crate version on [`crates.io`](https::/crates.io/crates/rhai/):

```toml
[dependencies]
rhai = "*"
```

Crate versions are released on [`crates.io`](https::/crates.io/crates/rhai/) infrequently, so if you want to track the
latest features, enhancements and bug fixes, pull directly from GitHub:

```toml
[dependencies]
rhai = { git = "https://github.com/jonathandturner/rhai" }
```

Beware that in order to use pre-releases (e.g. alpha and beta), the exact version must be specified in the `Cargo.toml`.

Optional features
-----------------

| Feature       | Description                                                                                                                                              |
| ------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `no_stdlib`   | Exclude the standard library of utility functions in the build, and only include the minimum necessary functionalities. Standard types are not affected. |
| `unchecked`   | Exclude arithmetic checking (such as overflows and division by zero). Beware that a bad script may panic the entire system!                              |
| `no_function` | Disable script-defined functions if not needed.                                                                                                          |
| `no_index`    | Disable arrays and indexing features if not needed.                                                                                                      |
| `no_object`   | Disable support for custom types and objects.                                                                                                            |
| `no_float`    | Disable floating-point numbers and math if not needed.                                                                                                   |
| `no_optimize` | Disable the script optimizer.                                                                                                                            |
| `only_i32`    | Set the system integer type to `i32` and disable all other integer types. `INT` is set to `i32`.                                                         |
| `only_i64`    | Set the system integer type to `i64` and disable all other integer types. `INT` is set to `i64`.                                                         |
| `no_std`      | Build for `no-std`. Notice that additional dependencies will be pulled in to replace `std` features.                                                     |

By default, Rhai includes all the standard functionalities in a small, tight package.  Most features are here to opt-**out** of certain functionalities that are not needed.
Excluding unneeded functionalities can result in smaller, faster builds as well as less bugs due to a more restricted language.

[`unchecked`]: #optional-features
[`no_stdlib`]: #optional-features
[`no_index`]: #optional-features
[`no_float`]: #optional-features
[`no_function`]: #optional-features
[`no_object`]: #optional-features
[`no_optimize`]: #optional-features
[`only_i32`]: #optional-features
[`only_i64`]: #optional-features
[`no_std`]: #optional-features

Related
-------

Other cool projects to check out:

* [ChaiScript](http://chaiscript.com/) - A strong inspiration for Rhai.  An embedded scripting language for C++ that I helped created many moons ago, now being led by my cousin.
* Check out the list of [scripting languages for Rust](https://github.com/rust-unofficial/awesome-rust#scripting) on [awesome-rust](https://github.com/rust-unofficial/awesome-rust)

Examples
--------

A number of examples can be found in the `examples` folder:

| Example                                                            | Description                                                                 |
| ------------------------------------------------------------------ | --------------------------------------------------------------------------- |
| [`arrays_and_structs`](examples/arrays_and_structs.rs)             | demonstrates registering a new type to Rhai and the usage of arrays on it   |
| [`custom_types_and_methods`](examples/custom_types_and_methods.rs) | shows how to register a type and methods for it                             |
| [`hello`](examples/hello.rs)                                       | simple example that evaluates an expression and prints the result           |
| [`no_std`](examples/no_std.rs)                                     | example to test out `no-std` builds                                         |
| [`reuse_scope`](examples/reuse_scope.rs)                           | evaluates two pieces of code in separate runs, but using a common [`Scope`] |
| [`rhai_runner`](examples/rhai_runner.rs)                           | runs each filename passed to it as a Rhai script                            |
| [`simple_fn`](examples/simple_fn.rs)                               | shows how to register a Rust function to a Rhai [`Engine`]                  |
| [`repl`](examples/repl.rs)                                         | a simple REPL, interactively evaluate statements from stdin                 |

Examples can be run with the following command:

```bash
cargo run --example name
```

The `repl` example is a particularly good one as it allows you to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).

Example Scripts
---------------

There are also a number of examples scripts that showcase Rhai's features, all in the `scripts` folder:

| Language feature scripts                             | Description                                                   |
| ---------------------------------------------------- | ------------------------------------------------------------- |
| [`array.rhai`](scripts/array.rhai)                   | arrays in Rhai                                                |
| [`assignment.rhai`](scripts/assignment.rhai)         | variable declarations                                         |
| [`comments.rhai`](scripts/comments.rhai)             | just comments                                                 |
| [`for1.rhai`](scripts/for1.rhai)                     | for loops                                                     |
| [`function_decl1.rhai`](scripts/function_decl1.rhai) | a function without parameters                                 |
| [`function_decl2.rhai`](scripts/function_decl2.rhai) | a function with two parameters                                |
| [`function_decl3.rhai`](scripts/function_decl3.rhai) | a function with many parameters                               |
| [`if1.rhai`](scripts/if1.rhai)                       | if example                                                    |
| [`loop.rhai`](scripts/loop.rhai)                     | endless loop in Rhai, this example emulates a do..while cycle |
| [`op1.rhai`](scripts/op1.rhai)                       | just a simple addition                                        |
| [`op2.rhai`](scripts/op2.rhai)                       | simple addition and multiplication                            |
| [`op3.rhai`](scripts/op3.rhai)                       | change evaluation order with parenthesis                      |
| [`string.rhai`](scripts/string.rhai)                 | string operations                                             |
| [`while.rhai`](scripts/while.rhai)                   | while loop                                                    |

| Example scripts                              | Description                                                                        |
| -------------------------------------------- | ---------------------------------------------------------------------------------- |
| [`speed_test.rhai`](scripts/speed_test.rhai) | a simple program to measure the speed of Rhai's interpreter (1 million iterations) |
| [`primes.rhai`](scripts/primes.rhai)         | use Sieve of Eratosthenes to find all primes smaller than a limit                  |

To run the scripts, either make a tiny program or use of the `rhai_runner` example:

```bash
cargo run --example rhai_runner scripts/any_script.rhai
```

Hello world
-----------

[`Engine`]: #hello-world

To get going with Rhai, create an instance of the scripting engine and then call `eval`:

```rust
use rhai::{Engine, EvalAltResult};

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    let result = engine.eval::<i64>("40 + 2")?;

    println!("Answer: {}", result);  // prints 42

    Ok(())
}
```

The type parameter is used to specify the type of the return value, which _must_ match the actual type or an error is returned.
Rhai is very strict here. There are two ways to specify the return type - _turbofish_ notation, or type inference.

```rust
let result = engine.eval::<i64>("40 + 2")?;     // return type is i64, specified using 'turbofish' notation

let result: i64 = engine.eval("40 + 2")?;       // return type is inferred to be i64

let result = engine.eval::<String>("40 + 2")?;  // returns an error because the actual return type is i64, not String
```

Evaluate a script file directly:

```rust
let result = engine.eval_file::<i64>("hello_world.rhai".into())?;       // 'eval_file' takes a 'PathBuf'
```

To repeatedly evaluate a script, _compile_ it first into an AST (abstract syntax tree) form:

```rust
// Compile to an AST and store it for later evaluations
let ast = engine.compile("40 + 2")?;

for _ in 0..42 {
    let result: i64 = engine.eval_ast(&ast)?;

    println!("Answer #{}: {}", i, result);  // prints 42
}
```

Compiling a script file is also supported:

```rust
let ast = engine.compile_file("hello_world.rhai".into())?;
```

Rhai also allows working _backwards_ from the other direction - i.e. calling a Rhai-scripted function from Rust - via `call_fn`:

```rust
// Define a function in a script and load it into the Engine.
// Pass true to 'retain_functions' otherwise these functions will be cleared at the end of consume()
engine.consume(true,
    r"
        // a function with two parameters: String and i64
        fn hello(x, y) {
            x.len() + y
        }

        // functions can be overloaded: this one takes only one parameter
        fn hello(x) {
            x * 2
        }
    ")?;

// Evaluate the function in the AST, passing arguments into the script as a tuple
// if there are more than one. Beware, arguments must be of the correct types because
// Rhai does not have built-in type conversions. If arguments of the wrong types are passed,
// the Engine will not find the function.

let result: i64 = engine.call_fn("hello", &ast, ( String::from("abc"), 123_i64 ) )?;
//                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ put arguments in a tuple

let result: i64 = engine.call_fn("hello", 123_i64)?
//                                        ^^^^^^^ calls 'hello' with one parameter (no need for tuple)
```

Evaluate expressions only
-------------------------

Sometimes a use case does not require a full-blown scripting _language_, but only needs to evaluate _expressions_.
In these cases, use the `compile_expression` and `eval_expression` methods or their `_with_scope` variants.

```rust
let result = engine.eval_expression::<i64>("2 + (10 + 10) * 2")?;
```

When evaluation _expressions_, no control-flow statement (e.g. `if`, `while`, `for`) is not supported and will be
parse errors when encountered - not even variable assignments.

```rust
// The following are all syntax errors because the script is not an expression.
engine.eval_expression::<()>("x = 42")?;
let ast = engine.compile_expression("let x = 42")?;
let result = engine.eval_expression_with_scope::<i64>(&mut scope, "if x { 42 } else { 123 }")?;
```

Values and types
----------------

[`type_of()`]: #values-and-types
[`to_string()`]: #values-and-types

The following primitive types are supported natively:

| Category                                                                      | Equivalent Rust types                                                                                | `type_of()`           | `to_string()`         |
| ----------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- | --------------------- | --------------------- |
| **Integer number**                                                            | `u8`, `i8`, `u16`, `i16`, <br/>`u32`, `i32` (default for [`only_i32`]),<br/>`u64`, `i64` _(default)_ | `"i32"`, `"u64"` etc. | `"42"`, `"123"` etc.  |
| **Floating-point number** (disabled with [`no_float`])                        | `f32`, `f64` _(default)_                                                                             | `"f32"` or `"f64"`    | `"123.4567"` etc.     |
| **Boolean value**                                                             | `bool`                                                                                               | `"bool"`              | `"true"` or `"false"` |
| **Unicode character**                                                         | `char`                                                                                               | `"char"`              | `"A"`, `"x"` etc.     |
| **Unicode string**                                                            | `String` (_not_ `&str`)                                                                              | `"string"`            | `"hello"` etc.        |
| **Array** (disabled with [`no_index`])                                        | `rhai::Array`                                                                                        | `"array"`             | `"[ ? ? ? ]"`         |
| **Object map** (disabled with [`no_object`])                                  | `rhai::Map`                                                                                          | `"map"`               | `#{ "a": 1, "b": 2 }` |
| **Dynamic value** (i.e. can be anything)                                      | `rhai::Dynamic`                                                                                      | _the actual type_     | _actual value_        |
| **System integer** (current configuration)                                    | `rhai::INT` (`i32` or `i64`)                                                                         | `"i32"` or `"i64"`    | `"42"`, `"123"` etc.  |
| **System floating-point** (current configuration, disabled with [`no_float`]) | `rhai::FLOAT` (`f32` or `f64`)                                                                       | `"f32"` or `"f64"`    | `"123.456"` etc.      |
| **Nothing/void/nil/null** (or whatever you want to call it)                   | `()`                                                                                                 | `"()"`                | `""` _(empty string)_ |

[`()`]: #values-and-types

All types are treated strictly separate by Rhai, meaning that `i32` and `i64` and `u32` are completely different - they even cannot be added together. This is very similar to Rust.

The default integer type is `i64`. If other integer types are not needed, it is possible to exclude them and make a smaller build with the [`only_i64`] feature.

If only 32-bit integers are needed, enabling the [`only_i32`] feature will remove support for all integer types other than `i32`, including `i64`.
This is useful on some 32-bit systems where using 64-bit integers incurs a performance penalty.

If no floating-point is needed or supported, use the [`no_float`] feature to remove it.

The `to_string` function converts a standard type into a string for display purposes.

The `type_of` function detects the actual type of a value. This is useful because all variables are [`Dynamic`] in nature.

```rust
// Use 'type_of()' to get the actual types of values
type_of('c') == "char";
type_of(42) == "i64";

let x = 123;
x.type_of();                    // <- error: 'type_of' cannot use method-call style
type_of(x) == "i64";

x = 99.999;
type_of(x) == "f64";

x = "hello";
if type_of(x) == "string" {
    do_something_with_string(x);
}
```

Dynamic values
--------------

[`Dynamic`]: #dynamic-values

A `Dynamic` value can be _any_ type.

Because [`type_of()`] a `Dynamic` value returns the type of the actual value, it is usually used to perform type-specific
actions based on the actual value's type.

```rust
let mystery = get_some_dynamic_value();

if type_of(mystery) == "i64" {
    print("Hey, I got an integer here!");
} else if type_of(mystery) == "f64" {
    print("Hey, I got a float here!");
} else if type_of(mystery) == "string" {
    print("Hey, I got a string here!");
} else if type_of(mystery) == "bool" {
    print("Hey, I got a boolean here!");
} else if type_of(mystery) == "array" {
    print("Hey, I got an array here!");
} else if type_of(mystery) == "map" {
    print("Hey, I got an object map here!");
} else if type_of(mystery) == "TestStruct" {
    print("Hey, I got the TestStruct custom type here!");
} else {
    print("I don't know what this is: " + type_of(mystery));
}
```

In Rust, sometimes a `Dynamic` forms part of the return value - a good example is elements within an `Array` which are `Dynamic`,
or property values in an object map.  In order to get the _real_ value, the actual value type _must_ be known in advance.
There is no easy way for Rust to detect, at run-time, what type the `Dynamic` value is (short of using the `type_name`
function to get the textual name of the type and then matching on that).

To use a `Dynamic` value in Rust, use the `cast` method to convert the value into a specific, known type.
Alternatively, use the `try_cast` method which does not panic but returns an error when the cast fails.

```rust
use rhai::AnyExt;                               // Pull in the trait.

let list: Array = engine.eval("...")?;          // return type is 'Array'
let item = list[0];                             // an element in an 'Array' is 'Dynamic'

let value = item.cast::<i64>();                 // if the element is 'i64', this succeeds; otherwise it panics
let value: i64 = item.cast();                   // type can also be inferred

let value = item.try_cast::<i64>()?;            // 'try_cast' does not panic when the cast fails, but returns an error
```

The `type_name` method gets the name of the actual type as a string, which you may match against.

```rust
use rhai::Any;                                  // Pull in the trait.

let list: Array = engine.eval("...")?;          // return type is 'Array'
let item = list[0];                             // an element in an 'Array' is 'Dynamic'

match item.type_name() {                        // 'type_name' returns the name of the actual Rust type
    "i64" => ...
    "std::string::String" => ...
    "bool" => ...
    "path::to::module::TestStruct" => ...
}
```

Value conversions
-----------------

[`to_int`]: #value-conversions
[`to_float`]: #value-conversions

The `to_float` function converts a supported number to `FLOAT` (`f32` or `f64`),
and the `to_int` function converts a supported number to `INT` (`i32` or `i64`).
That's about it. For other conversions, register custom conversion functions.

```rust
let x = 42;
let y = x * 100.0;                              // <- error: cannot multiply i64 with f64
let y = x.to_float() * 100.0;                   // works
let z = y.to_int() + x;                         // works

let c = 'X';                                    // character
print("c is '" + c + "' and its code is " + c.to_int());    // prints "c is 'X' and its code is 88"
```

Working with functions
----------------------

Rhai's scripting engine is very lightweight.  It gets most of its abilities from functions.
To call these functions, they need to be registered with the [`Engine`].

```rust
use rhai::{Engine, EvalAltResult};
use rhai::RegisterFn;                           // use `RegisterFn` trait for `register_fn`
use rhai::{Dynamic, RegisterDynamicFn};         // use `RegisterDynamicFn` trait for `register_dynamic_fn`

// Normal function
fn add(x: i64, y: i64) -> i64 {
    x + y
}

// Function that returns a Dynamic value
fn get_an_any() -> Dynamic {
    Box::new(42_i64)
}

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    engine.register_fn("add", add);

    let result = engine.eval::<i64>("add(40, 2)")?;

    println!("Answer: {}", result);             // prints 42

    // Functions that return Dynamic values must use register_dynamic_fn()
    engine.register_dynamic_fn("get_an_any", get_an_any);

    let result = engine.eval::<i64>("get_an_any()")?;

    println!("Answer: {}", result);             // prints 42

    Ok(())
}
```

To return a [`Dynamic`] value from a Rust function, simply `Box` it and return it.

```rust
fn decide(yes_no: bool) -> Dynamic {
    if yes_no {
        Box::new(42_i64)
    } else {
        Box::new("hello world!".to_string())    // remember &str is not supported
    }
}
```

Generic functions
-----------------

Generic functions can be used in Rhai, but separate instances for each concrete type must be registered separately:

```rust
use std::fmt::Display;

use rhai::{Engine, RegisterFn};

fn show_it<T: Display>(x: &mut T) -> () {
    println!("put up a good show: {}!", x)
}

fn main()
{
    let mut engine = Engine::new();

    engine.register_fn("print", show_it as fn(x: &mut i64)->());
    engine.register_fn("print", show_it as fn(x: &mut bool)->());
    engine.register_fn("print", show_it as fn(x: &mut String)->());
}
```

This example shows how to register multiple functions (or, in this case, multiple instances of the same function) to the same name in script.
This enables function overloading based on the number and types of parameters.

Fallible functions
------------------

If a function is _fallible_ (i.e. it returns a `Result<_, Error>`), it can be registered with `register_result_fn` (using the `RegisterResultFn` trait).

The function must return `Result<_, EvalAltResult>`. `EvalAltResult` implements `From<&str>` and `From<String>` etc. and the error text gets converted into `EvalAltResult::ErrorRuntime`.

```rust
use rhai::{Engine, EvalAltResult, Position};
use rhai::RegisterResultFn;                         // use `RegisterResultFn` trait for `register_result_fn`

// Function that may fail
fn safe_divide(x: i64, y: i64) -> Result<i64, EvalAltResult> {
    if y == 0 {
        // Return an error if y is zero
        Err("Division by zero detected!".into())    // short-cut to create EvalAltResult
    } else {
        Ok(x / y)
    }
}

fn main()
{
    let mut engine = Engine::new();

    // Fallible functions that return Result values must use register_result_fn()
    engine.register_result_fn("divide", safe_divide);

    if let Err(error) = engine.eval::<i64>("divide(40, 0)") {
       println!("Error: {:?}", error);              // prints ErrorRuntime("Division by zero detected!", (1, 1)")
    }
}
```

Overriding built-in functions
----------------------------

Any similarly-named function defined in a script overrides any built-in function.

```rust
// Override the built-in function 'to_int'
fn to_int(num) {
    print("Ha! Gotcha! " + num);
}

print(to_int(123));     // what happens?
```

Custom types and methods
-----------------------

Here's an more complete example of working with Rust.  First the example, then we'll break it into parts:

```rust
use rhai::{Engine, EvalAltResult};
use rhai::RegisterFn;

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

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

    println!("result: {}", result.field);           // prints 42

    Ok(())
}
```

All custom types must implement `Clone`.  This allows the [`Engine`] to pass by value.
You can turn off support for custom types via the [`no_object`] feature.

```rust
#[derive(Clone)]
struct TestStruct {
    field: i64
}
```

Next, we create a few methods that we'll later use in our scripts.  Notice that we register our custom type with the [`Engine`].

```rust
impl TestStruct {
    fn update(&mut self) {
        self.field += 41;
    }

    fn new() -> Self {
        TestStruct { field: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();
```

To use methods and functions with the [`Engine`], we need to register them. There are some convenience functions to help with this.
Below I register update and new with the [`Engine`].

*Note: [`Engine`] follows the convention that methods use a `&mut` first parameter so that invoking methods can update the value in memory.*

```rust
engine.register_fn("update", TestStruct::update);   // registers 'update(&mut ts)'
engine.register_fn("new_ts", TestStruct::new);      // registers 'new'
```

Finally, we call our script.  The script can see the function and method we registered earlier.
We need to get the result back out from script land just as before, this time casting to our custom struct type.

```rust
let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

println!("result: {}", result.field);               // prints 42
```

In fact, any function with a first argument (either by copy or via a `&mut` reference) can be used as a method-call on that type because internally they are the same thing:
methods on a type is implemented as a functions taking an first argument.

```rust
fn foo(ts: &mut TestStruct) -> i64 {
    ts.field
}

engine.register_fn("foo", foo);

let result = engine.eval::<i64>("let x = new_ts(); x.foo()")?;

println!("result: {}", result);                     // prints 1
```

If the [`no_object`] feature is turned on, however, the _method_ style of function calls (i.e. calling a function as an object-method) is no longer supported.

```rust
// Below is a syntax error under 'no_object' because 'len' cannot be called in method style.
let result = engine.eval::<i64>("let x = [1, 2, 3]; x.len()")?;
```

[`type_of()`] works fine with custom types and returns the name of the type.
If `register_type_with_name` is used to register the custom type
with a special "pretty-print" name, [`type_of()`] will return that name instead.

```rust
engine.register_type::<TestStruct>();
engine.register_fn("new_ts", TestStruct::new);
let x = new_ts();
print(type_of(x));                                 // prints "path::to::module::TestStruct"

engine.register_type_with_name::<TestStruct>("Hello");
engine.register_fn("new_ts", TestStruct::new);
let x = new_ts();
print(type_of(x));                                 // prints "Hello"
```

Getters and setters
-------------------

Similarly, custom types can expose members by registering a `get` and/or `set` function.

```rust
#[derive(Clone)]
struct TestStruct {
    field: i64
}

impl TestStruct {
    fn get_field(&mut self) -> i64 {
        self.field
    }

    fn set_field(&mut self, new_val: i64) {
        self.field = new_val;
    }

    fn new() -> Self {
        TestStruct { field: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();

engine.register_get_set("xyz", TestStruct::get_field, TestStruct::set_field);
engine.register_fn("new_ts", TestStruct::new);

let result = engine.eval::<i64>("let a = new_ts(); a.xyz = 42; a.xyz")?;

println!("Answer: {}", result);                     // prints 42
```

Needless to say, `register_type`, `register_type_with_name`, `register_get`, `register_set` and `register_get_set`
are not available when the [`no_object`] feature is turned on.

Initializing and maintaining state
---------------------------------

[`Scope`]: #initializing-and-maintaining-state

By default, Rhai treats each [`Engine`] invocation as a fresh one, persisting only the functions that have been defined but no global state.
This gives each evaluation a clean starting slate. In order to continue using the same global state from one invocation to the next,
such a state must be manually created and passed in.

In this example, a global state object (a `Scope`) is created with a few initialized variables, then the same state is threaded through multiple invocations:

```rust
use rhai::{Engine, Scope, EvalAltResult};

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    // First create the state
    let mut scope = Scope::new();

    // Then push some initialized variables into the state
    // NOTE: Remember the system number types in Rhai are i64 (i32 if 'only_i32') ond f64.
    //       Better stick to them or it gets hard working with the script.
    scope.push("y", 42_i64);
    scope.push("z", 999_i64);
    scope.push("s", "hello, world!".to_string());   // remember to use 'String', not '&str'

    // First invocation
    engine.eval_with_scope::<()>(&mut scope, r"
        let x = 4 + 5 - y + z + s.len();
        y = 1;
    ")?;

    // Second invocation using the same state
    let result = engine.eval_with_scope::<i64>(&mut scope, "x")?;

    println!("result: {}", result);                 // prints 979

    // Variable y is changed in the script
    assert_eq!(scope.get_value::<i64>("y").expect("variable x should exist"), 1);

    Ok(())
}
```

Engine configuration options
---------------------------

| Method                   | Description                                                                              |
| ------------------------ | ---------------------------------------------------------------------------------------- |
| `set_optimization_level` | Set the amount of script _optimizations_ performed. See [`script optimization`].         |
| `set_max_call_levels`    | Set the maximum number of function call levels (default 50) to avoid infinite recursion. |

[`script optimization`]: #script-optimization

-------

Rhai Language Guide
===================

Comments
--------

Comments are C-style, including '`/*` ... `*/`' pairs and '`//`' for comments to the end of the line.

```rust
let /* intruder comment */ name = "Bob";

// This is a very important comment

/* This comment spans
   multiple lines, so it
   only makes sense that
   it is even more important */

/* Fear not, Rhai satisfies all nesting needs with nested comments:
   /*/*/*/*/**/*/*/*/*/
*/
```

Statements
----------

Statements are terminated by semicolons '`;`' - they are mandatory, except for the _last_ statement where it can be omitted.

A statement can be used anywhere where an expression is expected. The _last_ statement of a statement block
(enclosed by '`{`' .. '`}`' pairs) is always the return value of the statement. If a statement has no return value
(e.g. variable definitions, assignments) then the value will be [`()`].

```rust
let a = 42;             // normal assignment statement
let a = foo(42);        // normal function call statement
foo < 42;               // normal expression as statement

let a = { 40 + 2 };     // 'a' is set to the value of the statement block, which is the value of the last statement
//              ^ notice that the last statement does not require a terminating semicolon (although it also works with it)
//                ^ notice that a semicolon is required here to terminate the assignment statement; it is syntax error without it

4 * 10 + 2              // this is also a statement, which is an expression, with no ending semicolon because
                        // it is the last statement of the whole block
```

Variables
---------

[variables]: #variables

Variables in Rhai follow normal C naming rules (i.e. must contain only ASCII letters, digits and underscores '`_`').

Variable names must start with an ASCII letter or an underscore '`_`', must contain at least one ASCII letter, and must start with an ASCII letter before a digit.
Therefore, names like '`_`', '`_42`', '`3a`' etc. are not legal variable names, but '`_c3po`' and '`r2d2`' are.
Variable names are also case _sensitive_.

Variables are defined using the `let` keyword. A variable defined within a statement block is _local_ to that block.

```rust
let x = 3;              // ok
let _x = 42;            // ok
let x_ = 42;            // also ok
let _x_ = 42;           // still ok

let _ = 123;            // <- syntax error: illegal variable name
let _9 = 9;             // <- syntax error: illegal variable name

let x = 42;             // variable is 'x', lower case
let X = 123;            // variable is 'X', upper case
x == 42;
X == 123;

{
    let x = 999;        // local variable 'x' shadows the 'x' in parent block
    x == 999;           // access to local 'x'
}
x == 42;                // the parent block's 'x' is not changed
```

Constants
---------

Constants can be defined using the `const` keyword and are immutable.  Constants follow the same naming rules as [variables].

```rust
const x = 42;
print(x * 2);           // prints 84
x = 123;                // <- syntax error: cannot assign to constant
```

Constants must be assigned a _value_, not an expression.

```rust
const x = 40 + 2;       // <- syntax error: cannot assign expression to constant
```

Numbers
-------

Integer numbers follow C-style format with support for decimal, binary ('`0b`'), octal ('`0o`') and hex ('`0x`') notations.

The default system integer type (also aliased to `INT`) is `i64`. It can be turned into `i32` via the [`only_i32`] feature.

Floating-point numbers are also supported if not disabled with [`no_float`]. The default system floating-point type is `i64` (also aliased to `FLOAT`).

'`_`' separators can be added freely and are ignored within a number.

| Format           | Type             |
| ---------------- | ---------------- |
| `123_345`, `-42` | `i64` in decimal |
| `0o07_76`        | `i64` in octal   |
| `0xabcd_ef`      | `i64` in hex     |
| `0b0101_1001`    | `i64` in binary  |
| `123_456.789`    | `f64`            |

Numeric operators
-----------------

Numeric operators generally follow C styles.

| Operator | Description                                          | Integers only |
| -------- | ---------------------------------------------------- | :-----------: |
| `+`      | Plus                                                 |               |
| `-`      | Minus                                                |               |
| `*`      | Multiply                                             |               |
| `/`      | Divide (integer division if acting on integer types) |               |
| `%`      | Modulo (remainder)                                   |               |
| `~`      | Power                                                |               |
| `&`      | Binary _And_ bit-mask                                |      Yes      |
| `\|`     | Binary _Or_ bit-mask                                 |      Yes      |
| `^`      | Binary _Xor_ bit-mask                                |      Yes      |
| `<<`     | Left bit-shift                                       |      Yes      |
| `>>`     | Right bit-shift                                      |      Yes      |

```rust
let x = (1 + 2) * (6 - 4) / 2;  // arithmetic, with parentheses
let reminder = 42 % 10;         // modulo
let power = 42 ~ 2;             // power (i64 and f64 only)
let left_shifted = 42 << 3;     // left shift
let right_shifted = 42 >> 3;    // right shift
let bit_op = 42 | 99;           // bit masking
```

Unary operators
---------------

| Operator | Description |
| -------- | ----------- |
| `+`      | Plus        |
| `-`      | Negative    |

```rust
let number = -5;
number = -5 - +5;
```

Numeric functions
-----------------

The following standard functions (defined in the standard library but excluded if [`no_stdlib`]) operate on `i8`, `i16`, `i32`, `i64`, `f32` and `f64` only:

| Function     | Description                       |
| ------------ | --------------------------------- |
| `abs`        | absolute value                    |
| [`to_float`] | converts an integer type to `f64` |

Floating-point functions
------------------------

The following standard functions (defined in the standard library but excluded if [`no_stdlib`]) operate on `f64` only:

| Category         | Functions                                                    |
| ---------------- | ------------------------------------------------------------ |
| Trigonometry     | `sin`, `cos`, `tan`, `sinh`, `cosh`, `tanh` in degrees       |
| Arc-trigonometry | `asin`, `acos`, `atan`, `asinh`, `acosh`, `atanh` in degrees |
| Square root      | `sqrt`                                                       |
| Exponential      | `exp` (base _e_)                                             |
| Logarithmic      | `ln` (base _e_), `log10` (base 10), `log` (any base)         |
| Rounding         | `floor`, `ceiling`, `round`, `int`, `fraction`               |
| Conversion       | [`to_int`]                                                   |
| Testing          | `is_nan`, `is_finite`, `is_infinite`                         |

Strings and Chars
-----------------

String and char literals follow C-style formatting, with support for Unicode ('`\u`_xxxx_' or '`\U`_xxxxxxxx_') and hex ('`\x`_xx_') escape sequences.

Hex sequences map to ASCII characters, while '`\u`' maps to 16-bit common Unicode code points and '`\U`' maps the full, 32-bit extended Unicode code points.

Internally Rhai strings are stored as UTF-8 just like Rust (they _are_ Rust `String`s!), but there are major differences.
In Rhai a string is the same as an array of Unicode characters and can be directly indexed (unlike Rust).
This is similar to most other languages where strings are internally represented not as UTF-8 but as arrays of multi-byte Unicode characters.
Individual characters within a Rhai string can also be replaced just as if the string is an array of Unicode characters.
In Rhai, there is also no separate concepts of `String` and `&str` as in Rust.

Strings can be built up from other strings and types via the `+` operator (provided by the standard library but excluded if [`no_stdlib`]).
This is particularly useful when printing output.

[`type_of()`] a string returns `"string"`.

```rust
let name = "Bob";
let middle_initial = 'C';
let last = "Davis";

let full_name = name + " " + middle_initial + ". " + last;
full_name == "Bob C. Davis";

// String building with different types
let age = 42;
let record = full_name + ": age " + age;
record == "Bob C. Davis: age 42";

// Unlike Rust, Rhai strings can be indexed to get a character
// (disabled with 'no_index')
let c = record[4];
c == 'C';

ts.s = record;                          // custom type properties can take strings

let c = ts.s[4];
c == 'C';

let c = "foo"[0];                       // indexing also works on string literals...
c == 'f';

let c = ("foo" + "bar")[5];             // ... and expressions returning strings
c == 'r';

// Escape sequences in strings
record += " \u2764\n";                  // escape sequence of '❤' in Unicode
record == "Bob C. Davis: age 42 ❤\n";   // '\n' = new-line

// Unlike Rust, Rhai strings can be directly modified character-by-character
// (disabled with 'no_index')
record[4] = '\x58'; // 0x58 = 'X'
record == "Bob X. Davis: age 42 ❤\n";
```

The following standard functions (defined in the standard library but excluded if [`no_stdlib`]) operate on strings:

| Function   | Description                                                              |
| ---------- | ------------------------------------------------------------------------ |
| `len`      | returns the number of characters (not number of bytes) in the string     |
| `pad`      | pads the string with an character until a specified number of characters |
| `append`   | Adds a character or a string to the end of another string                |
| `clear`    | empties the string                                                       |
| `truncate` | cuts off the string at exactly a specified number of characters          |
| `contains` | checks if a certain character or sub-string occurs in the string         |
| `replace`  | replaces a substring with another                                        |
| `trim`     | trims the string                                                         |

Examples:

```rust
let full_name == " Bob C. Davis ";
full_name.len() == 14;

full_name.trim();
full_name.len() == 12;
full_name == "Bob C. Davis";

full_name.pad(15, '$');
full_name.len() == 15;
full_name == "Bob C. Davis$$$";

full_name.truncate(6);
full_name.len() == 6;
full_name == "Bob C.";

full_name.replace("Bob", "John");
full_name.len() == 7;
full_name = "John C.";

full_name.contains('C') == true;
full_name.contains("John") == true;

full_name.clear();
full_name.len() == 0;
```

Arrays
------

Arrays are first-class citizens in Rhai. Like C, arrays are accessed with zero-based, non-negative integer indices.
Array literals are built within square brackets '`[`' ... '`]`' and separated by commas '`,`'.
All elements stored in an array are [`Dynamic`], and the array can freely grow or shrink with elements added or removed.

The Rust type of a Rhai array is `rhai::Array`. [`type_of()`] an array returns `"array"`.

Arrays are disabled via the [`no_index`] feature.

The following functions (defined in the standard library but excluded if [`no_stdlib`]) operate on arrays:

| Function     | Description                                                                           |
| ------------ | ------------------------------------------------------------------------------------- |
| `push`       | inserts an element at the end                                                         |
| `append`     | concatenates the second array to the end of the first                                 |
| `+` operator | concatenates the first array with the second                                          |
| `pop`        | removes the last element and returns it ([`()`] if empty)                             |
| `shift`      | removes the first element and returns it ([`()`] if empty)                            |
| `len`        | returns the number of elements                                                        |
| `pad`        | pads the array with an element until a specified length                               |
| `clear`      | empties the array                                                                     |
| `truncate`   | cuts off the array at exactly a specified length (discarding all subsequent elements) |

Examples:

```rust
let y = [1, 2, 3];      // array literal with 3 elements
y[1] = 42;

print(y[1]);            // prints 42

ts.list = y;            // arrays can be assigned completely (by value copy)
let foo = ts.list[1];
foo == 42;

let foo = [1, 2, 3][0];
foo == 1;

fn abc() {
    [42, 43, 44]        // a function returning an array
}

let foo = abc()[0];
foo == 42;

let foo = y[0];
foo == 1;

y.push(4);              // 4 elements
y.push(5);              // 5 elements

print(y.len());         // prints 5

let first = y.shift();  // remove the first element, 4 elements remaining
first == 1;

let last = y.pop();     // remove the last element, 3 elements remaining
last == 5;

print(y.len());         // prints 3

y.pad(10, "hello");     // pad the array up to 10 elements

print(y.len());         // prints 10

y.truncate(5);          // truncate the array to 5 elements

print(y.len());         // prints 5

y.clear();              // empty the array

print(y.len());         // prints 0
```

`push` and `pad` are only defined for standard built-in types. For custom types, type-specific versions must be registered:

```rust
engine.register_fn("push", |list: &mut Array, item: MyType| list.push(Box::new(item)) );
```

Object maps
-----------

Object maps are dictionaries. Properties are all [`Dynamic`] and can be freely added and retrieved.
Object map literals are built within braces '`#{`' ... '`}`' (_name_ `:` _value_ syntax similar to Rust)
and separated by commas '`,`'.  The property _name_ can be a simple variable name following the same
naming rules as [variables], or an arbitrary string literal.

Property values can be accessed via the dot notation (_object_ `.` _property_) or index notation (_object_ `[` _property_ `]`).
The dot notation allows only property names that follow the same naming rules as [variables].
The index notation allows setting/getting properties of arbitrary names (even the empty string).

**Important:** Trying to read a non-existent property returns `()` instead of causing an error.

The Rust type of a Rhai object map is `rhai::Map`. [`type_of()`] an object map returns `"map"`.

Object maps are disabled via the [`no_object`] feature.

The following functions (defined in the standard library but excluded if [`no_stdlib`]) operate on object maps:

| Function     | Description                                                                                                                              |
| ------------ | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `has`        | does the object map contain a property of a particular name?                                                                             |
| `len`        | returns the number of properties                                                                                                         |
| `clear`      | empties the object map                                                                                                                   |
| `mixin`      | mixes in all the properties of the second object map to the first (values of properties with the same names replace the existing values) |
| `+` operator | merges the first object map with the second                                                                                              |

Examples:

```rust
let y = #{              // object map literal with 3 properties
    a: 1,
    bar: "hello",
    "baz!$@": 123.456,  // like JS, you can use any string as property names...
    "": false,          // even the empty string!

    a: 42               // <- syntax error: duplicated property name
};

y.a = 42;               // access via dot notation
y.baz!$@ = 42;          // <- syntax error: only proper variable names allowed in dot notation
y."baz!$@" = 42;        // <- syntax error: strings not allowed in dot notation

print(y.a);             // prints 42

print(y["baz!$@"]);     // prints 123.456 - access via index notation

ts.obj = y;             // object maps can be assigned completely (by value copy)
let foo = ts.list.a;
foo == 42;

let foo = #{ a:1, b:2, c:3 }["a"];
foo == 1;

fn abc() {
    #{ a:1, b:2, c:3 }  // a function returning an object map
}

let foo = abc().b;
foo == 2;

let foo = y["a"];
foo == 42;

y.has("a") == true;
y.has("xyz") == false;

y.xyz == ();            // A non-existing property returns '()'
y["xyz"] == ();

print(y.len());         // prints 3

y.clear();              // empty the object map

print(y.len());         // prints 0
```

Comparison operators
--------------------

Comparing most values of the same data type work out-of-the-box for standard types supported by the system.

However, if the [`no_stdlib`] feature is turned on, comparisons can only be made between restricted system
types - `INT` (`i64` or `i32` depending on [`only_i32`] and [`only_i64`]), `f64` (if not [`no_float`]), string, array, `bool`, `char`.

```rust
42 == 42;               // true
42 > 42;                // false
"hello" > "foo";        // true
"42" == 42;             // false
```

Comparing two values of _different_ data types, or of unknown data types, always results in `false`.

```rust
42 == 42.0;             // false - i64 is different from f64
42 > "42";              // false - i64 is different from string
42 <= "42";             // false again

let ts = new_ts();      // custom type
ts == 42;               // false - types are not the same
```

Boolean operators
-----------------

| Operator | Description                     |
| -------- | ------------------------------- |
| `!`      | Boolean _Not_                   |
| `&&`     | Boolean _And_ (short-circuits)  |
| `\|\|`   | Boolean _Or_ (short-circuits)   |
| `&`      | Boolean _And_ (full evaluation) |
| `\|`     | Boolean _Or_ (full evaluation)  |

Double boolean operators `&&` and `||` _short-circuit_, meaning that the second operand will not be evaluated
if the first one already proves the condition wrong.

Single boolean operators `&` and `|` always evaluate both operands.

```rust
this() || that();       // that() is not evaluated if this() is true
this() && that();       // that() is not evaluated if this() is false

this() | that();        // both this() and that() are evaluated
this() & that();        // both this() and that() are evaluated
```

Compound assignment operators
----------------------------

```rust
let number = 5;
number += 4;            // number = number + 4
number -= 3;            // number = number - 3
number *= 2;            // number = number * 2
number /= 1;            // number = number / 1
number %= 3;            // number = number % 3
number <<= 2;           // number = number << 2
number >>= 1;           // number = number >> 1
```

The `+=` operator can also be used to build strings:

```rust
let my_str = "abc";
my_str += "ABC";
my_str += 12345;

my_str == "abcABC12345"
```

`if` statements
---------------

```rust
if foo(x) {
    print("It's true!");
} else if bar == baz {
    print("It's true again!");
} else if ... {
        :
} else if ... {
        :
} else {
    print("It's finally false!");
}
```

All branches of an `if` statement must be enclosed within braces '`{`' .. '`}`', even when there is only one statement.
Like Rust, there is no ambiguity regarding which `if` clause a statement belongs to.

```rust
if (decision) print("I've decided!");
//            ^ syntax error, expecting '{' in statement block
```

Like Rust, `if` statements can also be used as _expressions_, replacing the `? :` conditional operators in other C-like languages.

```rust
let x = 1 + if true { 42 } else { 123 } / 2;
x == 22;

let x = if false { 42 };    // No else branch defaults to '()'
x == ();
```

`while` loops
-------------

```rust
let x = 10;

while x > 0 {
    x = x - 1;
    if x < 6 { continue; }  // skip to the next iteration
    print(x);
    if x == 5 { break; }    // break out of while loop
}
```

Infinite `loop`
---------------

```rust
let x = 10;

loop {
    x = x - 1;
    if x > 5 { continue; }  // skip to the next iteration
    print(x);
    if x == 0 { break; }    // break out of loop
}
```

`for` loops
-----------

Iterating through a range or an array is provided by the `for` ... `in` loop.

```rust
let array = [1, 3, 5, 7, 9, 42];

// Iterate through array
for x in array {
    if x > 10 { continue; } // skip to the next iteration
    print(x);
    if x == 42 { break; }   // break out of for loop
}

// The 'range' function allows iterating from first to last-1
for x in range(0, 50) {
    if x > 10 { continue; } // skip to the next iteration
    print(x);
    if x == 42 { break; }   // break out of for loop
}
```

`return`-ing values
-------------------

```rust
return;                     // equivalent to return ();

return 123 + 456;           // returns 579
```

Errors and `throw`-ing exceptions
--------------------------------

All of [`Engine`]'s evaluation/consuming methods return `Result<T, rhai::EvalAltResult>` with `EvalAltResult` holding error information.
To deliberately return an error during an evaluation, use the `throw` keyword.

```rust
if some_bad_condition_has_happened {
    throw error;            // 'throw' takes a string as the exception text
}

throw;                      // defaults to empty exception text: ""
```

Exceptions thrown via `throw` in the script can be captured by matching `Err(EvalAltResult::ErrorRuntime(` _reason_ `,` _position_ `))`
with the exception text captured by the first parameter.

```rust
let result = engine.eval::<i64>(r#"
    let x = 42;

    if x > 0 {
        throw x + " is too large!";
    }
"#);

println!(result);           // prints "Runtime error: 42 is too large! (line 5, position 15)"
```

Functions
---------

Rhai supports defining functions in script (unless disabled with [`no_function`]):

```rust
fn add(x, y) {
    return x + y;
}

print(add(2, 3));
```

### Implicit return

Just like in Rust, an implicit return can be used. In fact, the last statement of a block is _always_ the block's return value
regardless of whether it is terminated with a semicolon `';'`. This is different from Rust.

```rust
fn add(x, y) {              // implicit return:
    x + y;                  // value of the last statement (no need for ending semicolon)
                            // is used as the return value
}

fn add2(x) {
    return x + 2;           // explicit return
}

print(add(2, 3));           // prints 5
print(add2(42));            // prints 44
```

### No access to external scope

Functions are not _closures_. They do not capture the calling environment and can only access their own parameters.
They cannot access variables external to the function itself.

```rust
let x = 42;

fn foo() { x }              // <- syntax error: variable 'x' doesn't exist
```

### Passing arguments by value

Functions defined in script always take [`Dynamic`] parameters (i.e. the parameter can be of any type).
It is important to remember that all arguments are passed by _value_, so all functions are _pure_ (i.e. they never modify their arguments).
Any update to an argument will **not** be reflected back to the caller. This can introduce subtle bugs, if not careful.

```rust
fn change(s) {              // 's' is passed by value
    s = 42;                 // only a COPY of 's' is changed
}

let x = 500;
x.change();                 // de-sugars to change(x)
x == 500;                   // 'x' is NOT changed!
```

### Global definitions only

Functions can only be defined at the global level, never inside a block or another function.

```rust
// Global level is OK
fn add(x, y) {
    x + y
}

// The following will not compile
fn do_addition(x) {
    fn add_y(n) {           // <- syntax error: functions cannot be defined inside another function
        n + y
    }

    add_y(x)
}
```

### Functions overloading

Functions can be _overloaded_ and are resolved purely upon the function's _name_ and the _number_ of parameters
(but not parameter _types_, since all parameters are the same type - [`Dynamic`]).
New definitions _overwrite_ previous definitions of the same name and number of parameters.

```rust
fn foo(x,y,z) { print("Three!!! " + x + "," + y + "," + z) }
fn foo(x) { print("One! " + x) }
fn foo(x,y) { print("Two! " + x + "," + y) }
fn foo() { print("None.") }
fn foo(x) { print("HA! NEW ONE! " + x) }    // overwrites previous definition

foo(1,2,3);                 // prints "Three!!! 1,2,3"
foo(42);                    // prints "HA! NEW ONE! 42"
foo(1,2);                   // prints "Two!! 1,2"
foo();                      // prints "None."
```

Members and methods
-------------------

Properties and methods in a Rust custom type registered with the [`Engine`] can be called just like in Rust.

```rust
let a = new_ts();           // constructor function
a.field = 500;              // property access
a.update();                 // method call

update(a);                  // this works, but 'a' is unchanged because only
                            // a COPY of 'a' is passed to 'update' by VALUE
```

Custom types, properties and methods can be disabled via the [`no_object`] feature.

`print` and `debug`
-------------------

The `print` and `debug` functions default to printing to `stdout`, with `debug` using standard debug formatting.

```rust
print("hello");             // prints hello to stdout
print(1 + 2 + 3);           // prints 6 to stdout
print("hello" + 42);        // prints hello42 to stdout
debug("world!");            // prints "world!" to stdout using debug formatting
```

### Overriding `print` and `debug` with callback functions

When embedding Rhai into an application, it is usually necessary to trap `print` and `debug` output
(for logging into a tracking log, for example).

```rust
// Any function or closure that takes an &str argument can be used to override
// print and debug
engine.on_print(|x| println!("hello: {}", x));
engine.on_debug(|x| println!("DEBUG: {}", x));

// Example: quick-'n-dirty logging
let mut log: Vec<String> = Vec::new();

// Redirect print/debug output to 'log'
engine.on_print(|s| log.push(format!("entry: {}", s)));
engine.on_debug(|s| log.push(format!("DEBUG: {}", s)));

// Evaluate script
engine.eval::<()>(script)?;

// 'log' captures all the 'print' and 'debug' output
for entry in log {
    println!("{}", entry);
}
```

Script optimization
===================

Rhai includes an _optimizer_ that tries to optimize a script after parsing.
This can reduce resource utilization and increase execution speed.
Script optimization can be turned off via the [`no_optimize`] feature.

For example, in the following:

```rust
{
    let x = 999;            // NOT eliminated: Rhai doesn't check yet whether a variable is used later on
    123;                    // eliminated: no effect
    "hello";                // eliminated: no effect
    [1, 2, x, x*2, 5];      // eliminated: no effect
    foo(42);                // NOT eliminated: the function 'foo' may have side effects
    666                     // NOT eliminated: this is the return value of the block,
                            // and the block is the last one so this is the return value of the whole script
}
```

Rhai attempts to eliminate _dead code_ (i.e. code that does nothing, for example an expression by itself as a statement, which is allowed in Rhai).
The above script optimizes to:

```rust
{
    let x = 999;
    foo(42);
    666
}
```

Constants propagation is used to remove dead code:

```rust
const ABC = true;
if ABC || some_work() { print("done!"); }   // 'ABC' is constant so it is replaced by 'true'...
if true || some_work() { print("done!"); }  // since '||' short-circuits, 'some_work' is never called
if true { print("done!"); }                 // <- the line above is equivalent to this
print("done!");                             // <- the line above is further simplified to this
                                            //    because the condition is always true
```

These are quite effective for template-based machine-generated scripts where certain constant values
are spliced into the script text in order to turn on/off certain sections.
For fixed script texts, the constant values can be provided in a user-defined [`Scope`] object
to the [`Engine`] for use in compilation and evaluation.

Beware, however, that most operators are actually function calls, and those functions can be overridden,
so they are not optimized away:

```rust
const DECISION = 1;

if DECISION == 1 {          // NOT optimized away because you can define
    :                       // your own '==' function to override the built-in default!
    :
} else if DECISION == 2 {   // same here, NOT optimized away
    :
} else if DECISION == 3 {   // same here, NOT optimized away
    :
} else {
    :
}
```

because no operator functions will be run (in order not to trigger side effects) during the optimization process
(unless the optimization level is set to [`OptimizationLevel::Full`]). So, instead, do this:

```rust
const DECISION_1 = true;
const DECISION_2 = false;
const DECISION_3 = false;

if DECISION_1 {
    :                       // this branch is kept and promoted to the parent level
} else if DECISION_2 {
    :                       // this branch is eliminated
} else if DECISION_3 {
    :                       // this branch is eliminated
} else {
    :                       // this branch is eliminated
}
```

In general, boolean constants are most effective for the optimizer to automatically prune
large `if`-`else` branches because they do not depend on operators.

Alternatively, turn the optimizer to [`OptimizationLevel::Full`]

Here be dragons!
================

Optimization levels
-------------------

[`OptimizationLevel::Full`]: #optimization-levels
[`OptimizationLevel::Simple`]: #optimization-levels
[`OptimizationLevel::None`]: #optimization-levels

There are actually three levels of optimizations: `None`, `Simple` and `Full`.

* `None` is obvious - no optimization on the AST is performed.

* `Simple` (default) performs relatively _safe_ optimizations without causing side effects
  (i.e. it only relies on static analysis and will not actually perform any function calls).

* `Full` is _much_ more aggressive, _including_ running functions on constant arguments to determine their result.
  One benefit to this is that many more optimization opportunities arise, especially with regards to comparison operators.

An [`Engine`]'s optimization level is set via a call to `set_optimization_level`:

```rust
// Turn on aggressive optimizations
engine.set_optimization_level(rhai::OptimizationLevel::Full);
```

When the optimization level is [`OptimizationLevel::Full`], the [`Engine`] assumes all functions to be _pure_ and will _eagerly_
evaluated all function calls with constant arguments, using the result to replace the call. This also applies to all operators
(which are implemented as functions). For instance, the same example above:

```rust
// When compiling the following with OptimizationLevel::Full...

const DECISION = 1;
                            // this condition is now eliminated because 'DECISION == 1'
if DECISION == 1 {          // is a function call to the '==' function, and it returns 'true'
    print("hello!");        // this block is promoted to the parent level
} else {
    print("boo!");          // this block is eliminated because it is never reached
}

print("hello!");            // <- the above is equivalent to this
                            //    ('print' and 'debug' are handled specially)
```

Because of the eager evaluation of functions, many constant expressions will be evaluated and replaced by the result.
This does not happen with [`OptimizationLevel::Simple`] which doesn't assume all functions to be _pure_.

```rust
// When compiling the following with OptimizationLevel::Full...

let x = (1+2)*3-4/5%6;      // <- will be replaced by 'let x = 9'
let y = (1>2) || (3<=4);    // <- will be replaced by 'let y = true'
```

Function side effect considerations
----------------------------------

All of Rhai's built-in functions (and operators which are implemented as functions) are _pure_ (i.e. they do not mutate state
nor cause side any effects, with the exception of `print` and `debug` which are handled specially) so using [`OptimizationLevel::Full`]
is usually quite safe _unless_ you register your own types and functions.

If custom functions are registered, they _may_ be called (or maybe not, if the calls happen to lie within a pruned code block).
If custom functions are registered to replace built-in operators, they will also be called when the operators are used (in an `if`
statement, for example) and cause side-effects.

Function volatility considerations
---------------------------------

Even if a custom function does not mutate state nor cause side effects, it may still be _volatile_, i.e. it _depends_ on the external
environment and is not _pure_. A perfect example is a function that gets the current time - obviously each run will return a different value!
The optimizer, when using [`OptimizationLevel::Full`], _assumes_ that all functions are _pure_, so when it finds constant arguments
it will eagerly execute the function call. This causes the script to behave differently from the intended semantics because
essentially the result of the function call will always be the same value.

Therefore, **avoid using [`OptimizationLevel::Full`]** if you intend to register non-_pure_ custom types and/or functions.

Subtle semantic changes
-----------------------

Some optimizations can alter subtle semantics of the script.  For example:

```rust
if true {                   // condition always true
    123.456;                // eliminated
    hello;                  // eliminated, EVEN THOUGH the variable doesn't exist!
    foo(42)                 // promoted up-level
}

foo(42)                     // <- the above optimizes to this
```

Nevertheless, if the original script were evaluated instead, it would have been an error - the variable `hello` doesn't exist,
so the script would have been terminated at that point with an error return.

In fact, any errors inside a statement that has been eliminated will silently _disappear_:

```rust
print("start!");
if my_decision { /* do nothing... */ }  // eliminated due to no effect
print("end!");

// The above optimizes to:

print("start!");
print("end!");
```

In the script above, if `my_decision` holds anything other than a boolean value, the script should have been terminated due to a type error.
However, after optimization, the entire `if` statement is removed (because an access to `my_decision` produces no side effects),
thus the script silently runs to completion without errors.

Turning off optimizations
-------------------------

It is usually a bad idea to depend on a script failing or such kind of subtleties, but if it turns out to be necessary (why? I would never guess),
turn it off by setting the optimization level to [`OptimizationLevel::None`].

```rust
let engine = rhai::Engine::new();

// Turn off the optimizer
engine.set_optimization_level(rhai::OptimizationLevel::None);
```

`eval` - or "How to Shoot Yourself in the Foot even Easier"
---------------------------------------------------------

Saving the best for last: in addition to script optimizations, there is the ever-dreaded... `eval` function!

```rust
let x = 10;

fn foo(x) { x += 12; x }

let script = "let y = x;";  // build a script
script +=    "y += foo(y);";
script +=    "x + y";

let result = eval(script);  // <- look, JS, we can also do this!

print("Answer: " + result); // prints 42

print("x = " + x);          // prints 10: functions call arguments are passed by value
print("y = " + y);          // prints 32: variables defined in 'eval' persist!

eval("{ let z = y }");      // to keep a variable local, use a statement block

print("z = " + z);          // <- error: variable 'z' not found

"print(42)".eval();         // <- nope... just like 'type_of', method-call style doesn't work
```

Script segments passed to `eval` execute inside the current [`Scope`], so they can access and modify _everything_,
including all variables that are visible at that position in code! It is almost as if the script segments were
physically pasted in at the position of the `eval` call.

```rust
let script = "x += 32";
let x = 10;
eval(script);               // variable 'x' in the current scope is visible!
print(x);                   // prints 42

// The above is equivalent to:
let script = "x += 32";
let x = 10;
x += 32;
print(x);
```

For those who subscribe to the (very sensible) motto of ["`eval` is **evil**"](http://linterrors.com/js/eval-is-evil),
disable `eval` by overriding it, probably with something that throws.

```rust
fn eval(script) { throw "eval is evil! I refuse to run " + script }

let x = eval("40 + 2");     // 'eval' here throws "eval is evil! I refuse to run 40 + 2"
```

Or override it from Rust:

```rust
fn alt_eval(script: String) -> Result<(), EvalAltResult> {
    Err(format!("eval is evil! I refuse to run {}", script).into())
}

engine.register_result_fn("eval", alt_eval);
```
