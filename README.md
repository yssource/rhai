Rhai - Embedded Scripting for Rust
=================================

Rhai is an embedded scripting language for Rust that gives you a safe and easy way to add scripting to your applications.

Rhai's current feature set:

* Easy integration with Rust functions and data types
* Fairly efficient (1 mil iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~0.6 sec debug/~3 sec release for script runner app)
* Easy-to-use language similar to JS+Rust
* Support for overloaded functions
* Very few additional dependencies (right now only `num-traits` to do checked arithmetic operations)

**Note:** Currently, the version is 0.10.2, so the language and API's may change before they stabilize.

Installation
------------

You can install Rhai using crates by adding this line to your dependencies:

```toml
[dependencies]
rhai = "0.10.2"
```

or simply:

```toml
[dependencies]
rhai = "*"
```

to use the latest version.

Beware that in order to use pre-releases (alpha and beta) you need to specify the exact version in your `Cargo.toml`.

Optional features
-----------------

| Feature      | Description                                                                                                             |
| ------------ | ----------------------------------------------------------------------------------------------------------------------- |
| `debug_msgs` | Print debug messages to stdout (using `println!`) related to function registrations and function calls.                 |
| `no_stdlib`  | Exclude the standard library of utility functions in the build, and only include the minimum necessary functionalities. |
| `unchecked`  | Exclude arithmetic checking in the standard library. Beware that a bad script may panic the entire system!              |

Related
-------

Other cool projects to check out:

* [ChaiScript](http://chaiscript.com/) - A strong inspiration for Rhai.  An embedded scripting language for C++ that I helped created many moons ago, now being lead by my cousin.
* You can also check out the list of [scripting languages for Rust](https://github.com/rust-unofficial/awesome-rust#scripting) on [awesome-rust](https://github.com/rust-unofficial/awesome-rust)

Examples
--------

A number of examples can be found in the `examples` folder:

| Example                    | Description                                                               |
| -------------------------- | ------------------------------------------------------------------------- |
| `arrays_and_structs`       | demonstrates registering a new type to Rhai and the usage of arrays on it |
| `custom_types_and_methods` | shows how to register a type and methods for it                           |
| `hello`                    | simple example that evaluates an expression and prints the result         |
| `reuse_scope`              | evaluates two pieces of code in separate runs, but using a common scope   |
| `rhai_runner`              | runs each filename passed to it as a Rhai script                          |
| `simple_fn`                | shows how to register a Rust function to a Rhai engine                    |
| `repl`                     | a simple REPL, interactively evaluate statements from stdin               |

Examples can be run with the following command:

```bash
cargo run --example name
```

The `repl` example is a particularly good one as it allows you to interactively try out Rhai's
language features in a standard REPL (**R**ead-**E**val-**P**rint **L**oop).

Example Scripts
---------------

There are also a number of examples scripts that showcase Rhai's features, all in the `scripts` folder:

| Script                | Description                                                   |
| --------------------- | ------------------------------------------------------------- |
| `array.rhai`          | arrays in Rhai                                                |
| `assignment.rhai`     | variable declarations                                         |
| `comments.rhai`       | just comments                                                 |
| `for1.rhai`           | for loops                                                     |
| `function_decl1.rhai` | a function without parameters                                 |
| `function_decl2.rhai` | a function with two parameters                                |
| `function_decl3.rhai` | a function with many parameters                               |
| `if1.rhai`            | if example                                                    |
| `loop.rhai`           | endless loop in Rhai, this example emulates a do..while cycle |
| `op1.rhai`            | just a simple addition                                        |
| `op2.rhai`            | simple addition and multiplication                            |
| `op3.rhai`            | change evaluation order with parenthesis                      |
| `speed_test.rhai`     | a simple program to measure the speed of Rhai's interpreter   |
| `string.rhai`         | string operations                                             |
| `while.rhai`          | while loop                                                    |

To run the scripts, either make a tiny program or use of the `rhai_runner` example:

```bash
cargo run --example rhai_runner scripts/any_script.rhai
```

Hello world
-----------

To get going with Rhai, create an instance of the scripting engine and then call `eval`:

```rust
use rhai::{Engine, EvalAltResult};

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    let result = engine.eval::<i64>("40 + 2")?;

    println!("Answer: {}", result);  // prints 42
}
```

You can also evaluate a script file:

```rust
let result = engine.eval_file::<i64>("hello_world.rhai")?;
```

If you want to repeatedly evaluate a script, you can _compile_ it first into an AST (abstract syntax tree) form:

```rust
use rhai::Engine;

let mut engine = Engine::new();

// Compile to an AST and store it for later evaluations
let ast = engine.compile("40 + 2")?;

for _ in 0..42 {
    let result = engine.eval_ast::<i64>(&ast)?;

    println!("Answer: {}", result);  // prints 42
}
```

Compiling a script file is also supported:

```rust
use rhai::Engine;

let mut engine = Engine::new();

let ast = engine.compile_file("hello_world.rhai".into()).unwrap();
```

Rhai also allows you to work _backwards_ from the other direction - i.e. calling a Rhai-scripted function from Rust.
You do this via `call_fn`, which takes a compiled AST (output from `compile`) and the
function call arguments:

```rust
use rhai::Engine;

let mut engine = Engine::new();

// Define a function in a script and compile to AST
let ast = engine.compile("fn hello(x, y) { x.len() + y }")?;

// Evaluate the function in the AST, passing arguments into the script as a tuple
// (beware, arguments must be of the correct types because Rhai does not have built-in type conversions)
let result: i64 = engine.call_fn("hello", &ast, (&mut String::from("abc"), &mut 123_i64))?;
```

Values and types
----------------

The following primitive types are supported natively:

| Category                       | Types                                  |
| ------------------------------ | -------------------------------------- |
| Integer                        | `i32`, `u32`, `i64` _(default)_, `u64` |
| Floating-point                 | `f32`, `f64` _(default)_               |
| Character                      | `char`                                 |
| Boolean                        | `bool`                                 |
| Array                          | `rhai::Array`                          |
| Dynamic (i.e. can be anything) | `rhai::Dynamic`                        |

Value conversions
-----------------

All types are treated strictly separate by Rhai, meaning that `i32` and `i64` and `u32` are completely different; you cannot even add them together.

There is a `to_float` function to convert a supported number to an `f64`, and a `to_int` function to convert a supported number to `i64` and that's about it. For other conversions you can register your own conversion functions.

There is also a `type_of` function to detect the type of a value.

```rust
let x = 42;
let y = x * 100.0;              // error: cannot multiply i64 with f64
let y = x.to_float() * 100.0;   // works
let z = y.to_int() + x;         // works

let c = 'X';                    // character
print("c is '" + c + "' and its code is " + c.to_int());    // prints "c is 'X' and its code is 88"

// Use 'type_of' to get the type of variables
type_of(c) == "char";
type_of(x) == "i64";
y.type_of() == "f64";

if z.type_of() == "string" {
    do_something_with_strong(z);
}
```

Working with functions
----------------------

Rhai's scripting engine is very lightweight.  It gets its ability from the functions in your program.  To call these functions, you need to register them with the scripting engine.

```rust
use rhai::{Engine, EvalAltResult};
use rhai::RegisterFn;                       // include the `RegisterFn` trait to use `register_fn`
use rhai::{Dynamic, RegisterDynamicFn};     // include the `RegisterDynamicFn` trait to use `register_dynamic_fn`

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

    println!("Answer: {}", result);  // prints 42

    // Functions that return Dynamic values must use register_dynamic_fn()
    engine.register_dynamic_fn("get_an_any", get_an_any);

    let result = engine.eval::<i64>("get_an_any()")?;

    println!("Answer: {}", result);  // prints 42

    Ok(())
}
```

To return a `Dynamic` value, simply `Box` it and return it.

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

Generic functions can be used in Rhai, but you'll need to register separate instances for each concrete type:

```rust
use std::fmt::Display;

use rhai::{Engine, RegisterFn};

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main()
{
    let mut engine = Engine::new();

    engine.register_fn("print", showit as fn(x: &mut i64)->());
    engine.register_fn("print", showit as fn(x: &mut bool)->());
    engine.register_fn("print", showit as fn(x: &mut String)->());
}
```

You can also see in this example how you can register multiple functions (or in this case multiple instances of the same function) to the same name in script.  This gives you a way to overload functions and call the correct one, based on the types of the arguments, from your script.

Fallible functions
------------------

If your function is _fallible_ (i.e. it returns a `Result<_, Error>`),  you can register it with `register_result_fn` (using the `RegisterResultFn` trait).

Your function must return `Result<_, EvalAltResult>`. `EvalAltResult` implements `From<&str>` and `From<String>` etc. and the error text gets converted into `EvalAltResult::ErrorRuntime`.

```rust
use rhai::{Engine, EvalAltResult, Position};
use rhai::RegisterResultFn;     // include the `RegisterResultFn` trait to use `register_result_fn`

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
       println!("Error: {:?}", error);  // prints ErrorRuntime("Division by zero detected!", (1, 1)")
    }
}
```

Overriding built-in functions
----------------------------

Any similarly-named function defined in a script overrides any built-in function.

```rust
// Override the built-in function 'to_int'
fn to_int(num) {
    print("Ha! Gotcha!" + num);
}

print(to_int(123));     // what will happen?
```

Custom types and methods
-----------------------

Here's an more complete example of working with Rust.  First the example, then we'll break it into parts:

```rust
use rhai::{Engine, EvalAltResult};
use rhai::RegisterFn;

#[derive(Clone)]
struct TestStruct {
    x: i64
}

impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

    println!("result: {}", result.x); // prints 1001

    Ok(())
}
```

First, for each type we use with the engine, we need to be able to Clone.  This allows the engine to pass by value and still keep its own state.

```rust
#[derive(Clone)]
struct TestStruct {
    x: i64
}
```

Next, we create a few methods that we'll later use in our scripts.  Notice that we register our custom type with the engine.

```rust
impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();
```

To use methods and functions with the engine, we need to register them.  There are some convenience functions to help with this.  Below I register update and new with the engine.

*Note: the engine follows the convention that methods use a &mut first parameter so that invoking methods can update the value in memory.*

```rust
engine.register_fn("update", TestStruct::update);
engine.register_fn("new_ts", TestStruct::new);
```

Finally, we call our script.  The script can see the function and method we registered earlier.  We need to get the result back out from script land just as before, this time casting to our custom struct type.

```rust
let result = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x")?;

println!("result: {}", result.x); // prints 1001
```

In fact, any function with a first argument (either by copy or via a `&mut` reference) can be used as a method-call on that type because internally they are the same thing: methods on a type is implemented as a functions taking an first argument.

```rust
fn foo(ts: &mut TestStruct) -> i64 {
    ts.x
}

engine.register_fn("foo", foo);

let result = engine.eval::<i64>("let x = new_ts(); x.foo()")?;

println!("result: {}", result); // prints 1
```

`type_of` works fine with custom types and returns the name of the type:

```rust
let x = new_ts();
print(x.type_of());     // prints "foo::bar::TestStruct"
```

If you use `register_type_with_name` to register the custom type with a special pretty-print name, `type_of` will return that instead.

Getters and setters
-------------------

Similarly, you can work with members of your custom types.  This works by registering a 'get' or a 'set' function for working with your struct.

For example:

```rust
#[derive(Clone)]
struct TestStruct {
    x: i64
}

impl TestStruct {
    fn get_x(&mut self) -> i64 {
        self.x
    }

    fn set_x(&mut self, new_x: i64) {
        self.x = new_x;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();

engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
engine.register_fn("new_ts", TestStruct::new);

let result = engine.eval::<i64>("let a = new_ts(); a.x = 500; a.x")?;

println!("result: {}", result);
```

Initializing and maintaining state
---------------------------------

By default, Rhai treats each engine invocation as a fresh one, persisting only the functions that have been defined but no top-level state.  This gives each one a fairly clean starting place.  Sometimes, though, you want to continue using the same top-level state from one invocation to the next.

In this example, we first create a state with a few initialized variables, then thread the same state through multiple invocations:

```rust
use rhai::{Engine, Scope, EvalAltResult};

fn main() -> Result<(), EvalAltResult>
{
    let mut engine = Engine::new();

    // First create the state
    let mut scope = Scope::new();

    // Then push some initialized variables into the state
    // NOTE: Remember the default numbers used by Rhai are i64 and f64.
    //       Better stick to them or it gets hard to work with other variables in the script.
    scope.push("y".into(), 42_i64);
    scope.push("z".into(), 999_i64);

    // First invocation
    // (the second boolean argument indicates that we don't need to retain function definitions
    // because we didn't declare any!)
    engine.eval_with_scope::<()>(&mut scope, false, r"
        let x = 4 + 5 - y + z;
        y = 1;
    ")?;

    // Second invocation using the same state
    let result = engine.eval_with_scope::<i64>(&mut scope, false, "x")?;

    println!("result: {}", result);  // should print 966

    // Variable y is changed in the script
    assert_eq!(scope.get_value::<i64>("y")?, 1);

    Ok(())
}
```

Rhai Language guide
===================

Comments
--------

```rust
let /* intruder comment */ name = "Bob";
// This is a very important comment
/* This comment spans
   multiple lines, so it
   only makes sense that
   it is even more important */

/* Fear not, Rhai satisfies all your nesting
   needs with nested comments:
   /*/*/*/*/**/*/*/*/*/
*/
```

Variables
---------

Variables in Rhai follow normal naming rules (i.e. must contain only ASCII letters, digits and '`_`' underscores).

```rust
let x = 3;
```

Numbers
-------

| Format           | Type                                           |
| ---------------- | ---------------------------------------------- |
| `123_345`, `-42` | `i64` in decimal, '`_`' separators are ignored |
| `0o07_76`        | `i64` in octal, '`_`' separators are ignored   |
| `0xabcd_ef`      | `i64` in hex, '`_`' separators are ignored     |
| `0b0101_1001`    | `i64` in binary, '`_`' separators are ignored  |
| `123_456.789`    | `f64`, '`_`' separators are ignored            |

Numeric operators
-----------------

```rust
let x = (1 + 2) * (6 - 4) / 2;  // arithmetic
let reminder = 42 % 10;         // modulo
let power = 42 ~ 2;             // power (i64 and f64 only)
let left_shifted = 42 << 3;     // left shift
let right_shifted = 42 >> 3;    // right shift
let bit_op = 42 | 99;           // bit masking
```

Unary operators
---------------

```rust
let number = -5;
number = -5 - +5;
let booly = !true;
```

Numeric functions
-----------------

The following standard functions (defined in the standard library but excluded if `no_stdlib`) operate on `i8`, `i16`, `i32`, `i64`, `f32` and `f64` only:

| Function   | Description                         |
| ---------- | ----------------------------------- |
| `abs`      | absolute value                      |
| `to_int`   | converts an `f32` or `f64` to `i64` |
| `to_float` | converts an integer type to `f64`   |

Floating-point functions
------------------------

The following standard functions (defined in the standard library but excluded if `no_stdlib`) operate on `f64` only:

| Category         | Functions                                                    |
| ---------------- | ------------------------------------------------------------ |
| Trigonometry     | `sin`, `cos`, `tan`, `sinh`, `cosh`, `tanh` in degrees       |
| Arc-trigonometry | `asin`, `acos`, `atan`, `asinh`, `acosh`, `atanh` in degrees |
| Square root      | `sqrt`                                                       |
| Exponential      | `exp` (base _e_)                                             |
| Logarithmic      | `ln` (base _e_), `log10` (base 10), `log` (any base)         |
| Rounding         | `floor`, `ceiling`, `round`, `int`, `fraction`               |
| Tests            | `is_nan`, `is_finite`, `is_infinite`                         |

Strings and Chars
-----------------

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

// Strings can be indexed to get a character
let c = record[4];
c == 'C';

ts.s = record;

let c = ts.s[4];
c == 'C';

let c = "foo"[0];
c == 'f';

let c = ("foo" + "bar")[5];
c == 'r';

// Escape sequences in strings
record += " \u2764\n";                  // escape sequence of '❤' in Unicode
record == "Bob C. Davis: age 42 ❤\n";   // '\n' = new-line

// Unlike Rust, Rhai strings can be modified
record[4] = '\x58'; // 0x58 = 'X'
record == "Bob X. Davis: age 42 ❤\n";
```

The following standard functions (defined in the standard library but excluded if `no_stdlib`) operate on strings:

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

You can create arrays of values, and then access them with numeric indices.

The following functions (defined in the standard library but excluded if `no_stdlib`) operate on arrays:

| Function   | Description                                                                           |
| ---------- | ------------------------------------------------------------------------------------- |
| `push`     | inserts an element at the end                                                         |
| `pop`      | removes the last element and returns it (`()` if empty)                               |
| `shift`    | removes the first element and returns it (`()` if empty)                              |
| `len`      | returns the number of elements                                                        |
| `pad`      | pads the array with an element until a specified length                               |
| `clear`    | empties the array                                                                     |
| `truncate` | cuts off the array at exactly a specified length (discarding all subsequent elements) |

Examples:

```rust
let y = [1, 2, 3];      // 3 elements
y[1] = 42;

print(y[1]);            // prints 42

ts.list = y;            // arrays can be assigned completely (by value copy)
let foo = ts.list[1];
foo == 42;

let foo = [1, 2, 3][0];
foo == 1;

fn abc() { [42, 43, 44] }

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

`push` and `pad` are only defined for standard built-in types.  If you want to use them with
your own custom type, you need to define a specific override:

```rust
engine.register_fn("push",
    |list: &mut Array, item: MyType| list.push(Box::new(item))
);
```

The type of a Rhai array is `rhai::Array`. `type_of()` returns `"array"`.

Comparison operators
--------------------

You can compare most values of the same data type.  If you compare two values of _different_ data types, the result is always `false`.

```rust
42 == 42;           // true
42 > 42;            // false
"hello" > "foo";    // true
"42" == 42;         // false
42 == 42.0;         // false - i64 is different from f64
```

Boolean operators
-----------------

Double boolean operators `&&` and `||` _short-circuit_, meaning that the second operand will not be evaluated if the first one already proves the condition wrong.

Single boolean operators `&` and `|` always evaluate both operands.

```rust
this() || that();   // that() is not evaluated if this() is true
this() && that();   // that() is not evaluated if this() is false

this() | that();    // both this() and that() are evaluated
this() & that();    // both this() and that() are evaluated
```

Compound assignment operators
----------------------------

```rust
let number = 5;
number += 4;    // number = number + 4
number -= 3;    // number = number - 3
number *= 2;    // number = number * 2
number /= 1;    // number = number / 1
number %= 3;    // number = number % 3
number <<= 2;   // number = number << 2
number >>= 1;   // number = number >> 1
```

The `+=` operator can also be used to build strings:

```rust
let my_str = "abc";
my_str += "ABC";
my_str += 12345;

my_str == "abcABC12345"
```

If
--

```rust
if true {
    print("It's true!");
} else if true {
    print("It's true again!");
} else {
    print("It's false!");
}
```

While
-----

```rust
let x = 10;

while x > 0 {
    print(x);
    if x == 5 { break; }
    x = x - 1;
}
```

Loop
----

```rust
let x = 10;

loop {
    print(x);
    x = x - 1;
    if x == 0 { break; }
}
```

For
---

```rust
let array = [1, 3, 5, 7, 9, 42];

// Iterate through array
for x in array {
    print(x);
    if x == 42 { break; }
}

// The 'range' function allows iterating from first..last-1
for x in range(0, 50) {
    print(x);
    if x == 42 { break; }
}
```

Return
------

```rust
return;     // equivalent to return ();

return 123 + 456;
```

Errors and Exceptions
---------------------

```rust
if some_bad_condition_has_happened {
    throw error;  // 'throw' takes a string to form the exception text
}

throw;  // no exception text
```

All of `Engine`'s evaluation/consuming methods return `Result<T, rhai::EvalAltResult>` with `EvalAltResult` holding error information.

Exceptions thrown via `throw` in the script can be captured by matching `Err(EvalAltResult::ErrorRuntime(reason, position))` with the exception text captured by the `reason` parameter.

```rust
let result = engine.eval::<i64>(&mut scope, r#"
    let x = 42;

    if x > 0 {
        throw x + " is too large!";
    }
"#);

println!(result);   // prints "Runtime error: 42 is too large! (line 5, position 15)"
```

Functions
---------

Rhai supports defining functions in script:

```rust
fn add(x, y) {
    return x + y;
}

print(add(2, 3));
```

Just like in Rust, you can also use an implicit return.

```rust
fn add(x, y) {
    x + y
}

print(add(2, 3));
```

Remember that functions defined in script always take `Dynamic` arguments (i.e. the arguments can be of any type).

However, all arguments are passed by _value_, so all functions are _pure_ (i.e. they never modify their arguments).
Any update to an argument will **not** be reflected back to the caller. This can introduce subtle bugs, if you are not careful.

```rust
fn change(s) {
    s = 42;     // only a COPY of 'x' is changed
}

let x = 500;
x.change();
x == 500;       // 'x' is NOT changed!
```

Furthermore, functions can only be defined at the top level, never inside a block or another function.

```rust
// Top level is OK
fn add(x, y) {
    x + y
}

// The following will not compile
fn do_addition(x) {
    fn add_y(n) {   // functions cannot be defined inside another function
        n + y
    }

    add_y(x)
}
```

Members and methods
-------------------

```rust
let a = new_ts();
a.x = 500;
a.update();
```

`print` and `debug`
-------------------

```rust
print("hello");         // prints hello to stdout
print(1 + 2 + 3);       // prints 6 to stdout
print("hello" + 42);    // prints hello42 to stdout
debug("world!");        // prints "world!" to stdout using debug formatting
```

### Overriding `print` and `debug` with callback functions

```rust
// Any function or closure that takes an &str argument can be used to override print and debug
engine.on_print(|x| println!("hello: {}", x));
engine.on_debug(|x| println!("DEBUG: {}", x));

// Example: quick-'n-dirty logging
let mut log: Vec<String> = Vec::new();

// Redirect print/debug output to 'log'
engine.on_print(|s| log.push(format!("entry: {}", s)));
engine.on_debug(|s| log.push(format!("DEBUG: {}", s)));

// Evalulate script
engine.eval::<()>(script)?;

// 'log' captures all the 'print' and 'debug' output
for entry in log {
    println!("{}", entry);
}
```
