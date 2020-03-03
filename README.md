# Rhai - Embedded Scripting for Rust

Rhai is an embedded scripting language for Rust that gives you a safe and easy way to add scripting to your applications.

Rhai's current feature set:

* Easy integration with Rust functions and data types
* Fairly efficient (1 mil iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~0.6 sec debug/~3 sec release for script runner app)
* Easy-to-use language similar to JS+Rust
* Support for overloaded functions
* No additional dependencies

**Note:** Currently, the version is 0.10.1, so the language and API may change before they stabilize.

## Installation

You can install Rhai using crates by adding this line to your dependencies:

```toml
[dependencies]
rhai = "0.10.1"
```

or simply:

```toml
[dependencies]
rhai = "*"
```

to use the latest version.

Beware that in order to use pre-releases (alpha and beta) you need to specify the exact version in your `Cargo.toml`.

## Related

Other cool projects to check out:

* [ChaiScript](http://chaiscript.com/) - A strong inspiration for Rhai.  An embedded scripting language for C++ that I helped created many moons ago, now being lead by my cousin.
* You can also check out the list of [scripting languages for Rust](https://github.com/rust-unofficial/awesome-rust#scripting) on [awesome-rust](https://github.com/rust-unofficial/awesome-rust)

## Examples

The repository contains several examples in the `examples` folder:

* `arrays_and_structs` demonstrates registering a new type to Rhai and the usage of arrays on it
* `custom_types_and_methods` shows how to register a type and methods for it
* `hello` simple example that evaluates an expression and prints the result
* `reuse_scope` evaluates two pieces of code in separate runs, but using a common scope
* `rhai_runner` runs each filename passed to it as a Rhai script
* `simple_fn` shows how to register a Rust function to a Rhai engine
* `repl` a simple REPL, see source code for what it can do at the moment

Examples can be run with the following command:

```bash
cargo run --example name
```

## Example Scripts

We also have a few examples scripts that showcase Rhai's features, all stored in the `scripts` folder:

* `array.rhai` - arrays in Rhai
* `assignment.rhai` - variable declarations
* `comments.rhai` - just comments
* `for1.rhai` - for loops
* `function_decl1.rhai` - a function without parameters
* `function_decl2.rhai` - a function with two parameters
* `function_decl3.rhai` - a function with many parameters
* `if1.rhai` - if example
* `loop.rhai` - endless loop in Rhai, this example emulates a do..while cycle
* `op1.rhai` - just a simple addition
* `op2.rhai` - simple addition and multiplication
* `op3.rhai` - change evaluation order with parenthesis
* `speed_test.rhai` - a simple program to measure the speed of Rhai's interpreter
* `string.rhai`- string operations
* `while.rhai` - while loop

To run the scripts, you can either make your own tiny program, or make use of the `rhai_runner`
example program:

```bash
cargo run --example rhai_runner scripts/any_script.rhai
```

# Hello world

To get going with Rhai, you create an instance of the scripting engine and then run eval.

```rust
extern crate rhai;
use rhai::Engine;

fn main() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("40 + 2") {
        println!("Answer: {}", result);  // prints 42
    }
}
```

You can also evaluate a script file:

```rust
if let Ok(result) = engine.eval_file::<i64>("hello_world.rhai") { ... }
```

If you want to repeatedly evaluate a script, you can compile it first into an AST form:

```rust
// Compile to an AST and store it for later evaluations
let ast = Engine::compile("40 + 2").unwrap();

for _ in 0..42 {
    if let Ok(result) = engine.eval_ast::<i64>(&ast) {
        println!("Answer: {}", result);  // prints 42
    }
}
```

Compiling a script file into AST is also supported:

```rust
let ast = Engine::compile_file("hello_world.rhai").unwrap();
```

# Values and types

The following primitive types are supported natively:

* Integer: `i32`, `u32`, `i64` (default), `u64`
* Floating-point: `f32`, `f64` (default)
* Character: `char`
* Boolean: `bool`
* Array: `rhai::Array`
* Dynamic (i.e. can be anything): `rhai::Dynamic`

# Value conversions

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

# Working with functions

Rhai's scripting engine is very lightweight.  It gets its ability from the functions in your program.  To call these functions, you need to register them with the scripting engine.

```rust
extern crate rhai;
use rhai::{Dynamic, Engine, RegisterFn};

// Normal function
fn add(x: i64, y: i64) -> i64 {
    x + y
}

// Function that returns a Dynamic value
fn get_an_any() -> Dynamic {
    Box::new(42_i64)
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("add", add);

    if let Ok(result) = engine.eval::<i64>("add(40, 2)") {
       println!("Answer: {}", result);  // prints 42
    }

    // Functions that return Dynamic values must use register_dynamic_fn()
    engine.register_dynamic_fn("get_an_any", get_an_any);

    if let Ok(result) = engine.eval::<i64>("get_an_any()") {
       println!("Answer: {}", result);  // prints 42
    }
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

# Working with generic functions

Generic functions can be used in Rhai, but you'll need to register separate instances for each concrete type:

```rust
use std::fmt::Display;

extern crate rhai;
use rhai::{Engine, RegisterFn};

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("print", showit as fn(x: &mut i64)->());
    engine.register_fn("print", showit as fn(x: &mut bool)->());
    engine.register_fn("print", showit as fn(x: &mut String)->());
}
```

You can also see in this example how you can register multiple functions (or in this case multiple instances of the same function) to the same name in script.  This gives you a way to overload functions and call the correct one, based on the types of the arguments, from your script.

# Override built-in functions

Any similarly-named function defined in a script overrides any built-in function.

```rust
// Override the built-in function 'to_int'
fn to_int(num) {
    print("Ha! Gotcha!" + num);
}

print(to_int(123));     // what will happen?
```

# Custom types and methods

Here's an more complete example of working with Rust.  First the example, then we'll break it into parts:

```rust
extern crate rhai;
use rhai::{Engine, RegisterFn};

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

fn main() {
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x") {
        println!("result: {}", result.x); // prints 1001
    }
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
if let Ok(result) = engine.eval::<TestStruct>("let x = new_ts(); x.update(); x") {
    println!("result: {}", result.x); // prints 1001
}
```

In fact, any function with a first argument (either by copy or via a `&mut` reference) can be used as a method-call on that type because internally they are the same thing: methods on a type is implemented as a functions taking an first argument.

```rust
fn foo(ts: &mut TestStruct) -> i64 {
    ts.x
}

engine.register_fn("foo", foo);

if let Ok(result) = engine.eval::<i64>("let x = new_ts(); x.foo()") {
    println!("result: {}", result); // prints 1
}
```

`type_of` works fine with custom types and returns the name of the type:

```rust
let x = new_ts();
print(x.type_of());     // prints "foo::bar::TestStruct"
```

# Getters and setters

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

if let Ok(result) = engine.eval::<i64>("let a = new_ts(); a.x = 500; a.x") {
    println!("result: {}", result);
}
```

### WARNING: Gotcha's with Getters

When you _get_ a property, the value is cloned.  Any update to it downstream will **NOT** be reflected back to the custom type.

This can introduce subtle bugs.  For example:

```rust
fn change(s) {
    s = 42;
}

let a = new_ts();
a.x = 500;
a.x.change();   // Only a COPY of 'a.x' is changed. 'a.x' is NOT changed.
a.x == 500;
```

# Initializing and maintaining state

By default, Rhai treats each engine invocation as a fresh one, persisting only the functions that have been defined but no top-level state.  This gives each one a fairly clean starting place.  Sometimes, though, you want to continue using the same top-level state from one invocation to the next.

In this example, we first create a state with a few initialized variables, then thread the same state through multiple invocations:

```rust
extern crate rhai;
use rhai::{Engine, Scope};

fn main() {
    let mut engine = Engine::new();

    // First create the state
    let mut scope = Scope::new();

    // Then push some initialized variables into the state
    // NOTE: Remember the default numbers used by Rhai are i64 and f64.
    //       Better stick to them or it gets hard to work with other variables in the script.
    scope.push("y".into(), 42_i64);
    scope.push("z".into(), 999_i64);

    // First invocation
    engine.eval_with_scope::<()>(&mut scope, r"
        let x = 4 + 5 - y + z;
        y = 1;
    ").expect("y and z not found?");

    // Second invocation using the same state
    if let Ok(result) = engine.eval_with_scope::<i64>(&mut scope, "x") {
       println!("result: {}", result);  // should print 966
    }

    // Variable y is changed in the script
    assert_eq!(scope.get_value::<i64>("y").unwrap(), 1);
}
```

# Rhai Language guide

## Variables

```rust
let x = 3;
```

## Numeric operators

```rust
let x = (1 + 2) * (6 - 4) / 2;
```

## Comparison operators

You can compare most values of the same data type.  If you compare two values of _different_ data types, the result is always `false`.

```rust
42 == 42;           // true
42 > 42;            // false
"hello" > "foo";    // true
"42" == 42;         // false
42 == 42.0;         // false - i64 is different from f64
```

## Boolean operators

Double boolean operators `&&` and `||` _short-circuit_, meaning that the second operand will not be evaluated if the first one already proves the condition wrong.

Single boolean operators `&` and `|` always evaluate both operands.

```rust
this() || that();   // that() is not evaluated if this() is true
this() && that();   // that() is not evaluated if this() is false

this() | that();    // both this() and that() are evaluated
this() & that();    // both this() and that() are evaluated
```

## If

```rust
if true {
    print("It's true!");
} else if true {
    print("It's true again!");
} else {
    print("It's false!");
}
```

## While

```rust
let x = 10;

while x > 0 {
    print(x);
    if x == 5 { break; }
    x = x - 1;
}
```

## Loop

```rust
let x = 10;

loop {
    print(x);
    x = x - 1;
    if x == 0 { break; }
}
```

## Functions

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

## Arrays

You can create arrays of values, and then access them with numeric indices.

The following standard functions operate on arrays:

* `push` - inserts an element at the end
* `pop` - removes the last element and returns it (() if empty)
* `shift` - removes the first element and returns it (() if empty)
* `len` - returns the number of elements
* `pad` - pads the array with an element until a specified length
* `clear` - empties the array
* `truncate` - cuts off the array at exactly a specified length (discarding all subsequent elements)

```rust
let y = [1, 2, 3];      // 3 elements
y[1] = 42;

print(y[1]);            // prints 42

let foo = [1, 2, 3][0]; // a syntax error for now - cannot index into literals
let foo = ts.list[0];   // a syntax error for now - cannot index into properties
let foo = y[0];         // this works

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

## For loops

```rust
let array = [1, 3, 5, 7, 9, 42];

for x in array {
    print(x);
    if x == 42 { break; }
}

// The range function allows iterating from first..last-1
for x in range(0,50) {
    print(x);
    if x == 42 { break; }
}
```

## Members and methods

```rust
let a = new_ts();
a.x = 500;
a.update();
```

## Strings and Chars

```rust
let name = "Bob";
let middle_initial = 'C';
let last = 'Davis';

let full_name = name + " " + middle_initial + ". " + last;
full_name == "Bob C. Davis";

// String building with different types
let age = 42;
let record = full_name + ": age " + age;
record == "Bob C. Davis: age 42";

// Strings can be indexed to get a character
let c = record[4];
c == 'C';

let c = "foo"[0];   // a syntax error for now - cannot index into literals
let c = ts.s[0];    // a syntax error for now - cannot index into properties
let c = record[0];  // this works

// Unlike Rust, Rhai strings can be modified
record[4] = 'Z';
record == "Bob Z. Davis: age 42";
```

The following standard functions operate on strings:

* `len` - returns the number of characters (not number of bytes) in the string
* `pad` - pads the string with an character until a specified number of characters
* `clear` - empties the string
* `truncate` - cuts off the string at exactly a specified number of characters
* `contains` - checks if a certain character or sub-string occurs in the string
* `replace` - replaces a substring with another
* `trim` - trims the string

```rust
let full_name == " Bob C. Davis ";
full_name.len() == 14;

full_name.trim();
full_name.len() == 12;

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

## Print and Debug

```rust
print("hello");         // prints hello to stdout
print(1 + 2 + 3);       // prints 6 to stdout
print("hello" + 42);    // prints hello42 to stdout
debug("world!");        // prints "world!" to stdout using debug formatting
```

### Overriding Print and Debug with Callback functions

```rust
// Any function that takes a &str argument can be used to override print and debug
engine.on_print(|x: &str| println!("hello: {}", x));
engine.on_debug(|x: &str| println!("DEBUG: {}", x));
```

## Comments

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

## Unary operators

```rust
let number = -5;
number = -5 - +5;
let booly = !true;
```

## Compound assignment operators

```rust
let number = 5;
number += 4;
number -= 3;
number *= 2;
number /= 1;
number %= 3;
number <<= 2;
number >>= 1;
```

The `+=` operator can also be used to build strings:

```rust
let my_str = "abc";
my_str += "ABC";
my_str += 12345;

my_str == "abcABC12345"
```
