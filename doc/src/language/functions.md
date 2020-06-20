Functions
=========

{{#include ../links.md}}

Rhai supports defining functions in script (unless disabled with [`no_function`]):

```rust
fn add(x, y) {
    return x + y;
}

fn sub(x, y,) {     // trailing comma in parameters list is OK
    return x - y;
}

print(add(2, 3));   // prints 5
print(sub(2, 3,));  // prints -1 - trailing comma in arguments list is OK
```

Implicit Return
---------------

Just like in Rust, an implicit return can be used. In fact, the last statement of a block is _always_ the block's return value
regardless of whether it is terminated with a semicolon `';'`. This is different from Rust.

```rust
fn add(x, y) {      // implicit return:
    x + y;          // value of the last statement (no need for ending semicolon)
                    // is used as the return value
}

fn add2(x) {
    return x + 2;   // explicit return
}

print(add(2, 3));   // prints 5
print(add2(42));    // prints 44
```

No Access to External Scope
--------------------------

Functions are not _closures_. They do not capture the calling environment and can only access their own parameters.
They cannot access variables external to the function itself.

```rust
let x = 42;

fn foo() { x }      // <- syntax error: variable 'x' doesn't exist
```

Passing Arguments by Value
-------------------------

Functions defined in script always take [`Dynamic`] parameters (i.e. the parameter can be of any type).
It is important to remember that all arguments are passed by _value_, so all functions are _pure_
(i.e. they never modify their arguments).

Any update to an argument will **not** be reflected back to the caller.

This can introduce subtle bugs, if not careful, especially when using the _method-call_ style.

```rust
fn change(s) {      // 's' is passed by value
    s = 42;         // only a COPY of 's' is changed
}

let x = 500;
x.change();         // de-sugars to 'change(x)'
x == 500;           // 'x' is NOT changed!
```

Global Definitions Only
----------------------

Functions can only be defined at the global level, never inside a block or another function.

```rust
// Global level is OK
fn add(x, y) {
    x + y
}

// The following will not compile
fn do_addition(x) {
    fn add_y(n) {   // <- syntax error: functions cannot be defined inside another function
        n + y
    }

    add_y(x)
}
```

Unlike C/C++, functions can be defined _anywhere_ within the global level. A function does not need to be defined
prior to being used in a script; a statement in the script can freely call a function defined afterwards.
This is similar to Rust and many other modern languages.
