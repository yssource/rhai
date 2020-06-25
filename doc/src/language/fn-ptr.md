Function Pointers
=================

It is possible to store a _function pointer_ in a variable just like a normal value.
In fact, internally a function pointer simply stores the _name_ of the function as a string.

Call a function pointer using the `call` method, which needs to be called in method-call style.


Built-in Functions
------------------

The following standard methods (mostly defined in the [`BasicFnPackage`]({{rootUrl}}/rust/packages.md) but excluded if
using a [raw `Engine`]) operate on [strings]:

| Function                   | Parameter(s) | Description                                                           |
| -------------------------- | ------------ | --------------------------------------------------------------------- |
| `name` method and property | _none_       | returns the name of the function encapsulated by the function pointer |


Examples
--------

```rust
fn foo(x) { 41 + x }

let func = Fn("foo");       // use the 'Fn' function to create a function pointer

print(func);                // prints 'Fn(foo)'

let func = fn_name.Fn();    // <- error: 'Fn' cannot be called in method-call style

func.type_of() == "Fn";     // type_of() as function pointer is 'Fn'

func.name == "foo";

func.call(1) == 42;         // call a function pointer with the 'call' method

foo(1) == 42;               // <- the above de-sugars to this

call(func, 1);              //<- error: 'call (Fn, i64)' is not a registered function

let len = Fn("len");        // 'Fn' also works with registered native Rust functions

len.call("hello") == 5;

let add = Fn("+");          // 'Fn' works with built-in operators also

add.call(40, 2) == 42;

let fn_name = "hello";      // the function name does not have to exist yet

let hello = Fn(fn_name + "_world");

hello.call(0);              // error: function not found - "hello_world (i64)"
```
