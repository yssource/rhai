Anonymous Functions
===================

{{#include ../links.md}}

Sometimes it gets tedious to define separate functions only to dispatch them via single [function pointers].
This scenario is especially common when simulating object-oriented programming ([OOP]).

```rust
// Define object
let obj = #{
    data: 42,
    increment: Fn("inc_obj"),   // use function pointers to
    decrement: Fn("dec_obj"),   // refer to method functions
    print: Fn("print_obj")
};

// Define method functions one-by-one
fn inc_obj(x) { this.data += x; }
fn dec_obj(x) { this.data -= x; }
fn print_obj() { print(this.data); }
```

The above can be replaced by using _anonymous functions_ which have the same syntax as Rust's closures
(but they are **NOT** closures, merely syntactic sugar):

```rust
let obj = #{
    data: 42,
    increment: |x| this.data += x,          // one-liner
    decrement: |x| this.data -= x,
    print_obj: || { print(this.data); }     // full function body
};
```

The anonymous functions will be hoisted into separate functions in the global namespace.
The above is equivalent to:

```rust
let obj = #{
    data: 42,
    increment: Fn("anon_fn_1000"),
    decrement: Fn("anon_fn_1001"),
    print: Fn("anon_fn_1002")
};

fn anon_fn_1000(x) { this.data += x; }
fn anon_fn_1001(x) { this.data -= x; }
fn anon_fn_1002() { print this.data; }
```


WARNING - NOT Closures
----------------------

Remember: anonymous functions, though having the same syntax as Rust _closures_, are themselves
**not** closures.  In particular, they do not capture their execution environment.  They are more like
Rust's function pointers.

They do, however, _capture_ variable _values_ from their execution environment, unless the [`no_capture`]
feature is turned on.  This is accomplished via [automatic currying][capture].
