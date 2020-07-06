Use the Low-Level API to Register a Rust Function
================================================

{{#include ../links.md}}

When a native Rust function is registered with an `Engine` using the `Engine::register_XXX` API,
Rhai transparently converts all function arguments from [`Dynamic`] into the correct types before
calling the function.

For more power and flexibility, there is a _low-level_ API to work directly with [`Dynamic`] values
without the conversions.


Raw Function Registration
-------------------------

The `Engine::register_raw_fn` method is marked _volatile_, meaning that it may be changed without warning.

If this is acceptable, then using this method to register a Rust function opens up more opportunities.

In particular, a reference to the current `Engine` instance is passed as an argument so the Rust function
can also use `Engine` facilities (like evaluating a script).

```rust
engine.register_raw_fn(
    "increment_by",                                         // function name
    &[                                                      // a slice containing parameter types
        std::any::TypeId::of::<i64>(),                      // type of first parameter
        std::any::TypeId::of::<i64>()                       // type of second parameter
    ],
    |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| {    // fixed function signature
        // Arguments are guaranteed to be correct in number and of the correct types.

        // But remember this is Rust, so you can keep only one mutable reference at any one time!
        // Therefore, get a '&mut' reference to the first argument _last_.
        // Alternatively, use `args.split_at_mut(1)` etc. to split the slice first.

        let y: i64 = *args[1].downcast_ref::<i64>()         // get a reference to the second argument
                             .unwrap();                     // then copying it because it is a primary type

        let y: i64 = std::mem::take(args[1]).cast::<i64>(); // alternatively, directly 'consume' it

        let x: &mut i64 = args[0].downcast_mut::<i64>()     // get a '&mut' reference to the
                                 .unwrap();                 // first argument

        *x += y;                                            // perform the action

        Ok(().into())                                       // must be 'Result<Dynamic, Box<EvalAltResult>>'
    }
);

// The above is the same as (in fact, internally they are equivalent):

engine.register_fn("increment_by", |x: &mut i64, y: i64| x += y);
```


Shortcuts
---------

As usual with Rhai, there are shortcuts.  For functions of zero to four parameters, which should be
the majority, use one of the `Engine::register_raw_fn_n` (where `n = 0..4`) methods:

```rust
// Specify parameter types as generics
engine.register_raw_fn_2::<i64, i64>(
    "increment_by",
    |engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]| { ... }
);
```


Closure Signature
-----------------

The closure passed to `Engine::register_raw_fn` takes the following form:

`Fn(engine: &Engine, lib: &Module, args: &mut [&mut Dynamic]) -> Result<Dynamic, Box<EvalAltResult>> + 'static`

where:

* `engine` - a reference to the current [`Engine`], with all configurations and settings.

* `lib` - a reference to the current collection of script-defined functions, as a [`Module`].

* `args` - a reference to a slice containing `&mut` references to [`Dynamic`] values.
  The slice is guaranteed to contain enough arguments _of the correct types_.

Remember, in Rhai, all arguments _except_ the _first_ one are always passed by _value_ (i.e. cloned).
Therefore, it is unnecessary to ever mutate any argument except the first one, as all mutations
will be on the cloned copy.


Extract Arguments
-----------------

To extract an argument from the `args` parameter (`&mut [&mut Dynamic]`), use the following:

| Argument type                  | Access (`n` = argument position)       | Result                                                     |
| ------------------------------ | -------------------------------------- | ---------------------------------------------------------- |
| [Primary type][standard types] | `args[n].clone().cast::<T>()`          | Copy of value.                                             |
| Custom type                    | `args[n].downcast_ref::<T>().unwrap()` | Immutable reference to value.                              |
| Custom type (consumed)         | `mem::take(args[n]).cast::<T>()`       | The _consumed_ value.<br/>The original value becomes `()`. |
| `this` object                  | `args[0].downcast_mut::<T>().unwrap()` | Mutable reference to value.                                |

When there is a mutable reference to the `this` object (i.e. the first argument),
there can be no other immutable references to `args`, otherwise the Rust borrow checker will complain.


Hold Multiple References
------------------------

In order to access a value argument that is expensive to clone _while_ holding a mutable reference
to the first argument, either _consume_ that argument via `mem::take` as above, or use `args.split_at`
to partition the slice:

```rust
// Partition the slice
let (first, rest) = args.split_at_mut(1);

// Mutable reference to the first parameter
let this_ptr = first[0].downcast_mut::<A>().unwrap();

// Immutable reference to the second value parameter
// This can be mutable but there is no point because the parameter is passed by value
let value = rest[0].downcast_ref::<B>().unwrap();
```
