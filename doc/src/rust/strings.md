`String` Parameters in Rust Functions
====================================

{{#include ../links.md}}


`&str` Maps to `ImmutableString`
-------------------------------

Rust functions accepting parameters of `String` should use `&str` instead because it maps directly to
[`ImmutableString`][string] which is the type that Rhai uses to represent [strings] internally.

The parameter type `String` is discouraged because it involves converting an [`ImmutableString`] into a `String`.
Using `ImmutableString` or `&str` is much more efficient.
A common mistake made by novice Rhai users is to register functions with `String` parameters.

```rust
fn get_len1(s: String) -> i64 { s.len() as i64 }            // <- Rhai finds this function, but very inefficient
fn get_len2(s: &str) -> i64 { s.len() as i64 }              // <- Rhai finds this function fine
fn get_len3(s: ImmutableString) -> i64 { s.len() as i64 }   // <- the above is equivalent to this

engine
    .register_fn("len1", get_len1)
    .register_fn("len2", get_len2)
    .register_fn("len3", get_len3);

let len = engine.eval::<i64>("x.len1()")?;                  // error: function 'len1 (&str | ImmutableString)' not found
let len = engine.eval::<i64>("x.len2()")?;                  // works fine
let len = engine.eval::<i64>("x.len3()")?;                  // works fine
```


Avoid `&mut ImmutableString`
---------------------------

Rhai functions can take a first `&mut` parameter.  Usually this is a good idea because it avoids
cloning of the argument (except for primary types where cloning is cheap), so its use is encouraged
even though there is no intention to ever mutate that argument.

`ImmutableString` is an exception to this rule.  While `ImmutableString` is cheap to clone (only
incrementing a reference count), taking a mutable reference to it involves making a private clone
of the underlying string because Rhai has no way to find out whether that parameter will be mutated.

If the `ImmutableString` is not shared by any other variables, then Rhai just returns a mutable
reference to it since nobody else is watching! Otherwise a private copy is made first,
because other reference holders will not expect the `ImmutableString` to ever change
(it is supposed to be _immutable_).

Therefore, avoid using `&mut ImmutableString` as the first parameter of a function unless you really
intend to mutate that string.  Use `ImmutableString` instead.
