`String` Parameters in Rust Functions
====================================

{{#include ../links.md}}

Rust functions accepting parameters of `String` should use `&str` instead because it maps directly to [`ImmutableString`]
which is the type that Rhai uses to represent [strings] internally.

```rust
fn get_len1(s: String) -> i64 { s.len() as i64 }            // <- Rhai will not find this function
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
