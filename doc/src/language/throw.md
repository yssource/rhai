Throw Exception on Error
=======================

{{#include ../links.md}}

All of [`Engine`]'s evaluation/consuming methods return `Result<T, Box<rhai::EvalAltResult>>`
with `EvalAltResult` holding error information.

To deliberately return an error during an evaluation, use the `throw` keyword.

```rust
if some_bad_condition_has_happened {
    throw error;    // 'throw' takes a string as the exception text
}

throw;              // defaults to empty exception text: ""
```

Exceptions thrown via `throw` in the script can be captured by matching `Err(Box<EvalAltResult::ErrorRuntime(` _reason_ `,` _position_ `)>)`
with the exception text captured by the first parameter.

```rust
let result = engine.eval::<i64>(r#"
    let x = 42;

    if x > 0 {
        throw x + " is too large!";
    }
"#);

println!(result);   // prints "Runtime error: 42 is too large! (line 5, position 15)"
```
