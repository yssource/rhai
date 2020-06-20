Value Conversions
=================

{{#include ../links.md}}

The `to_float` function converts a supported number to `FLOAT` (defaults to `f64`).

The `to_int` function converts a supported number to `INT` (`i32` or `i64` depending on [`only_i32`]).

That's it; for other conversions, register custom conversion functions.

```rust
let x = 42;
let y = x * 100.0;              // <- error: cannot multiply i64 with f64
let y = x.to_float() * 100.0;   // works
let z = y.to_int() + x;         // works

let c = 'X';                    // character
print("c is '" + c + "' and its code is " + c.to_int());    // prints "c is 'X' and its code is 88"
```
