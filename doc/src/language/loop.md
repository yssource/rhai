Infinite `loop`
===============

{{#include ../links.md}}

```rust
let x = 10;

loop {
    x = x - 1;
    if x > 5 { continue; }  // skip to the next iteration
    print(x);
    if x == 0 { break; }    // break out of loop
}
```
