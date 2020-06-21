`while` Loop
============

{{#include ../links.md}}

```rust
let x = 10;

while x > 0 {
    x = x - 1;
    if x < 6 { continue; }  // skip to the next iteration
    print(x);
    if x == 5 { break; }    // break out of while loop
}
```
