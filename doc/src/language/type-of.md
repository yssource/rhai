`type_of`
=========

{{#include ../links.md}}

The `type_of` function detects the actual type of a value.

This is useful because all variables are [`Dynamic`] in nature.

```rust
// Use 'type_of()' to get the actual types of values
type_of('c') == "char";
type_of(42) == "i64";

let x = 123;
x.type_of() == "i64";       // method-call style is also OK
type_of(x) == "i64";

x = 99.999;
type_of(x) == "f64";

x = "hello";
if type_of(x) == "string" {
    do_something_with_string(x);
}
```
