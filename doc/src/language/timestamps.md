`timestamp`
===========

{{#include ../links.md}}

Timestamps are provided by the [`BasicTimePackage`][packages] (excluded if using a [raw `Engine`])
via the `timestamp` function.

Timestamps are not available under [`no_std`].

The Rust type of a timestamp is `std::time::Instant` ([`instant::Instant`] in [WASM] builds).

[`type_of()`] a timestamp returns `"timestamp"`.


Built-in Functions
-----------------

The following methods (defined in the [`BasicTimePackage`][packages] but excluded if using a [raw `Engine`]) operate on timestamps:

| Function                      | Parameter(s)                       | Description                                              |
| ----------------------------- | ---------------------------------- | -------------------------------------------------------- |
| `elapsed` method and property | _none_                             | returns the number of seconds since the timestamp        |
| `-` operator                  | later timestamp, earlier timestamp | returns the number of seconds between the two timestamps |


Examples
--------

```rust
let now = timestamp();

// Do some lengthy operation...

if now.elapsed > 30.0 {
    print("takes too long (over 30 seconds)!")
}
```
