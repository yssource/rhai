Override a Built-in Function
===========================

{{#include ../links.md}}

Any similarly-named function defined in a script overrides any built-in or registered
native Rust function of the same name and number of parameters.

```rust
// Override the built-in function 'to_int'
fn to_int(num) {
    print("Ha! Gotcha! " + num);
}

let x = (123).to_int();

print(x);               // what happens?
```

A registered native Rust function, in turn, overrides any built-in function of the
same name, number and types of parameters.
