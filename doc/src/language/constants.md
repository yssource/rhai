Constants
=========

{{#include ../links.md}}

Constants can be defined using the `const` keyword and are immutable.

Constants follow the same naming rules as [variables].

```rust
const x = 42;
print(x * 2);       // prints 84
x = 123;            // <- syntax error: cannot assign to constant
```

Unlike variables which need not have initial values (default to [`()`]),
constants must be assigned one, and it must be a constant _value_, not an expression.

```rust
const x = 40 + 2;   // <- syntax error: cannot assign expression to constant
```
