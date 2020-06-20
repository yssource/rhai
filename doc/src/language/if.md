`if` Statement
==============

{{#include ../links.md}}

```rust
if foo(x) {
    print("It's true!");
} else if bar == baz {
    print("It's true again!");
} else if ... {
        :
} else if ... {
        :
} else {
    print("It's finally false!");
}
```

All branches of an `if` statement must be enclosed within braces '`{`' .. '`}`', even when there is only one statement.
Like Rust, there is no ambiguity regarding which `if` clause a statement belongs to.

```rust
if (decision) print("I've decided!");
//            ^ syntax error, expecting '{' in statement block
```


`if`-Expressions
---------------

Like Rust, `if` statements can also be used as _expressions_, replacing the `? :` conditional operators
in other C-like languages.

```rust
// The following is equivalent to C: int x = 1 + (decision ? 42 : 123) / 2;
let x = 1 + if decision { 42 } else { 123 } / 2;
x == 22;

let x = if decision { 42 }; // no else branch defaults to '()'
x == ();
```
