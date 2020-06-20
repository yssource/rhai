Numeric Operators
=================

{{#include ../links.md}}

Numeric operators generally follow C styles.

Unary Operators
---------------

| Operator | Description |
| -------- | ----------- |
| `+`      | Plus        |
| `-`      | Negative    |

```rust
let number = -5;

number = -5 - +5;
```

Binary Operators
----------------

| Operator | Description                                          | Integers only |
| -------- | ---------------------------------------------------- | :-----------: |
| `+`      | Plus                                                 |               |
| `-`      | Minus                                                |               |
| `*`      | Multiply                                             |               |
| `/`      | Divide (integer division if acting on integer types) |               |
| `%`      | Modulo (remainder)                                   |               |
| `~`      | Power                                                |               |
| `&`      | Binary _And_ bit-mask                                |      Yes      |
| `\|`     | Binary _Or_ bit-mask                                 |      Yes      |
| `^`      | Binary _Xor_ bit-mask                                |      Yes      |
| `<<`     | Left bit-shift                                       |      Yes      |
| `>>`     | Right bit-shift                                      |      Yes      |

```rust
let x = (1 + 2) * (6 - 4) / 2;  // arithmetic, with parentheses

let reminder = 42 % 10;         // modulo

let power = 42 ~ 2;             // power (i64 and f64 only)

let left_shifted = 42 << 3;     // left shift

let right_shifted = 42 >> 3;    // right shift

let bit_op = 42 | 99;           // bit masking
```
