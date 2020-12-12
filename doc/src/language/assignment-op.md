Compound Assignment Operators
=============================

{{#include ../links.md}}


```rust
let number = 9;

number += 8;            // number = number + 8

number -= 7;            // number = number - 7

number *= 6;            // number = number * 6

number /= 5;            // number = number / 5

number %= 4;            // number = number % 4

number ~= 3;            // number = number ~ 3

number <<= 2;           // number = number << 2

number >>= 1;           // number = number >> 1

number &= 0x00ff;       // number = number & 0x00ff;

number |= 0x00ff;       // number = number | 0x00ff;

number ^= 0x00ff;       // number = number ^ 0x00ff;
```


The Flexible `+=`
----------------

The `+=` operator can also be used to build [strings]:

```rust
let my_str = "abc";
my_str += "ABC";
my_str += 12345;

my_str == "abcABC12345"
```

It may also be used to concatenate [arrays]:

```rust
let my_array = [1, 2, 3];
my_array += [4, 5];

my_array == [1, 2, 3, 4, 5];
```

or mix two [object maps] together:

```rust
let my_obj = #{a:1, b:2};
my_obj += #{c:3, d:4, e:5};

my_obj.len() == 5;
```

In fact, the `+` and `+=` operators are usually [overloaded][function overloading] when
something is to be _added_ to an existing type.
