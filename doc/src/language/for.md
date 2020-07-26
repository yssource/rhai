`for` Loop
==========

{{#include ../links.md}}

Iterating through a range or an [array], or any type with a registered _iterator_,
is provided by the `for` ... `in` loop.

Like C, `continue` can be used to skip to the next iteration, by-passing all following statements;
`break` can be used to break out of the loop unconditionally.

```rust
// Iterate through string, yielding characters
let s = "hello, world!";

for ch in s {
    if ch > 'z' { continue; }   // skip to the next iteration

    print(ch);

    if x == '@' { break; }      // break out of for loop
}

// Iterate through array
let array = [1, 3, 5, 7, 9, 42];

for x in array {
    if x > 10 { continue; }     // skip to the next iteration

    print(x);

    if x == 42 { break; }       // break out of for loop
}

// The 'range' function allows iterating from first to last-1
for x in range(0, 50) {
    if x > 10 { continue; }     // skip to the next iteration

    print(x);

    if x == 42 { break; }       // break out of for loop
}

// The 'range' function also takes a step
for x in range(0, 50, 3) {      // step by 3
    if x > 10 { continue; }     // skip to the next iteration

    print(x);

    if x == 42 { break; }       // break out of for loop
}

// Iterate through object map
let map = #{a:1, b:3, c:5, d:7, e:9};

// Property names are returned in unsorted, random order
for x in keys(map) {
    if x > 10 { continue; }     // skip to the next iteration

    print(x);

    if x == 42 { break; }       // break out of for loop
}

// Property values are returned in unsorted, random order
for val in values(map) {
    print(val);
}
```
