Arrays
======

{{#include ../links.md}}

Arrays are first-class citizens in Rhai. Like C, arrays are accessed with zero-based, non-negative integer indices.

Array literals are built within square brackets '`[`' ... '`]`' and separated by commas '`,`'.

All elements stored in an array are [`Dynamic`], and the array can freely grow or shrink with elements added or removed.

The Rust type of a Rhai array is `rhai::Array`.

[`type_of()`] an array returns `"array"`.

Arrays are disabled via the [`no_index`] feature.

The maximum allowed size of an array can be controlled via `Engine::set_max_array_size`
(see [maximum size of arrays].


Built-in Functions
-----------------

The following methods (mostly defined in the [`BasicArrayPackage`]({{rootUrl}}/rust/packages.md) but excluded if using a [raw `Engine`]) operate on arrays:

| Function                  | Parameter(s)                                                          | Description                                                                                          |
| ------------------------- | --------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `push`                    | element to insert                                                     | inserts an element at the end                                                                        |
| `+=` operator, `append`   | array to append                                                       | concatenates the second array to the end of the first                                                |
| `+` operator              | first array, second array                                             | concatenates the first array with the second                                                         |
| `insert`                  | element to insert, position<br/>(beginning if <= 0, end if >= length) | insert an element at a certain index                                                                 |
| `pop`                     | _none_                                                                | removes the last element and returns it ([`()`] if empty)                                            |
| `shift`                   | _none_                                                                | removes the first element and returns it ([`()`] if empty)                                           |
| `remove`                  | index                                                                 | removes an element at a particular index and returns it, or returns [`()`] if the index is not valid |
| `len` method and property | _none_                                                                | returns the number of elements                                                                       |
| `pad`                     | element to pad, target length                                         | pads the array with an element to at least a specified length                                        |
| `clear`                   | _none_                                                                | empties the array                                                                                    |
| `truncate`                | target length                                                         | cuts off the array at exactly a specified length (discarding all subsequent elements)                |


Examples
--------

```rust
let y = [2, 3];         // array literal with 2 elements

let y = [2, 3,];        // trailing comma is OK

y.insert(0, 1);         // insert element at the beginning
y.insert(999, 4);       // insert element at the end

y.len == 4;

y[0] == 1;
y[1] == 2;
y[2] == 3;
y[3] == 4;

(1 in y) == true;       // use 'in' to test if an item exists in the array
(42 in y) == false;     // 'in' uses the '==' operator (which users can override)
                        // to check if the target item exists in the array

y[1] = 42;              // array elements can be reassigned

(42 in y) == true;

y.remove(2) == 3;       // remove element

y.len == 3;

y[2] == 4;              // elements after the removed element are shifted

ts.list = y;            // arrays can be assigned completely (by value copy)
let foo = ts.list[1];
foo == 42;

let foo = [1, 2, 3][0];
foo == 1;

fn abc() {
    [42, 43, 44]        // a function returning an array
}

let foo = abc()[0];
foo == 42;

let foo = y[0];
foo == 1;

y.push(4);              // 4 elements
y.push(5);              // 5 elements

y.len == 5;

let first = y.shift();  // remove the first element, 4 elements remaining
first == 1;

let last = y.pop();     // remove the last element, 3 elements remaining
last == 5;

y.len == 3;

for item in y {         // arrays can be iterated with a 'for' statement
    print(item);
}

y.pad(10, "hello");     // pad the array up to 10 elements

y.len == 10;

y.truncate(5);          // truncate the array to 5 elements

y.len == 5;

y.clear();              // empty the array

y.len == 0;
```

`push` and `pad` are only defined for standard built-in types. For custom types, type-specific versions must be registered:

```rust
engine.register_fn("push", |list: &mut Array, item: MyType| list.push(Box::new(item)) );
```
