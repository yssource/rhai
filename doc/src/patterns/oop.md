Object-Oriented Programming (OOP)
================================

{{#include ../links.md}}

Rhai does not have _objects_ per se, but it is possible to _simulate_ object-oriented programming.


Use Object Maps to Simulate OOP
------------------------------

Rhai's [object maps] has [special support for OOP]({{rootUrl}}/language/object-maps-oop.md).

| Rhai concept                                          | Maps to OOP |
| ----------------------------------------------------- | :---------: |
| [Object maps]                                         |   objects   |
| [Object map] properties holding values                | properties  |
| [Object map] properties that hold [function pointers] |   methods   |

When a property of an [object map] is called like a method function, and if it happens to hold
a valid [function pointer] (perhaps defined via an [anonymous function] or more commonly as a [closure]),
then the call will be dispatched to the actual function with `this` binding to the [object map] itself.


Use Closures to Define Methods
-----------------------------

[Anonymous functions] or [closures] defined as values for [object map] properties take on
a syntactic shape that resembles very closely that of class methods in an OOP language.

Closures also _[capture][automatic currying]_ variables from the defining environment, which is a very
common OOP pattern.  Capturing is accomplished via a feature called _[automatic currying]_ and
can be turned off via the [`no_closure`] feature.


Examples
--------

```rust
let factor = 1;

// Define the object
let obj = #{
        data: 0,                             // object field
        increment: |x| this.data += x,       // 'this' binds to 'obj'
        update: |x| this.data = x * factor,  // 'this' binds to 'obj', 'factor' is captured
        action: || print(this.data)          // 'this' binds to 'obj'
    };

// Use the object
obj.increment(1);
obj.action();                                // prints 1

obj.update(42);
obj.action();                                // prints 42

factor = 2;

obj.update(42);
obj.action();                                // prints 84
```
