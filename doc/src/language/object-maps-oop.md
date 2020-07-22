Special Support for OOP via Object Maps
======================================

{{#include ../links.md}}

[Object maps] can be used to simulate [object-oriented programming (OOP)][OOP] by storing data
as properties and methods as properties holding [function pointers].

If an [object map]'s property holds a [function pointer], the property can simply be called like
a normal method in method-call syntax.  This is a _short-hand_ to avoid the more verbose syntax
of using the `call` function keyword.

When a property holding a [function pointer] is called like a method, what happens next depends
on whether the target function is a native Rust function or a script-defined function.

If it is a registered native Rust method function, then it is called directly.

If it is a script-defined function, the `this` variable within the function body is bound
to the [object map] before the function is called.  There is no way to simulate this behavior
via a normal function-call syntax because all scripted function arguments are passed by value.

```rust
fn do_action(x) { this.data += x; }          // 'this' binds to the object when called

let obj = #{
                data: 40,
                action: Fn("do_action")      // 'action' holds a function pointer to 'do_action'
           };

obj.action(2);                               // Calls 'do_action' with `this` bound to 'obj'

obj.call(obj.action, 2);                     // The above de-sugars to this

obj.data == 42;

// To achieve the above with normal function pointer call will fail.
fn do_action(map, x) { map.data += x; }      // 'map' is a copy

obj.action.call(obj, 2);                     // 'obj' is passed as a copy by value
```
