Special Support for OOP via Object Maps
======================================

{{#include ../links.md}}

[Object maps] can be used to simulate object-oriented programming ([OOP]) by storing data
as properties and methods as properties holding [function pointers].

If an [object map]'s property holding a [function pointer] is called in method-call style,
it calls the function referenced by the [function pointer].

```rust
fn do_action(x) { print(this + x); }        // 'this' binds to the object when called

let obj = #{
                data: 40,
                action: Fn("do_action")     // 'action' holds a function pointer to 'do_action'
           };

obj.action(2);                              // prints 42

obj.action.call(2);                         // <- the above de-sugars to this
```
