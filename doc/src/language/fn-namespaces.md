Function Namespaces
==================

{{#include ../links.md}}

Each Function is a Separate Compilation Unit
-------------------------------------------

[Functions] in Rhai are _pure_ and they form individual _compilation units_.
This means that individual functions can be separated, exported, re-grouped, imported,
and generally mix-'n-match-ed with other completely unrelated scripts.

For example, the `AST::merge` method allows Global all functions in one [`AST`] into another,
forming a new, combined, group of functions.

In general, there are two types of _namespaces_ where functions are looked up:

| Namespace | Source                                                                 | Lookup method                     |         How Many         |
| --------- | ---------------------------------------------------------------------- | --------------------------------- | :----------------------: |
| Global    | `Engine::register_XXX` API, [`AST`] being evaluated, [packages] loaded | simple function name              |           one            |
| Module    | [`Module`]                                                             | namespace-qualified function name | as many as [`import`]-ed |


Global Namespace
----------------

There is one _global_ namespace for every [`Engine`], which includes:

* All the native Rust functions registered via the `Engine::register_XXX` API.

* All the Rust functions defined in [packages] that are loaded into the [`Engine`].

In addition, during evaluation of an [`AST`], all script-defined functions bundled together within
the [`AST`] are added to the global namespace and override any existing registered functions of
the same names and number of parameters.

Anywhere in a Rhai script, when a function call is made, it is searched within the global namespace.
Therefore, function calls in Rhai are _late_ bound - meaning that the function called cannot be
determined or guaranteed and there is no way to _lock down_ the function being called.
This aspect is very similar to JavaScript before ES6 modules.

```rust
// Compile a script into AST
let ast1 = engine.compile(
    r#"
        fn get_message() { "Hello!" }   // greeting message

        fn say_hello() {
            print(get_message());       // prints message
        }

        say_hello();
    "#
)?;

// Compile another script with an overriding function
let ast2 = engine.compile(r#"fn get_message() { "Boo!" }"#)?;

// Merge the two AST's
let ast = ast1.merge(ast2);         // 'message' will be overwritten

engine.consume_ast(&ast)?;          // prints 'Boo!'
```

Therefore, care must be taken when _cross-calling_ functions to make sure that the correct
functions are called.

The only practical way to ensure that a function is a correct one is to use [modules] -
i.e. define the function in a separate module and then [`import`] it:

```rust
----------------
| message.rhai |
----------------

fn get_message() { "Hello!" }


---------------
| script.rhai |
---------------

fn say_hello() {
    import "message" as msg;
    print(msg::get_message());
}
say_hello();
```


Namespace Consideration When Not Using `FileModuleResolver`
---------------------------------------------------------

[Modules] can be dynamically loaded into a Rhai script using the [`import`] keyword.
When that happens, functions defined within the [module] can be called with a _qualified_ name.

The [`FileModuleResolver`][module resolver] encapsulates the namespace inside the module itself,
so everything works as expected.  A function defined in the module script cannot access functions
defined in the calling script, but it can freely call functions defined within the same module script.

There is a catch, though.  When using anything other than the [`FileModuleResolver`][module resolver],
functions in a module script refer to functions defined in the _global namespace_.
When called later, those functions may not be found.

When using the [`GlobalFileModuleResolver`][module resolver], for example:

```rust
Using GlobalFileModuleResolver
==============================

-----------------
| greeting.rhai |
-----------------

fn get_message() { "Hello!" };

fn say_hello() {
    print(get_message());           // 'get_message' is looked up in the global namespace
                                    // when exported
}

say_hello();                        // Here, 'get_message' is found in the module namespace

---------------
| script.rhai |
---------------

import "greeting" as g;
g::say_hello();                     // <- error: function not found - 'get_message'
                                    //    because it does not exist in the global namespace
```

In the example above, although the module `greeting.rhai` loads fine (`"Hello!"` is printed),
the subsequent call using the _namespace-qualified_ function name fails to find the same function
'`message`' which now essentially becomes `g::message`.  The call fails as there is no more
function named '`message`' in the global namespace.

Therefore, when writing functions for a [module] intended for the [`GlobalFileModuleResolver`][module resolver],
make sure that those functions are as _pure_ as possible and avoid cross-calling them from each other.

A [function pointer] is a valid technique to call another function in an environment-independent manner:

```rust
-----------------
| greeting.rhai |
-----------------

fn get_message() { "Hello!" };

fn say_hello(msg_func) {            // 'msg_func' is a function pointer
    print(msg_func.call());         // call via the function pointer
}

say_hello(Fn("get_message"));


---------------
| script.rhai |
---------------

import "greeting" as g;

fn my_msg() {
    import "greeting" as g;         // <- must import again here...
    g::get_message()                // <- ... otherwise will not find module 'g'
}

g::say_hello(Fn("my_msg"));         // prints 'Hello!'
```
