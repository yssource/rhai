Catch Exceptions
================

{{#include ../links.md}}


When an [exception] is thrown via a `throw` statement, evaluation of the script halts
and the [`Engine`] returns with `Err(Box<EvalAltResult::ErrorRuntime>)` containing the
exception value that has been thrown.

It is possible, via the `try` ... `catch` statement, to _catch_ exceptions.

```rust
// Catch an exception and capturing its value
try
{
    throw 42;
}
catch (err)         // 'err' captures the thrown exception value
{
    print(err);     // prints 42
}

// Catch an exception without capturing its value
try
{
    print(42/0);    // deliberate divide-by-zero exception
}
catch               // no catch variable - exception value is discarded
{
    print("Ouch!");
}

// Exception in the 'catch' block
try
{
    print(42/0);    // throw divide-by-zero exception
}
catch
{
    print("You seem to be dividing by zero here...");

    throw "die";    // a 'throw' statement inside a 'catch' block
                    // throws a new exception
}
```


Re-Throw Exception
------------------

Like the `try` ... `catch` syntax in most languages, it is possible to _re-throw_
an exception within the `catch` block simply by another `throw` statement without
a value.


```rust
try
{
    // Call something that will throw an exception...
    do_something_bad_that_throws();
}
catch
{
    print("Oooh! You've done something real bad!");

    throw;          // 'throw' without a value within a 'catch' block
                    // re-throws the original exception
}

```


Catchable Exceptions
--------------------

Many script-oriented exceptions can be caught via `try` ... `catch`:

* Runtime error thrown by a `throw` statement
* Arithmetic error
* Variable not found
* [Function] not found
* [Module] not found
* Unbound [`this`]
* Data type mismatch
* [Array]/[string] indexing out-of-bounds
* Indexing with an inappropriate type
* `for` statement without an iterator
* Error in an `in` expression
* Data race detected
* Assignment to a calculated value/constant value
* Dot expression error


Non-Catchable Exceptions
------------------------

Some exceptions _cannot_ be caught:

* Syntax error during parsing
* System error - e.g. script file not found
* Script evaluation over [limits]({{rootUrl}}/safety/index.md)
* [Stack overflow][maximum call stack depth]
* Script evaluation manually terminated
