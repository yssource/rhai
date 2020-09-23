Custom Type Indexers
===================

{{#include ../links.md}}

A custom type can also expose an _indexer_ by registering an indexer function.

A custom type with an indexer function defined can use the bracket notation to get a property value:

> _object_ `[` _index_ `]`

Like getters and setters, indexers take a `&mut` reference to the first parameter.

Indexers are disabled when the [`no_index`] feature is used.

| `Engine` API                  | Description                                              |       Return Value of Function        |
| ----------------------------- | -------------------------------------------------------- | :-----------------------------------: |
| `register_indexer_get`        | Register an index getter                                 |                 _Any_                 |
| `register_indexer_set`        | Register an index setter                                 |                _None_                 |
| `register_indexer_get_set`    | Short-hand to register both an index getter and a setter |                _None_                 |
| `register_indexer_get_result` | Register an index getter                                 | `Result<Dynamic, Box<EvalAltResult>>` |
| `register_indexer_set_result` | Register an index setter                                 |   `Result<(), Box<EvalAltResult>>`    |


Cannot Override Arrays, Object Maps and Strings
----------------------------------------------

For efficiency reasons, indexers **cannot** be used to overload (i.e. override)
built-in indexing operations for [arrays], [object maps] and [strings].

Attempting to register indexers for an [array], [object map] or [string] panics.


Examples
--------

```rust
#[derive(Clone)]
struct TestStruct {
    fields: Vec<i64>
}

impl TestStruct {
    // Remember &mut must be used even for getters
    fn get_field(&mut self, index: i64) -> i64 {
        self.fields[index as usize]
    }
    fn set_field(&mut self, index: i64, value: i64) {
        self.fields[index as usize] = value
    }

    fn new() -> Self {
        TestStruct { fields: vec![1, 2, 3, 4, 5] }
    }
}

let mut engine = Engine::new();

engine
    .register_type::<TestStruct>()
    .register_fn("new_ts", TestStruct::new)
    // Short-hand: .register_indexer_get_set(TestStruct::get_field, TestStruct::set_field);
    .register_indexer_get(TestStruct::get_field)
    .register_indexer_set(TestStruct::set_field);

let result = engine.eval::<i64>("let a = new_ts(); a[2] = 42; a[2]")?;

println!("Answer: {}", result);                     // prints 42
```

**IMPORTANT: Rhai does NOT support normal references (i.e. `&T`) as parameters.**
