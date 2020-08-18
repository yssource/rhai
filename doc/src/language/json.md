Parse an Object Map from JSON
============================

{{#include ../links.md}}

The syntax for an [object map] is extremely similar to JSON, with the exception of `null` values which can
technically be mapped to [`()`].  A valid JSON string does not start with a hash character `#` while a
Rhai [object map] does - that's the major difference!

Use the `Engine::parse_json` method to parse a piece of _simple_ JSON into an object map:

```rust
// JSON string - notice that JSON property names are always quoted
//               notice also that comments are acceptable within the JSON string
let json = r#"{
                "a": 1,                 // <- this is an integer number
                "b": true,
                "c": 123.0,             // <- this is a floating-point number
                "$d e f!": "hello",     // <- any text can be a property name
                "^^^!!!": [1,42,"999"], // <- value can be array or another hash
                "z": null               // <- JSON 'null' value
              }
"#;

// Parse the JSON expression as an object map
// Set the second boolean parameter to true in order to map 'null' to '()'
let map = engine.parse_json(json, true)?;

map.len() == 6;       // 'map' contains all properties in the JSON string

// Put the object map into a 'Scope'
let mut scope = Scope::new();
scope.push("map", map);

let result = engine.eval_with_scope::<INT>(r#"map["^^^!!!"].len()"#)?;

result == 3;          // the object map is successfully used in the script
```

Representation of Numbers
------------------------

JSON numbers are all floating-point while Rhai supports integers (`INT`) and floating-point (`FLOAT`) if
the [`no_float`] feature is not used.  Most common generators of JSON data distinguish between
integer and floating-point values by always serializing a floating-point number with a decimal point
(i.e. `123.0` instead of `123` which is assumed to be an integer).  This style can be used successfully
with Rhai [object maps].


Parse JSON with Sub-Objects
--------------------------

`Engine::parse_json` depends on the fact that the [object map] literal syntax in Rhai is _almost_
the same as a JSON object.  However, it is _almost_ because the syntax for a sub-object in JSON
(i.e. "`{ ... }`") is different from a Rhai [object map] literal (i.e. "`#{ ... }`").

When `Engine::parse_json` encounters JSON with sub-objects, it fails with a syntax error.

If it is certain that no text string in the JSON will ever contain the character '`{`',
then it is possible to parse it by first replacing all occupance of '`{`' with "`#{`".

```rust
// JSON with sub-object 'b'.
let json = r#"{"a":1, "b":{"x":true, "y":false}}"#;

let new_json = json.replace("{" "#{");

// The leading '{' will also be replaced to '#{', but parse_json can handle this.
let map = engine.parse_json(&new_json, false)?;

map.len() == 2;       // 'map' contains two properties: 'a' and 'b'
```
