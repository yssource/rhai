Printing for Custom Types
========================

{{#include ../links.md}}

To use custom types for [`print`] and [`debug`], or convert its value into a [string], it is necessary that the following
functions be registered (assuming the custom type is `T : Display + Debug`):

| Function    | Signature                                                     | Typical implementation                | Usage                                                                                   |
| ----------- | ------------------------------------------------------------- | ------------------------------------- | --------------------------------------------------------------------------------------- |
| `to_string` | <code>\|s: &mut T\| -> ImmutableString</code>                 | `s.to_string().into()`                | Converts the custom type into a [string]                                                |
| `print`     | <code>\|s: &mut T\| -> ImmutableString</code>                 | `s.to_string().into()`                | Converts the custom type into a [string] for the [`print`](#print-and-debug) statement  |
| `debug`     | <code>\|s: &mut T\| -> ImmutableString</code>                 | `format!("{:?}", s).into()`           | Converts the custom type into a [string] for the [`debug`](#print-and-debug) statement  |
| `+`         | <code>\|s1: ImmutableString, s: T\| -> ImmutableString</code> | `s1 + s`                              | Append the custom type to another [string], for `print("Answer: " + type);` usage       |
| `+`         | <code>\|s: T, s2: ImmutableString\| -> ImmutableString</code> | `s.to_string().push_str(&s2).into();` | Append another [string] to the custom type, for `print(type + " is the answer");` usage |
| `+=`        | <code>\|s1: &mut ImmutableString, s: T\|</code>               | `s1 += s.to_string()`                 | Append the custom type to an existing [string], for `s += type;` usage                  |
