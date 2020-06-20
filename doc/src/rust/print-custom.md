Printing for Custom Types
========================

{{#include ../links.md}}

To use custom types for [`print`] and [`debug`], or convert its value into a [string], it is necessary that the following
functions be registered (assuming the custom type is `T : Display + Debug`):

| Function    | Signature                                        | Typical implementation                | Usage                                                                                   |
| ----------- | ------------------------------------------------ | ------------------------------------- | --------------------------------------------------------------------------------------- |
| `to_string` | `|s: &mut T| -> ImmutableString`                 | `s.to_string().into()`                | Converts the custom type into a [string]                                                |
| `print`     | `|s: &mut T| -> ImmutableString`                 | `s.to_string().into()`                | Converts the custom type into a [string] for the [`print`](#print-and-debug) statement  |
| `debug`     | `|s: &mut T| -> ImmutableString`                 | `format!("{:?}", s).into()`           | Converts the custom type into a [string] for the [`debug`](#print-and-debug) statement  |
| `+`         | `|s1: ImmutableString, s: T| -> ImmutableString` | `s1 + s`                              | Append the custom type to another [string], for `print("Answer: " + type);` usage       |
| `+`         | `|s: T, s2: ImmutableString| -> ImmutableString` | `s.to_string().push_str(&s2).into();` | Append another [string] to the custom type, for `print(type + " is the answer");` usage |
| `+=`        | `|s1: &mut ImmutableString, s: T|`               | `s1 += s.to_string()`                 | Append the custom type to an existing [string], for `s += type;` usage                  |
