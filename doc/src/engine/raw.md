
Raw `Engine`
===========

{{#include ../links.md}}

`Engine::new` creates a scripting [`Engine`] with common functionalities (e.g. printing to the console via `print`).
In many controlled embedded environments, however, these are not needed.

Use `Engine::new_raw` to create a _raw_ `Engine`, in which only a minimal set of basic arithmetic and logical operators
are supported.

Built-in Operators
------------------

| Operators                | Assignment operators         | Supported for type (see [standard types])                                     |
| ------------------------ | ---------------------------- | ----------------------------------------------------------------------------- |
| `+`,                     | `+=`                         | `INT`, `FLOAT` (if not [`no_float`]), `ImmutableString`                       |
| `-`, `*`, `/`, `%`, `~`, | `-=`, `*=`, `/=`, `%=`, `~=` | `INT`, `FLOAT` (if not [`no_float`])                                          |
| `<<`, `>>`, `^`,         | `<<=`, `>>=`, `^=`           | `INT`                                                                         |
| `&`, `\|`,               | `&=`, `\|=`                  | `INT`, `bool`                                                                 |
| `&&`, `\|\|`             |                              | `bool`                                                                        |
| `==`, `!=`               |                              | `INT`, `FLOAT` (if not [`no_float`]), `bool`, `char`, `()`, `ImmutableString` |
| `>`, `>=`, `<`, `<=`     |                              | `INT`, `FLOAT` (if not [`no_float`]), `char`, `()`, `ImmutableString`         |
