Example Scripts
==============

{{#include ../../links.md}}

Language Feature Scripts
-----------------------

There are also a number of examples scripts that showcase Rhai's features, all in the `scripts` directory:

| Script                                                                                                   | Description                                                                   |
| -------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| [`array.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/array.rhai)                   | [Arrays]                                                                      |
| [`assignment.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/assignment.rhai)         | Variable declarations                                                         |
| [`comments.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/comments.rhai)             | Just comments                                                                 |
| [`for1.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/for1.rhai)                     | [`for`](#for-loop) loops                                                      |
| [`for2.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/for2.rhai)                     | [`for`](#for-loop) loops on [arrays]                                          |
| [`function_decl1.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/function_decl1.rhai) | A [function] without parameters                                               |
| [`function_decl2.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/function_decl2.rhai) | A [function] with two parameters                                              |
| [`function_decl3.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/function_decl3.rhai) | A [function] with many parameters                                             |
| [`if1.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/if1.rhai)                       | [`if`](#if-statement) example                                                 |
| [`loop.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/loop.rhai)                     | Count-down [`loop`](#infinite-loop) in Rhai, emulating a `do` .. `while` loop |
| [`oop.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/oop.rhai)                       | Simulate [object-oriented programming (OOP)][OOP]                             |
| [`op1.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/op1.rhai)                       | Just simple addition                                                          |
| [`op2.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/op2.rhai)                       | Simple addition and multiplication                                            |
| [`op3.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/op3.rhai)                       | Change evaluation order with parenthesis                                      |
| [`string.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/string.rhai)                 | [String] operations                                                           |
| [`strings_map.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/strings_map.rhai)       | [String] and [object map] operations                                          |
| [`while.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/while.rhai)                   | [`while`](#while-loop) loop                                                   |


Benchmark Scripts
----------------

The following scripts are for benchmarking the speed of Rhai:

| Scripts                                                                                          | Description                                                                         |
| ------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------- |
| [`speed_test.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/speed_test.rhai) | A simple program to measure the speed of Rhai's interpreter (1 million iterations). |
| [`primes.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/primes.rhai)         | Use Sieve of Eratosthenes to find all primes smaller than a limit.                  |
| [`fibonacci.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/fibonacci.rhai)   | Calculate the n-th Fibonacci number using a really dumb algorithm.                  |
| [`mat_mul.rhai`](https://github.com/jonathandturner/rhai/tree/master/scripts/mat_mul.rhai)       | Matrix multiplication test to measure the speed of multi-dimensional array access.  |


Running Example Scripts
----------------------

To run the scripts, either make a tiny program or use of the `rhai_runner` example:

```bash
cargo run --example rhai_runner scripts/any_script.rhai
```
