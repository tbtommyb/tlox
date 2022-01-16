# t-lox

t-lox is a variant of the Lox programming language from [Crafting Interpreters](https://craftinginterpreters.com ). The code in this repo is based on the `cloc` implementation in Crafting Interpreters with various additions and amendments.

## Building

To build with compilation database:

```
$ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
$ make
$ make build # or make test
$ ./build/cloc
```

## Features

In addition to Lox's feature set, t-lox has some additional features:

- built-in Array class, supporting indexing, `length()`, `pop()` and `push()`.
- Very simple file-based IO using `read` and `write` native functions.
- `switch` statements with `default` branches (no fall-through).
- `continue` statement for loops.
- postfix `++` and `--` operators.
- infix `%` operator for modulo.
- computed property getters/setters using JS-style square brackets e.g. `object["fieldName"]`.
- ternary operator `?:`.
- const variables using `const`.
- string concatenation using `+`.

## TODOs

- [x] switch statement
- [x] continue statement
- [ ] fix line numbers in error messages
- [ ] remove hardcoded array limits
- [x] challenges from calls and functions
- [x] challenges from closures
- [ ] function expressions
- [ ] ||, && etc
