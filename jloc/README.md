# jloc

A simple interpreter based on [Crafting Interpreters](https://craftinginterpreters.com).

## Minor improvements / challenges

- C-style block quotes (not nested).
- Support for C-style ternary operator (right-associative).
- Support for C-style comma operator.
- Function expressions (lambdas).
- `break` statements.
- static classes.
- getter methods.


## Build instructions

```
$ mvn verify
```

## TODOs

- [x] make `print` a native function.
- [ ] improve static analysis e.g. `break`.
- [ ] add a basic type system.
