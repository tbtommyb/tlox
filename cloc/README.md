# cloc

To build with compilation database:

```
$ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
$ make
$ make build # or make test
$ ./build/cloc
```

## TODOs

- [x] switch statement
- [x] continue statement
- [ ] fix line numbers in error messages
- [ ] remove hardcoded array limits
- [ ] challenges from calls and functions
- [ ] challenges from closures
- [ ] function expressions
