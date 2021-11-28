# cloc

To build with compilation database:

```
$ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
$ make
$ make build # or make test
$ ./build/cloc
```
