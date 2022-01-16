# tlox

`tlox` is a work-in-progress optimising compiler based on the bytecode interpreter in [Crafting Interpreters](https://craftinginterpreters.com).

The language implementation has a few minor changes to the Lox language in Crafting Interpreters and I plan to implement bigger changes, so I am renaming it to `tlox`.

`jloc` just contains the original tree-walking interpreter as implemented in the book, with minor additions such as tenary operator (`?:`).

## Planned work

- [ ] implement AST in C and construct control-flow graph (*in progress*)
- [ ] implement basic type system
- [ ] implement local analysis and optimisations

