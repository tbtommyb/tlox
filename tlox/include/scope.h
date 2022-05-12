#ifndef SCOPE_H_
#define SCOPE_H_

#include "common.h"
#include "symbol_table.h"

// FIXME: for now this is a mix of compile time and run time data
typedef struct Scope {
  struct Scope *enclosing;
  FunctionType type;

  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;

  int loopOffset;
  int currentStackDepth;

  // FIXME: store symbol table elsewhere. CompilerState?
  SymbolTable *st;
} Scope;

Scope *scope_allocate(FunctionType type);
void scope_init(Scope *scope, Compiler *compiler);
bool scope_search(Scope *scope, const char *chars, int length, Symbol *symbol);
bool scope_set(Scope *scope, const char *chars, int length, Symbol *symbol);

#endif // SCOPE_H_
