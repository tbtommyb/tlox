#ifndef SCOPE_H_
#define SCOPE_H_

#include "common.h"
#include "symbol_table.h"

typedef struct Scope {
  struct Scope *enclosing;
  FunctionType type;
  SymbolTable *st;
} Scope;

Scope *scope_allocate(FunctionType type);
void scope_init(Scope *scope, Compiler *compiler);
bool scope_search(Scope *scope, const char *chars, int length, Symbol *symbol);
bool scope_set(Scope *scope, const char *chars, int length, Symbol *symbol);

#endif // SCOPE_H_
