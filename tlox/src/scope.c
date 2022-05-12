#include "scope.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

Scope *scope_allocate(FunctionType type) {
  Scope *scope = (Scope *)reallocate(NULL, 0, sizeof(Scope));

  scope->type = type;
  scope->enclosing = NULL;
  scope->st = st_allocate();
  scope->localCount = 0;
  scope->scopeDepth = 0;
  scope->loopOffset = -1;
  scope->currentStackDepth = 0;

  return scope;
}

void scope_init(Scope *scope, Compiler *compiler) {
  scope->enclosing = compiler->currentScope;

  compiler->currentScope = scope;
  st_init(scope->st);

  if (scope->type != TYPE_FUNCTION) {
    st_set(
        scope->st, "this", 4,
        newSymbol(syntheticToken("this"), SCOPE_LOCAL, false, false, true, 0));
  } else {
    st_set(scope->st, "", 0,
           newSymbol(syntheticToken(""), SCOPE_LOCAL, false, false, true, 0));
  }
}

bool scope_search(Scope *scope, const char *chars, int length, Symbol *symbol) {
  Scope *curr = scope;

  while (curr != NULL) {
    bool found = st_get(curr->st, chars, length, symbol);
    if (found) {
      return true;
    }
    curr = curr->enclosing;
  }

  return false;
}

bool scope_set(Scope *scope, const char *chars, int length, Symbol *symbol) {
  return st_set(scope->st, chars, length, symbol);
}
