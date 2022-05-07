#include "symbol_table.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

SymbolTable *st_allocate() {
  SymbolTable *st = (SymbolTable *)reallocate(NULL, 0, sizeof(SymbolTable));
  st->symbols = allocateTable();
  st->parent = NULL;
  return st;
}

void st_init(SymbolTable *table, SymbolTable *parent) {
  initTable(table->symbols);
  table->parent = parent;
}

void st_free(SymbolTable *table) { freeTable(table->symbols); }

bool st_get(SymbolTable *table, const char *chars, int length, Symbol *symbol) {
  Value foundSymbol;
  if (!tableGet(table->symbols, OBJ_VAL(copyString(chars, length)),
                &foundSymbol)) {
    return false;
  }
  *symbol = *(Symbol *)AS_POINTER(foundSymbol);
  return true;
}

bool st_set(SymbolTable *table, const char *chars, int length, Symbol *symbol) {
  return tableSet(table->symbols, OBJ_VAL(copyString(chars, length)),
                  POINTER_VAL(symbol));
}

int st_size(SymbolTable *table) { return tableSize(table->symbols); }

bool st_search(SymbolTable *table, const char *chars, int length,
               Symbol *symbol) {
  SymbolTable *curr = table;

  while (curr != NULL) {
    bool found = st_get(curr, chars, length, symbol);
    if (found) {
      return true;
    }
    curr = curr->parent;
  }

  return false;
}

Symbol *newSymbol(Token name, ScopeType type, bool isCaptured, bool isConst,
                  bool isDefined) {
  Symbol *symbol = (Symbol *)reallocate(NULL, 0, sizeof(Symbol));
  symbol->isCaptured = isCaptured;
  symbol->isConst = isConst;
  symbol->isDefined = isDefined;
  symbol->name = name;
  symbol->type = type;
  return symbol;
}
