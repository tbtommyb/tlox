#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "common.h"
#include "table.h"
#include "token.h"

typedef struct SymbolTable {
  struct SymbolTable *parent;
  Table *symbols;
} SymbolTable;

SymbolTable *st_allocate();
void st_init(SymbolTable *table);
void st_free(SymbolTable *table);
bool st_get(SymbolTable *table, const char *chars, int length, Symbol **symbol);
bool st_set(SymbolTable *table, const char *chars, int length,
            const Symbol symbol);
int st_size(SymbolTable *table);

Symbol newSymbol(Token name, ScopeType type, bool isCaptured, bool isConst,
                 bool isDefined, int arity);

#endif // SYMBOL_TABLE_H_
