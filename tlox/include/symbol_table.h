#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "table.h"
#include "token.h"

typedef enum { SCOPE_LOCAL, SCOPE_GLOBAL, SCOPE_FUNCTION_PARAM } ScopeType;

typedef struct Symbol {
  Token name;
  bool isCaptured;
  bool isConst;
  bool isDefined;
  ScopeType type;
} Symbol;

typedef struct SymbolTable {
  struct SymbolTable *parent;
  Table *symbols;
} SymbolTable;

SymbolTable *st_allocate();
void st_init(SymbolTable *table, SymbolTable *parent);
void st_free(SymbolTable *table);
bool st_get(SymbolTable *table, const char *chars, int length, Symbol *symbol);
bool st_set(SymbolTable *table, const char *chars, int length, Symbol *symbol);
int st_size(SymbolTable *table);
bool st_search(SymbolTable *table, const char *chars, int length,
               Symbol *symbol);

Symbol *newSymbol(Token name, ScopeType type, bool isCaptured, bool isConst,
                  bool isDefined);

#endif // SYMBOL_TABLE_H_
