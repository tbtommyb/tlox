#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
  Value key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry *entries;
} Table;

Table *allocateTable();
void initTable(Table *table);
void freeTable(Table *table);
bool tableGet(Table *table, Value key, Value *value);
bool tableGetPointer(Table *table, Value key, Value **value);
bool tableSet(Table *table, Value key, Value value);
bool tableDelete(Table *table, Value key);
void tableAddAll(Table *from, Table *to);
int tableSize(Table *table);
ObjString *tableFindString(Table *table, const char *chars, int length);
void tableRemoveWhite(Table *table);
void markTable(Table *table);
#endif
