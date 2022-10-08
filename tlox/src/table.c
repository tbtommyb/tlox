#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

Table *allocateTable() { return (Table *)reallocate(NULL, 0, sizeof(Table)); }

void initTable(Table *table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table *table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

static Entry *findEntry(Entry *entries, int capacity, Value key) {
  uint32_t index = computeHash(key) & (capacity - 1);
  Entry *tombstone = NULL;

  for (;;) {
    Entry *entry = &entries[index];
    if (IS_EMPTY(entry->key)) {
      if (IS_NIL(entry->value)) {
        // Empty entry.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone.
        if (tombstone == NULL) {
          tombstone = entry;
        }
      }
    } else if (valuesEqual(entry->key, key)) {
      // We found the key.
      return entry;
    }
    index = (index + 1) & (capacity - 1);
  }
}

static void adjustCapacity(Table *table, int capacity) {
  Entry *entries = ALLOCATE(Entry, capacity);

  for (int i = 0; i < capacity; i++) {
    entries[i].key = EMPTY_VAL;
    entries[i].value = NIL_VAL;
  }

  table->count = 0;

  for (int i = 0; i < table->capacity; i++) {
    Entry *entry = &table->entries[i];
    if (IS_EMPTY(entry->key)) {
      continue;
    }

    Entry *dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableGet(Table *table, Value key, Value *value) {
  if (table->count == 0)
    return false;

  Entry *entry = findEntry(table->entries, table->capacity, key);
  if (IS_EMPTY(entry->key)) {
    return false;
  }

  *value = entry->value;
  return true;
}

bool tableGetPointer(Table *table, Value key, Value **value) {
  if (table->count == 0)
    return false;

  Entry *entry = findEntry(table->entries, table->capacity, key);
  if (IS_EMPTY(entry->key)) {
    return false;
  }

  *value = &entry->value;
  return true;
}

bool tableSet(Table *table, Value key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }
  Entry *entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = IS_EMPTY(entry->key);
  if (isNewKey && IS_NIL(entry->value)) {
    table->count++;
  }

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

bool tableDelete(Table *table, Value key) {
  if (table->count == 0)
    return false;

  // Find the entry.
  Entry *entry = findEntry(table->entries, table->capacity, key);
  if (IS_EMPTY(entry->key)) {
    return false;
  }

  // Place a tombstone in the entry.
  entry->key = EMPTY_VAL;
  entry->value = BOOL_VAL(true);
  return true;
}

void tableAddAll(Table *from, Table *to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry *entry = &from->entries[i];
    if (!IS_EMPTY(entry->key)) {
      tableSet(to, entry->key, entry->value);
    }
  }
}

int tableSize(Table *table) { return table->count; }

ObjString *tableFindString(Table *table, const char *chars, int length) {
  if (table->count == 0) {
    return NULL;
  }

  uint32_t hash = hashString(chars, length);
  uint32_t index = hash & (table->capacity - 1);
  for (;;) {
    Entry *entry = &table->entries[index];
    if (IS_EMPTY(entry->key)) {
      return NULL;
    }

    ObjString *string = AS_STRING(entry->key);
    if (string->length == length && memcmp(string->chars, chars, length) == 0) {
      return string;
    }

    index = (index + 1) & (table->capacity - 1);
  }
}

void tableRemoveWhite(Table *table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry *entry = &table->entries[i];
    if (!IS_EMPTY(entry->key) && !AS_OBJ(entry->key)->isMarked) {
      tableDelete(table, entry->key);
    }
  }
}

void markTable(Table *table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry *entry = &table->entries[i];
    markObject(AS_OBJ(entry->key));
    markValue(entry->value);
  }
}
