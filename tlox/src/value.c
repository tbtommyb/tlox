#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    return AS_NUMBER(a) == AS_NUMBER(b);
  }
  return a == b;
#else
  if (a.type != b.type)
    return false;

  switch (a.type) {
  case VAL_BOOL:
    return AS_BOOL(a) == AS_BOOL(b);
  case VAL_NIL:
    return true;
  case VAL_NUMBER:
    return AS_NUMBER(a) == AS_NUMBER(b);
  case VAL_OBJ:
    return AS_OBJ(a) == AS_OBJ(b);
  case VAL_EMPTY:
    return true;
  case VAL_SYMBOL: {
    Symbol symA = AS_SYMBOL(a);
    Symbol symB = AS_SYMBOL(b);
    return memcmp(&symA, &symB, sizeof symA);
  }
  case VAL_POINTER:
    return AS_POINTER(a) == AS_POINTER(b);
  default:
    return false; // Unreachable.
  }
#endif
}

void initValueArray(ValueArray *array) {
  array->values = NULL;
  array->capacity = 0;
  array->count = 0;
}

void writeValueArray(ValueArray *array, Value value) {
  if (array->capacity < array->count + 1) {
    int oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->values =
        GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
  }

  array->values[array->count] = value;
  array->count++;
}

void freeValueArray(ValueArray *array) {
  FREE_ARRAY(Value, array->values, array->capacity);
  initValueArray(array);
}

void printValue(FILE *stream, Value value) {
#ifdef NAN_BOXING
  if (IS_BOOL(value)) {
    fprintf(stream, AS_BOOL(value) ? "true" : "false");
  } else if (IS_NIL(value)) {
    fprintf(stream, "nil");
  } else if (IS_NUMBER(value)) {
    fprintf(stream, "%g", AS_NUMBER(value));
  } else if (IS_OBJ(value)) {
    printObject(stream, value);
  }
#else
  switch (value.type) {
  case VAL_BOOL:
    fprintf(stream, AS_BOOL(value) ? "true" : "false");
    break;
  case VAL_NIL:
    fprintf(stream, "nil");
    break;
  case VAL_NUMBER:
    fprintf(stream, "%g", AS_NUMBER(value));
    break;
  case VAL_OBJ:
    printObject(stream, value);
    break;
  case VAL_EMPTY:
    fprintf(stream, "<empty>");
    break;
  case VAL_POINTER:
    fprintf(stream, "%p", AS_POINTER(value));
    break;
  case VAL_SYMBOL:
    fprintf(stream, "<symbol> %.*s", AS_SYMBOL(value).name.length,
            AS_SYMBOL(value).name.start);
    break;
  }
#endif
}

static uint32_t hashDouble(double value) {
  union BitCast {
    double value;
    uint32_t ints[2];
  };

  union BitCast cast;
  cast.value = (value) + 1.0;
  return cast.ints[0] + cast.ints[1];
}

static uint32_t hashSymbol(Symbol *symbol) {
  uint32_t hash = 0;
  unsigned short c;
  unsigned short *reinterpret_symbol = (unsigned short *)symbol;
  size_t size_ = sizeof(Symbol);
  size_t elem_size_ = sizeof(unsigned short);
  int len = (int)size_ / elem_size_;

  for (int i = 0; i < len; i++) {
    c = reinterpret_symbol[i];
    hash += c;
  }
  return hash;
}

uint32_t computeHash(Value value) {
  switch (value.type) {
  case VAL_BOOL:
    return AS_BOOL(value) ? 3 : 5;
  case VAL_NIL:
    return 7;
  case VAL_NUMBER:
    return hashDouble(AS_NUMBER(value));
  case VAL_OBJ:
    return AS_STRING(value)->hash;
  case VAL_EMPTY:
    return 0;
  case VAL_POINTER:
    return *(double *)AS_POINTER(value);
  case VAL_SYMBOL:
    return hashSymbol(&AS_SYMBOL(value));
  }
}
