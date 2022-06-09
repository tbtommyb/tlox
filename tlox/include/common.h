#ifndef clox_common_h
#define clox_common_h

#include "token.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEBUG_TRACE_EXECUTION
#define DEBUG_PRINT_CODE
#define DEBUG_LOG_GC
#undef DEBUG_LOG_GC
#define DEBUG_STRESS_GC
#undef DEBUG_STRESS_GC
#define UINT8_COUNT (UINT8_MAX + 1)
#define NAN_BOXING
#undef NAN_BOXING

typedef struct Parser Parser;
typedef struct Compiler Compiler;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
  TYPE_METHOD,
  TYPE_INITIALIZER,
  TYPE_CLASS
} FunctionType;

typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

#endif
