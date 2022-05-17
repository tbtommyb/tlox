#ifndef EXECUTION_CONTEXT_H_
#define EXECUTION_CONTEXT_H_

#include "common.h"

typedef struct ExecutionContext {
  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;
  int loopOffset;
  int currentStackDepth;
} ExecutionContext;

#endif // EXECUTION_CONTEXT_H_
