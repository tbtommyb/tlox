#ifndef EXECUTION_CONTEXT_H_
#define EXECUTION_CONTEXT_H_

#include "common.h"
#include "linked_list.h"

typedef struct ExecutionContext {
  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int upvalueCount;
  int scopeDepth;
  int loopOffset;
  int currentStackDepth;
  struct ExecutionContext *enclosing;
  LinkedList *definedFunctions;
} ExecutionContext;

ExecutionContext *ec_allocate();
#endif // EXECUTION_CONTEXT_H_
