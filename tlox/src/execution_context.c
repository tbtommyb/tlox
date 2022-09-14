#include "execution_context.h"
#include "linked_list.h"
#include "memory.h"

ExecutionContext *ec_allocate() {
  ExecutionContext *ec =
      (ExecutionContext *)reallocate(NULL, 0, sizeof(ExecutionContext));
  ec->localCount = 0;
  ec->upvalueCount = 0;
  ec->scopeDepth = 0;
  ec->loopOffset = -1;
  ec->enclosing = NULL;
  ec->currentStackDepth = 0;
  ec->definedFunctions = linkedList_allocate();

  return ec;
}
