#include "lox_array.h"
#include "memory.h"
#include "vm.h"

static bool loxArrayLength(int argCount, Value *args) {
  ObjArrayInstance *instance = AS_ARRAY_INSTANCE(args[0]);
  int length = instance->elements.count;
  args[0] = NUMBER_VAL(length);

  return true;
}

static bool loxArrayPush(int argCount, Value *args) {
  ObjArrayInstance *instance = AS_ARRAY_INSTANCE(args[0]);

  ValueArray *arr = &instance->elements;
  if (arr->capacity < arr->count + argCount) {
    int oldCapacity = arr->capacity;
    arr->capacity = GROW_CAPACITY(oldCapacity + argCount);
    arr->values = GROW_ARRAY(Value, arr->values, oldCapacity, arr->capacity);
  }

  for (int i = 0; i < argCount; i++) {
    arr->values[instance->elements.count++] = args[1 + i];
  }

  return true;
}

static bool loxArrayPop(int argCount, Value *args) {
  if (argCount != 0) {
    runtimeError("Array.pop takes no arguments");
  }

  ObjArrayInstance *instance = AS_ARRAY_INSTANCE(args[0]);
  ValueArray *arr = &instance->elements;

  if (arr->count == 0) {
    args[0] = NIL_VAL;
  } else {
    args[0] = arr->values[arr->count - 1];
    arr->count--;
  }

  return true;
}

ObjClass *createArrayClass(ObjString *init) {
  ObjString *name = copyString("Array", 5);
  ObjString *length = copyString("length", 6);
  ObjString *push = copyString("push", 4);
  ObjString *pop = copyString("pop", 3);

  ObjClass *klass = newClass(name);

  ObjNative *lengthMethod = newNative(loxArrayLength, 0);
  ObjNative *pushMethod = newNative(loxArrayPush, 1);
  ObjNative *popMethod = newNative(loxArrayPop, 0);
  tableSet(&klass->methods, OBJ_VAL(length), OBJ_VAL(lengthMethod));
  tableSet(&klass->methods, OBJ_VAL(push), OBJ_VAL(pushMethod));
  tableSet(&klass->methods, OBJ_VAL(pop), OBJ_VAL(popMethod));

  return klass;
}
