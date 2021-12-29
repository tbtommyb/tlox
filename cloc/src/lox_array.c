#include "lox_array.h"
#include "memory.h"
#include "vm.h"

static bool loxArrayLength(int argCount, Value *args) {
  ObjInstance *instance = AS_INSTANCE(args[0]);
  ObjString *itemsField = copyString("items", 5);

  Value value;
  if (!tableGet(&instance->fields, OBJ_VAL(itemsField), &value)) {
    runtimeError("Failed to get array items");
    return false;
  }

  int length = AS_ARRAY(value)->items.count;
  args[0] = NUMBER_VAL(length);

  return true;
}

static bool loxArrayPush(int argCount, Value *args) {
  ObjInstance *instance = AS_INSTANCE(args[0]);

  ObjString *itemsField = copyString("items", 5);

  Value value;
  if (!tableGet(&instance->fields, OBJ_VAL(itemsField), &value)) {
    runtimeError("Failed to get array items");
    return false;
  }

  ObjArray *arr = AS_ARRAY(value);

  if (arr->items.capacity < arr->items.count + argCount) {
    int oldCapacity = arr->items.capacity;
    arr->items.capacity = GROW_CAPACITY(oldCapacity + argCount);
    arr->items.values =
        GROW_ARRAY(Value, &arr->items.values, oldCapacity, arr->items.capacity);
  }

  for (int i = 0; i < argCount; i++) {
    arr->items.values[arr->items.count++] = args[1 + i];
  }

  return true;
}

static bool loxArrayPop(int argCount, Value *args) {
  if (argCount != 0) {
    runtimeError("Array.pop takes no arguments");
  }

  ObjInstance *instance = AS_INSTANCE(args[0]);
  ObjString *itemsField = copyString("items", 5);

  Value value;
  if (!tableGet(&instance->fields, OBJ_VAL(itemsField), &value)) {
    runtimeError("Failed to get array items");
    return false;
  }

  ObjArray *arr = AS_ARRAY(value);

  if (arr->items.count == 0) {
    args[0] = NIL_VAL;
  } else {
    args[0] = arr->items.values[arr->items.count - 1];
    arr->items.count--;
  }

  return true;
}

static bool loxArrayInit(int argCount, Value *args) {
  ObjString *itemsField = copyString("items", 5);
  ObjInstance *instance = AS_INSTANCE(args[0]);

  tableSet(&instance->fields, OBJ_VAL(itemsField), args[1]);

  return true;
}

ObjClass *createArrayClass(ObjString *init) {
  ObjString *name = copyString("Array", 5);
  ObjString *length = copyString("length", 6);
  ObjString *push = copyString("push", 4);
  ObjString *pop = copyString("pop", 3);

  ObjClass *klass = newClass(name);

  ObjNative *initMethod = newNative(loxArrayInit, 1);
  ObjNative *lengthMethod = newNative(loxArrayLength, 0);
  ObjNative *pushMethod = newNative(loxArrayPush, 1);
  ObjNative *popMethod = newNative(loxArrayPop, 0);
  tableSet(&klass->methods, OBJ_VAL(init), OBJ_VAL(initMethod));
  tableSet(&klass->methods, OBJ_VAL(length), OBJ_VAL(lengthMethod));
  tableSet(&klass->methods, OBJ_VAL(push), OBJ_VAL(pushMethod));
  tableSet(&klass->methods, OBJ_VAL(pop), OBJ_VAL(popMethod));

  return klass;
}
