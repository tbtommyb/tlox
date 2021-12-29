#include "lox_array.h"
#include "vm.h"

static bool loxArrayLength(int argCount, Value *args) {
  ObjInstance *instance = AS_INSTANCE(args[0]);
  ObjString *itemsField = copyString("items", 5);

  Value value;
  if (!tableGet(&instance->fields, OBJ_VAL(itemsField), &value)) {
    runtimeError("Failed to read array length");
    return false;
  }

  int length = AS_ARRAY(value)->items.count;
  args[0] = NUMBER_VAL(length);

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

  ObjClass *klass = newClass(name);

  ObjNative *initMethod = newNative(loxArrayInit, 1);
  ObjNative *lengthMethod = newNative(loxArrayLength, 0);
  tableSet(&klass->methods, OBJ_VAL(init), OBJ_VAL(initMethod));
  tableSet(&klass->methods, OBJ_VAL(length), OBJ_VAL(lengthMethod));

  return klass;
}
