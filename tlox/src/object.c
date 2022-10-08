#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "util.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;
  object->isMarked = false;
  object->next = vm.objects;
  vm.objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void *)object, size, type);
#endif

  return object;
}

ObjBoundNativeMethod *newBoundNativeMethod(Value receiver, ObjNative *method) {
  ObjBoundNativeMethod *bound =
      ALLOCATE_OBJ(ObjBoundNativeMethod, OBJ_BOUND_NATIVE_METHOD);
  bound->receiver = receiver;
  bound->method = method;
  return bound;
}

ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method) {
  ObjBoundMethod *bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
  bound->receiver = receiver;
  bound->method = method;
  return bound;
}

ObjClass *newClass(ObjString *name) {
  ObjClass *klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
  klass->name = name;
  initTable(&klass->methods);
  return klass;
}

ObjClosure *newClosure(ObjFunction *function) {
  ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue *, function->upvalueCount);
  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction *newFunction() {
  ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
  function->name = NULL;
  function->upvalueCount = 0;
  initChunk(&function->chunk);
  return function;
}

ObjInstance *newInstance(ObjClass *klass) {
  ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
  instance->klass = klass;
  initTable(&instance->fields);
  return instance;
}

ObjNative *newNative(NativeFn function, int arity) {
  ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  native->arity = arity;
  return native;
}

// FNV-1a hash function
uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

ObjString *makeString(const char *chars, int length) {
  ObjString *string =
      (ObjString *)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;
  memcpy(string->chars, chars, length);
  string->chars[length] = '\0';
  string->hash = hashString(chars, length);

  return string;
}

static ObjString *allocString(int length) {
  ObjString *string =
      (ObjString *)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;

  return string;
}

ObjString *copyString(const char *chars, int length) {
  ObjString *interned = tableFindString(&vm.strings, chars, length);
  if (interned != NULL) {
    return interned;
  }

  ObjString *string = makeString(chars, length);

  push(OBJ_VAL(string));
  tableSet(&vm.strings, OBJ_VAL(string), NIL_VAL);
  pop();

  return string;
}

ObjString *concatenateStrings(ObjString *a, ObjString *b) {
  int length = a->length + b->length;

  ObjString *result = allocString(length);
  memcpy(result->chars, a->chars, a->length);
  memcpy(result->chars + a->length, b->chars, b->length);
  result->chars[length] = '\0';

  uint32_t hash = hashString(result->chars, length);
  result->hash = hash;

  ObjString *interned = tableFindString(&vm.strings, result->chars, length);
  if (interned != NULL) {
    return interned;
  }

  push(OBJ_VAL(result));
  tableSet(&vm.strings, OBJ_VAL(result), NIL_VAL);
  pop();

  return result;
}

ObjUpvalue *newUpvalue(Value *slot) {
  ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->closed = NIL_VAL;
  upvalue->location = slot;
  upvalue->next = NULL;
  return upvalue;
}

ObjArrayInstance *newArrayInstance(ObjClass *klass) {
  ObjArrayInstance *instance =
      ALLOCATE_OBJ(ObjArrayInstance, OBJ_ARRAY_INSTANCE);
  instance->klass = klass;
  initTable(&instance->fields);
  initValueArray(&instance->elements);
  return instance;
}

static void printFunction(ObjFunction *function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
}

static char *writeFunction(ObjFunction *function) {
  if (function->name == NULL) {
    return "<script>";
  }
  return writeString("<fn %s>", function->name->chars);
}

char *writeObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_FUNCTION:
    return writeFunction(AS_FUNCTION(value));
  case OBJ_NATIVE:
    return "<native fn>";
  case OBJ_STRING:
    return writeString("%s", AS_CSTRING(value));
  case OBJ_CLOSURE:
    return writeFunction(AS_CLOSURE(value)->function);
  case OBJ_CLASS:
    return writeString("%s", AS_CLASS(value)->name->chars);
  case OBJ_INSTANCE:
    return writeString("%s instance", AS_INSTANCE(value)->klass->name->chars);
  case OBJ_BOUND_METHOD:
    return writeFunction(AS_BOUND_METHOD(value)->method->function);
  case OBJ_BOUND_NATIVE_METHOD:
    return "<native method>";
  case OBJ_UPVALUE:
    return "<upvalue>";
  case OBJ_ARRAY_INSTANCE:
    return writeString("array len %d",
                       AS_ARRAY_INSTANCE(value)->elements.count);
  }
}

void printObject(FILE *stream, Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_FUNCTION:
    printFunction(AS_FUNCTION(value));
    break;
  case OBJ_NATIVE:
    fprintf(stream, "<native fn>");
    break;
  case OBJ_STRING:
    fprintf(stream, "%s", AS_CSTRING(value));
    break;
  case OBJ_CLOSURE:
    printFunction(AS_CLOSURE(value)->function);
    break;
  case OBJ_CLASS:
    fprintf(stream, "%s", AS_CLASS(value)->name->chars);
    break;
  case OBJ_INSTANCE:
    fprintf(stream, "%s instance", AS_INSTANCE(value)->klass->name->chars);
    break;
  case OBJ_BOUND_METHOD:
    printFunction(AS_BOUND_METHOD(value)->method->function);
    break;
  case OBJ_BOUND_NATIVE_METHOD:
    fprintf(stream, "<native method>");
    break;
  case OBJ_UPVALUE:
    fprintf(stream, "upvalue");
    break;
  case OBJ_ARRAY_INSTANCE:
    fprintf(stream, "array len %d", AS_ARRAY_INSTANCE(value)->elements.count);
    break;
  }
}
