#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#include <stdio.h>

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_BOUND_NATIVE_METHOD(value) isObjType(value, OBJ_BOUND_NATIVE_METHOD)
#define IS_ARRAY_INSTANCE(value) isObjType(value, OBJ_ARRAY_INSTANCE)

#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value)))
#define AS_CLASS(value) ((ObjClass *)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance *)AS_OBJ(value))
#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_BOUND_NATIVE_METHOD(value) ((ObjBoundNativeMethod *)AS_OBJ(value))
#define AS_ARRAY_INSTANCE(value) ((ObjArrayInstance *)AS_OBJ(value))

typedef enum {
  OBJ_FUNCTION,
  OBJ_STRING,
  OBJ_NATIVE,
  OBJ_CLOSURE,
  OBJ_UPVALUE,
  OBJ_CLASS,
  OBJ_INSTANCE,
  OBJ_BOUND_METHOD,
  OBJ_BOUND_NATIVE_METHOD,
  OBJ_ARRAY_INSTANCE
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj *next;
};

typedef struct {
  Obj obj;
  int arity;
  Chunk chunk;
  ObjString *name;
  int upvalueCount;
} ObjFunction;

typedef bool (*NativeFn)(int argCount, Value *args);

typedef struct {
  Obj obj;
  NativeFn function;
  int arity;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  uint32_t hash;
  char chars[];
};

typedef struct ObjUpvalue {
  Obj obj;
  Value *location;
  Value closed;
  struct ObjUpvalue *next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  int upvalueCount;
} ObjClosure;

typedef struct {
  Obj obj;
  ObjString *name;
  Table methods;
} ObjClass;

typedef struct {
  Obj obj;
  ObjClass *klass;
  Table fields;
} ObjInstance;

typedef struct {
  Obj obj;
  Value receiver;
  ObjClosure *method;
} ObjBoundMethod;

typedef struct {
  Obj obj;
  Value receiver;
  ObjNative *method;
} ObjBoundNativeMethod;

typedef struct {
  Obj obj;
  ObjClass *klass;
  Table fields;
  ValueArray elements;
} ObjArrayInstance;

ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjNative *newNative(NativeFn function, int arity);
ObjString *makeString(const char *chars, int length);
ObjString *copyString(const char *chars, int length);
ObjString *concatenateStrings(ObjString *a, ObjString *b);
ObjUpvalue *newUpvalue(Value *slot);
ObjClass *newClass(ObjString *name);
ObjInstance *newInstance(ObjClass *klass);
ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method);
ObjBoundNativeMethod *newBoundNativeMethod(Value receiver, ObjNative *method);
ObjArrayInstance *newArrayInstance(ObjClass *klass);
uint32_t hashString(const char *key, int length);

void printObject(FILE *stream, Value value);
char *writeObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}
#endif
