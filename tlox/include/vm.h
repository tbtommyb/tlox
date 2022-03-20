#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  Obj *function;
  uint8_t *ip;
  Value *slots;
  int slotCount;
} CallFrame;

typedef struct {
  ObjClass *array;
} NativeClassReferences;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  int frameCount;

  int stackCount;
  int stackCapacity;
  Value *stack;

  Table strings;
  ObjString *initString;
  Table globals;
  ObjUpvalue *openUpvalues;

  size_t bytesAllocated;
  size_t nextGC;
  Obj *objects;
  int grayCount;
  int grayCapacity;
  Obj **grayStack;

  NativeClassReferences classes;

  FILE *ostream;
  FILE *errstream;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM(FILE *ostream, FILE *errstream);
void freeVM();
InterpretResult interpret(const char *source);
void push(Value value);
Value pop();
void runtimeError(const char *format, ...);

#endif