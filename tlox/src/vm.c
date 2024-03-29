#include <errno.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "lox_array.h"
#include "memory.h"
#include "object.h"
#include "parser.h"
#include "table.h"
#include "value.h"
#include "vm.h"

VM vm;

static void resetStack() {
  vm.stackCount = 0;
  vm.frameCount = 0;
  vm.openUpvalues = NULL;
}

void runtimeError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(vm.errstream, format, args);
  va_end(args);
  fputs("\n", vm.errstream);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame *frame = &vm.frames[i];
    ObjFunction *function = frame->closure->function;
    size_t instruction = frame->ip - function->chunk.code - 1;
    int line = getLine(&function->chunk, instruction);
    fprintf(vm.errstream, "[line %d] in ", line);
    if (function->name == NULL) {
      fprintf(vm.errstream, "script\n");
    } else {
      fprintf(vm.errstream, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}

static bool clockNative(int argCount, Value *args) {
  args[-1] = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
  return true;
}

static bool writeNative(int argCount, Value *args) {
  const char *path = AS_CSTRING(args[0]);
  const char *data = AS_CSTRING(args[1]);
  const char *mode = AS_CSTRING(args[2]);

  FILE *handle = fopen(path, mode);
  if (handle == NULL) {
    runtimeError("Could not open file %s. Error %d.", path, errno);
    return false;
  }

  int res = fputs(data, handle);
  if (res < 0) {
    runtimeError("Could not write to file %s. Error %d", path, errno);
    fclose(handle);
    return false;
  }

  int closeRes = fclose(handle);
  if (closeRes != 0) {
    runtimeError("Could not close file %s. Error %d.", path, errno);
    return false;
  }

  args[-1] = NUMBER_VAL((double)res);
  return true;
}

static bool readNative(int argCount, Value *args) {
  const char *path = AS_CSTRING(args[0]);

  FILE *handle = fopen(path, "r");
  if (handle == NULL) {
    runtimeError("Could not open file %s. Error %d.", path, errno);
    return false;
  }

  fseek(handle, 0L, SEEK_END);
  int bytesToRead = ftell(handle);
  fseek(handle, 0L, SEEK_SET);
  char *buffer = ALLOCATE(char, bytesToRead);
  if (buffer == NULL) {
    runtimeError("Could not open file %s. Out of memory.", path);
    return false;
  }

  int charsRead = fread(buffer, sizeof(char), bytesToRead, handle);
  fclose(handle);

  args[-1] = OBJ_VAL(copyString(buffer, charsRead));
  return true;
}

static void defineNative(const char *name, NativeFn function, int arity) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function, arity)));
  tableSet(&vm.globals, vm.stack[0], vm.stack[1]);
  pop();
  pop();
}

static void defineNativeClass(const char *name, ObjClass *klass) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(klass));
  tableSet(&vm.globals, vm.stack[0], vm.stack[1]);
  pop();
  pop();
}

void initVM(FILE *ostream, FILE *errstream) {
  resetStack();
  vm.stackCapacity = 256;
  vm.stack = GROW_ARRAY(Value, vm.stack, 0, vm.stackCapacity);

  vm.objects = NULL;
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;

  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;

  initTable(&vm.strings);
  initTable(&vm.globals);

  vm.initString = NULL;
  vm.initString = copyString("init", 4);

  defineNative("clock", clockNative, 0);
  defineNative("write", writeNative, 3);
  defineNative("read", readNative, 1);

  vm.classes.array = createArrayClass(vm.initString);
  defineNativeClass("Array", vm.classes.array);

  vm.ostream = ostream;
  vm.errstream = errstream;
}

void freeVM() {
  FREE_ARRAY(Value, vm.stack, vm.stackCapacity);
  freeTable(&vm.strings);
  freeTable(&vm.globals);
  vm.initString = NULL;
  freeObjects();
}

void push(Value value) {
  if (vm.stackCapacity < vm.stackCount + 1) {
    int oldCapacity = vm.stackCapacity;
    vm.stackCapacity = GROW_CAPACITY(oldCapacity);
    vm.stack = GROW_ARRAY(Value, vm.stack, oldCapacity, vm.stackCapacity);
  }
  vm.stack[vm.stackCount] = value;
  vm.stackCount++;
}

Value pop() {
  vm.stackCount--;
  return vm.stack[vm.stackCount];
}

static Value peek(int distance) {
  return vm.stack[vm.stackCount - 1 - distance];
}

static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static bool call(ObjClosure *closure, int argCount) {
  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }
  if (argCount < closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.", closure->function->arity,
                 argCount);
    return false;
  }

  CallFrame *frame = &vm.frames[vm.frameCount++];
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
  frame->slots = &vm.stack[vm.stackCount - argCount - 1];

  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_BOUND_METHOD: {
      ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
      vm.stack[vm.stackCount - argCount - 1] = bound->receiver;
      return call(bound->method, argCount);
    }
    case OBJ_BOUND_NATIVE_METHOD: {
      ObjBoundNativeMethod *bound = AS_BOUND_NATIVE_METHOD(callee);
      ObjNative *native = bound->method;
      vm.stack[vm.stackCount - argCount - 1] = bound->receiver;
      if (argCount < native->arity) {
        runtimeError("Expected %d arguments but got %d.", native->arity,
                     argCount);
        return false;
      }
      if (native->function(argCount, &vm.stack[vm.stackCount - argCount - 1])) {
        vm.stackCount -= argCount;
        return true;
      } else {
        return false;
      }
    }
    case OBJ_CLOSURE:
      return call(AS_CLOSURE(callee), argCount);
    case OBJ_NATIVE: {
      ObjNative *native = AS_NATIVE(callee);
      if (argCount < native->arity) {
        runtimeError("Expected %d arguments but got %d.", native->arity,
                     argCount);
        return false;
      }
      if (native->function(argCount, &vm.stack[vm.stackCount - argCount])) {
        vm.stackCount -= argCount;
        return true;
      } else {
        return false;
      }
    }
    case OBJ_CLASS: {
      ObjClass *klass = AS_CLASS(callee);
      vm.stack[vm.stackCount - argCount - 1] = OBJ_VAL(newInstance(klass));
      Value initializer;
      if (tableGet(&klass->methods, OBJ_VAL(vm.initString), &initializer)) {

        if (argCount != AS_CLOSURE(initializer)->function->arity) {
          runtimeError("Expected %d arguments but got %d.",
                       AS_CLOSURE(initializer)->function->arity, argCount);
          return false;
        }

        return call(AS_CLOSURE(initializer), argCount);
      } else if (argCount != 0) {
        runtimeError("Expected 0 arguments but got %d.", argCount);
        return false;
      }
      return true;
    }
    default:
      break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}

static bool invokeFromClass(ObjClass *klass, ObjString *name, int argCount) {
  Value method;
  if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }
  /* Begin ugly native handling code */
  if (IS_NATIVE(method)) {
    ObjNative *native = AS_NATIVE(method);
    if (argCount < native->arity) {
      runtimeError("Expected %d arguments but got %d.", native->arity,
                   argCount);
      return false;
    }
    if (native->function(argCount, &vm.stack[vm.stackCount - argCount - 1])) {
      vm.stackCount -= argCount;
      return true;
    } else {
      return false;
    }
  }
  /* End ugly native handling code */
  return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString *name, int argCount) {
  Value receiver = peek(argCount);

  if (!IS_INSTANCE(receiver) && !IS_ARRAY_INSTANCE(receiver)) {
    runtimeError("Only instances have methods.");
    return false;
  }

  ObjInstance *instance = AS_INSTANCE(receiver);

  Value value;
  if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
    vm.stack[vm.stackCount - argCount - 1] = value;
    return callValue(value, argCount);
  }

  return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass *klass, ObjString *name) {
  Value method;
  if (!tableGet(&klass->methods, OBJ_VAL(name), &method)) {
    return false;
  }

  if (IS_NATIVE(method)) {
    ObjBoundNativeMethod *bound =
        newBoundNativeMethod(peek(0), AS_NATIVE(method));
    pop();
    push(OBJ_VAL(bound));
  } else {
    ObjBoundMethod *bound = newBoundMethod(peek(0), AS_CLOSURE(method));
    pop();
    push(OBJ_VAL(bound));
  }
  return true;
}

static ObjUpvalue *captureUpvalue(Value *local) {
  ObjUpvalue *prevUpvalue = NULL;
  ObjUpvalue *upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue *createdUpvalue = newUpvalue(local);
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }
  return createdUpvalue;
}

static void closeUpvalues(Value *last) {
  while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
    ObjUpvalue *upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}

static void defineMethod(ObjString *name) {
  Value method = peek(0);
  ObjClass *klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, OBJ_VAL(name), method);
  pop();
}

static void concatenate() {
  ObjString *b = AS_STRING(peek(0));
  ObjString *a = AS_STRING(peek(1));

  ObjString *result = concatenateStrings(a, b);
  pop();
  pop();
  push(OBJ_VAL(result));
}

static void createArray(int len) {
  ObjArrayInstance *instance = newArrayInstance(vm.classes.array);

  int startingPoint = vm.stackCount - len;
  for (int i = startingPoint; i < vm.stackCount; i++) {
    Value item = vm.stack[i];
    writeValueArray(&instance->elements, item);
  }
  for (int i = 0; i < len; i++) {
    pop();
  }

  push(OBJ_VAL(instance));
}

static InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.frameCount - 1];
  register uint8_t *ip = frame->ip;

#define READ_BYTE() (*ip++)
#define READ_CONSTANT()                                                        \
  (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {                          \
      frame->ip = ip;                                                          \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    double b = AS_NUMBER(pop());                                               \
    double a = AS_NUMBER(pop());                                               \
    push(valueType(a op b));                                                   \
  } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (int i = 0; i < vm.stackCount; i++) {
      Value slot = vm.stack[i];
      printf("[ ");
      printValue(vm.ostream, slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk,
                           (int)(ip - frame->closure->function->chunk.code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_NIL:
      push(NIL_VAL);
      break;
    case OP_TRUE:
      push(BOOL_VAL(true));
      break;
    case OP_FALSE:
      push(BOOL_VAL(false));
      break;
    case OP_ADD: {
      // TODO support array concatenation
      if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
        concatenate();
      } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
        double b = AS_NUMBER(pop());
        double a = AS_NUMBER(pop());
        push(NUMBER_VAL(a + b));
      } else {
        frame->ip = ip;
        runtimeError("Operands must be two numbers or two strings.");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUBTRACT:
      BINARY_OP(NUMBER_VAL, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(NUMBER_VAL, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(NUMBER_VAL, /);
      break;
    case OP_MODULO:
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {
        frame->ip = ip;
        runtimeError("Operands must be numbers.");
        return INTERPRET_RUNTIME_ERROR;
      }
      double b = AS_NUMBER(pop());
      double a = AS_NUMBER(pop());
      push(NUMBER_VAL(fmod(a, b)));
      break;
    case OP_NOT:
      push(BOOL_VAL(isFalsey(pop())));
      break;
    case OP_NEGATE:
      if (!IS_NUMBER(peek(0))) {
        frame->ip = ip;
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }

      vm.stack[vm.stackCount - 1] =
          NUMBER_VAL(-AS_NUMBER(vm.stack[vm.stackCount - 1]));
      break;
    case OP_EQUAL: {
      Value b = pop();
      Value a = pop();
      push(BOOL_VAL(valuesEqual(a, b)));
      break;
    }
    case OP_EQUAL_PEEK: {
      Value b = pop();
      Value a = peek(0);
      push(BOOL_VAL(valuesEqual(a, b)));
      break;
    }
    case OP_GREATER:
      BINARY_OP(BOOL_VAL, >);
      break;
    case OP_LESS:
      BINARY_OP(BOOL_VAL, <);
      break;
    case OP_PRINT: {
      printValue(vm.ostream, pop());
      fprintf(vm.ostream, "\n");
      break;
    }
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(peek(0))) {
        ip += offset;
      }
      break;
    }
    case OP_LOOP: {
      uint16_t offset = READ_SHORT();
      ip -= offset;
      break;
    }
    case OP_RETURN: {
      Value result = pop();
      closeUpvalues(frame->slots);
      vm.frameCount--;
      if (vm.frameCount == 0) {
        pop();
        return INTERPRET_OK;
      }

      vm.stackCount -= vm.stack + vm.stackCount - frame->slots;
      push(result);
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_CALL: {
      int argCount = READ_BYTE();
      frame->ip = ip;
      if (!callValue(peek(argCount), argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_POP:
      pop();
      break;
    case OP_DEFINE_GLOBAL: {
      ObjString *name = READ_STRING();
      tableSet(&vm.globals, OBJ_VAL(name), peek(0));
      pop();
      break;
    }
    case OP_GET_LOCAL: {
      uint8_t slot = READ_BYTE();
      push(frame->slots[slot]);
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString *name = READ_STRING();
      Value value;
      if (!tableGet(&vm.globals, OBJ_VAL(name), &value)) {
        frame->ip = ip;
        runtimeError("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      push(value);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      frame->slots[slot] = peek(0);
      break;
    }
    case OP_SET_GLOBAL: {
      ObjString *name = READ_STRING();
      if (tableSet(&vm.globals, OBJ_VAL(name), peek(0))) {
        tableDelete(&vm.globals, OBJ_VAL(name));
        frame->ip = ip;
        runtimeError("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_CLOSURE: {
      ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
      ObjClosure *closure = newClosure(function);
      push(OBJ_VAL(closure));
      for (int i = 0; i < closure->upvalueCount; i++) {
        uint8_t isLocal = READ_BYTE();
        uint8_t index = READ_BYTE();
        if (isLocal) {
          closure->upvalues[i] = captureUpvalue(frame->slots + index);
        } else {
          closure->upvalues[i] = frame->closure->upvalues[index];
        }
      }
      break;
    }
    case OP_GET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      push(*frame->closure->upvalues[slot]->location);
      break;
    }
    case OP_SET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      *frame->closure->upvalues[slot]->location = peek(0);
      break;
    }
    case OP_CLASS: {
      push(OBJ_VAL(newClass(READ_STRING())));
      break;
    }
    case OP_ARRAY: {
      createArray(READ_BYTE());
      break;
    }
    case OP_GET_PROPERTY: {
      if (!IS_INSTANCE(peek(0)) && !IS_ARRAY_INSTANCE(peek(0))) {
        frame->ip = ip;
        runtimeError("Only instances have properties.");
        return INTERPRET_RUNTIME_ERROR;
      }
      ObjInstance *instance = AS_INSTANCE(peek(0));
      ObjString *name = READ_STRING();

      Value value;
      if (tableGet(&instance->fields, OBJ_VAL(name), &value)) {
        pop(); // Instance.
        push(value);
        break;
      }
      if (!bindMethod(instance->klass, name)) {
        push(NIL_VAL);
        break;
      }
      break;
    }
    case OP_GET_COMPUTED_PROPERTY: {
      if (IS_ARRAY_INSTANCE(peek(1))) {
        Value property = pop();
        ObjArrayInstance *instance = AS_ARRAY_INSTANCE(pop());
        if (!IS_NUMBER(property)) {
          frame->ip = ip;
          runtimeError("Only numbers supported as array indexes");
          return INTERPRET_RUNTIME_ERROR;
        }
        ValueArray *array = &instance->elements;
        int idx = (int)AS_NUMBER(property);
        if (idx < 0 || idx >= array->count) {
          frame->ip = ip;
          runtimeError("Index out of bounds");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(array->values[idx]);
        break;
      }

      if (!IS_INSTANCE(peek(1))) {
        frame->ip = ip;
        runtimeError("Only instances have properties.");
        return INTERPRET_RUNTIME_ERROR;
      }
      Value name = pop();
      ObjInstance *instance = AS_INSTANCE(peek(0));

      if (!IS_STRING(name)) {
        frame->ip = ip;
        runtimeError("Only strings supported as computed property names");
        return INTERPRET_RUNTIME_ERROR;
      }

      Value value;
      if (tableGet(&instance->fields, name, &value)) {
        pop(); // Instance.
        push(value);
        break;
      }
      if (!bindMethod(instance->klass, AS_STRING(name))) {
        push(NIL_VAL);
      }
      break;
    }
    case OP_SET_PROPERTY: {
      if (!IS_INSTANCE(peek(1)) && !IS_ARRAY_INSTANCE(peek(1))) {
        frame->ip = ip;
        runtimeError("Only instances have fields.");
        return INTERPRET_RUNTIME_ERROR;
      }
      ObjInstance *instance = AS_INSTANCE(peek(1));
      tableSet(&instance->fields, OBJ_VAL(READ_STRING()), peek(0));
      Value value = pop();
      pop();
      push(value);
      break;
    }
    case OP_SET_COMPUTED_PROPERTY: {
      if (IS_ARRAY_INSTANCE(peek(2))) {
        ObjArrayInstance *instance = AS_ARRAY_INSTANCE(peek(2));
        if (!IS_NUMBER(peek(1))) {
          frame->ip = ip;
          runtimeError("Index must be a number");
          return INTERPRET_RUNTIME_ERROR;
        }
        ValueArray *array = &instance->elements;
        int idx = (int)AS_NUMBER(peek(1));
        if (idx < 0 || idx >= array->count) {
          frame->ip = ip;
          runtimeError("Index out of bounds");
          return INTERPRET_RUNTIME_ERROR;
        }
        array->values[idx] = peek(0);
      } else {
        if (!IS_INSTANCE(peek(2))) {
          frame->ip = ip;
          runtimeError("Only instances have fields.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance *instance = AS_INSTANCE(peek(2));
        tableSet(&instance->fields, peek(1), peek(0));
      }
      Value value = pop();
      pop(); // Computed
      pop(); // Instance
      push(value);
      break;
    }
    case OP_INVOKE: {
      ObjString *method = READ_STRING();
      int argCount = READ_BYTE();
      frame->ip = ip;
      if (!invoke(method, argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_INHERIT: {
      Value superclass = peek(1);
      if (!IS_CLASS(superclass)) {
        frame->ip = ip;
        runtimeError("Superclass must be a class.");
        return INTERPRET_RUNTIME_ERROR;
      }
      ObjClass *subclass = AS_CLASS(peek(0));
      tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
      pop(); // Subclass.
      break;
    }
    case OP_GET_SUPER: {
      ObjString *name = READ_STRING();
      ObjClass *superclass = AS_CLASS(pop());

      if (!bindMethod(superclass, name)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUPER_INVOKE: {
      ObjString *method = READ_STRING();
      int argCount = READ_BYTE();
      ObjClass *superclass = AS_CLASS(pop());
      frame->ip = ip;
      if (!invokeFromClass(superclass, method, argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.frameCount - 1];
      ip = frame->ip;
      break;
    }
    case OP_METHOD:
      defineMethod(READ_STRING());
      break;
    case OP_CLOSE_UPVALUE:
      closeUpvalues(&vm.stack[vm.stackCount - 1]);
      pop();
      break;
    }
  }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
  Parser parser = {0};
  Compiler compiler = {0};
  initCompiler(&parser, &compiler, TYPE_SCRIPT, vm.ostream, vm.errstream);
  ObjFunction *function = compile(&compiler, source);
  if (function == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }

  push(OBJ_VAL(function));
  ObjClosure *closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);

  return run();
}
