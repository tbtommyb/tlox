#ifndef clox_compiler_h
#define clox_compiler_h

#include "common.h"
#include "object.h"
#include "parser.h"
#include "vm.h"
#include <stdio.h>

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
  TYPE_METHOD,
  TYPE_INITIALIZER,
} FunctionType;

typedef struct CompilerState {
  Table *globalConsts;
  Table *stringConstants;
  Table *labels;
} CompilerState;

typedef struct {
  Token name;
  int depth;
  bool isConst;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef struct Scope {
  struct Scope *enclosing;

  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;

  int loopOffset;
  int currentStackDepth;
} Scope;

typedef struct Compiler {
  bool hadError;
  bool panicMode;
  FILE *ostream;
  FILE *errstream;
  Parser *parser;
  Scope *currentScope;
  FunctionType type;
} Compiler;

void errorAt(Compiler *compiler, Token *token, const char *message);
void error(Compiler *compiler, const char *message);
void errorAtCurrent(Compiler *compiler, const char *message);
Compiler initCompiler(FunctionType type, FILE *ostream, FILE *errstream);
ObjFunction *compile(Compiler *compiler, const char *source);
void markCompilerRoots();

#endif
