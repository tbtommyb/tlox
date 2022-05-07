#ifndef clox_compiler_h
#define clox_compiler_h

#include "common.h"
#include "object.h"
#include "parser.h"
#include "symbol_table.h"
#include "token.h"
#include "vm.h"
#include <stdio.h>

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
  TYPE_METHOD,
  TYPE_INITIALIZER,
} FunctionType;

typedef struct CompilerState {
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

// FIXME: for now this is a mix of compile time and run time data
typedef struct Scope {
  struct Scope *enclosing;

  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;

  int loopOffset;
  int currentStackDepth;

  // FIXME: store symbol table elsewhere. CompilerState?
  SymbolTable *st;
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
void initCompiler(Parser *parser, Compiler *compiler, FunctionType type,
                  FILE *ostream, FILE *errstream);
ObjFunction *compile(Compiler *compiler, const char *source);
void markCompilerRoots();

#endif
