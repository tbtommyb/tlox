#ifndef clox_compiler_h
#define clox_compiler_h

#include "common.h"
#include "object.h"
#include "parser.h"
#include "scope.h"
#include "symbol_table.h"
#include "token.h"
#include "vm.h"
#include <stdio.h>

typedef struct Compiler {
  bool hadError;
  bool panicMode;
  FILE *ostream;
  FILE *errstream;
  Parser *parser;
  Scope *currentScope;
  FunctionType type;
  Table *labels;
} Compiler;

void errorAt(Compiler *compiler, Token *token, const char *message);
void error(Compiler *compiler, const char *message);
void errorAtCurrent(Compiler *compiler, const char *message);
void initCompiler(Parser *parser, Compiler *compiler, FunctionType type,
                  FILE *ostream, FILE *errstream);
ObjFunction *compile(Compiler *compiler, const char *source);
void markCompilerRoots();

#endif
