#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction *compile(const char *source, FILE *ostream, FILE *errstream);
void markCompilerRoots();

typedef struct {
  Table *globalConsts;
  Table *stringConstants;
  Table *labels;
} CompilerState;

#endif
