#ifndef SEMANTIC_H_
#define SEMANTIC_H_

#include "ast.h"
#include "compiler.h"

void analyse(AstNode *ast, Compiler *compiler, CompilerState *state);

#endif // SEMANTIC_H_
