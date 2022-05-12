#ifndef SEMANTIC_H_
#define SEMANTIC_H_

#include "ast.h"
#include "common.h"
#include "compiler.h"

void analyse(AstNode *ast, Compiler *compiler, FunctionType currentEnv);

#endif // SEMANTIC_H_
