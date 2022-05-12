#ifndef clox_codegen_h
#define clox_codegen_h

#include "cfg.h"
#include "chunk.h"
#include "compiler.h"
#include "table.h"

Chunk *allocateChunk();
void generateChunk(Compiler *compiler, CFG *cfg, Table *labels, Chunk *chunk);
ObjFunction *compileFunction(Compiler *compiler, CFG *cfg, Table *labels);
ObjFunction *compileMain(Compiler *compiler, CFG *cfg, Node *fs, Table *labels);

#endif
