#ifndef clox_codegen_h
#define clox_codegen_h

#include "cfg.h"
#include "chunk.h"
#include "compiler.h"
#include "table.h"

Chunk *allocateChunk();
void generateChunk(Compiler *compiler, CFG *cfg, Table *labels, Chunk *chunk);

#endif
