#ifndef clox_codegen_h
#define clox_codegen_h

#include "cfg.h"
#include "chunk.h"

Chunk *allocateChunk();
Chunk *generateChunk(BasicBlock *bb);

#endif
