#ifndef clox_codegen_h
#define clox_codegen_h

#include "cfg.h"
#include "chunk.h"
#include "table.h"

Chunk *allocateChunk();
Chunk *generateChunk(CFG *cfg, Table *labels);

#endif
