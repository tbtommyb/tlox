#ifndef clox_debug_h
#define clox_debug_h

#include "cfg.h"
#include "chunk.h"

void disassembleChunk(Chunk *chunk, const char *name);
void disassembleWorkUnits(WorkUnit *root);
int disassembleInstruction(Chunk *chunk, int offset);

#endif
