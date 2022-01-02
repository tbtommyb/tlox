#import "codegen.h"
#include "chunk.h"
#import "memory.h"

static void emitByte(Chunk *chunk, uint8_t byte) {
  writeChunk(chunk, byte, 123);
}

static void emitBytes(Chunk *chunk, uint8_t byte1, uint8_t byte2) {
  emitByte(chunk, byte1);
  emitByte(chunk, byte2);
}

static uint8_t makeConstant(Chunk *chunk, Value value) {
  int constant = addConstant(chunk, value);
  if (constant > UINT8_MAX) {
    /* error("Too many constants in one chunk."); */
    return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Chunk *chunk, Value value) {
  emitBytes(chunk, OP_CONSTANT, makeConstant(chunk, value));
}

Chunk *allocateChunk() {
  Chunk *chunk = (Chunk *)reallocate(NULL, 0, sizeof(Chunk));
  initChunk(chunk);
  return chunk;
}

void writeOperation(Operation *op, Chunk *chunk) {
  switch (op->opcode) {
  case CFG_ADD:
    emitByte(chunk, OP_ADD);
    break;
  case CFG_ASSIGN:
    emitConstant(chunk, op->first->val.literal); // TODO hardcode literals
    break;
  case CFG_DIVIDE:
    emitByte(chunk, OP_DIVIDE);
    break;
  case CFG_MINUS:
    emitByte(chunk, OP_SUBTRACT);
    break;
  case CFG_MODULO:
    emitByte(chunk, OP_MODULO);
    break;
  case CFG_MULTIPLY:
    emitByte(chunk, OP_MULTIPLY);
    break;
  case CFG_NEGATE:
    emitByte(chunk, OP_NEGATE);
    break;
  default:
    printf("Unknown opcode %d\n", op->opcode);
  }
}

Chunk *generateChunk(BasicBlock *bb) {
  Chunk *chunk = allocateChunk();

  Operation *current = bb->ops;
  while (current != NULL) {
    writeOperation(current, chunk);
    current = current->next;
  }
  emitByte(chunk, OP_POP);
  emitByte(chunk, OP_NIL);
  emitByte(chunk, OP_RETURN);
  return chunk;
}
