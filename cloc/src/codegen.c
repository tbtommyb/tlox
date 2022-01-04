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

static void emitReturn(Chunk *chunk) {
  emitByte(chunk, OP_NIL);
  emitByte(chunk, OP_RETURN);
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
  case IR_ADD:
    emitByte(chunk, OP_ADD);
    break;
  case IR_ASSIGN:
    emitConstant(chunk, op->first->val.literal); // TODO hardcode literals
    break;
  case IR_CODE_START:
    break;
  case IR_DIVIDE:
    emitByte(chunk, OP_DIVIDE);
    break;
  case IR_SUBTRACT:
    emitByte(chunk, OP_SUBTRACT);
    break;
  case IR_MODULO:
    emitByte(chunk, OP_MODULO);
    break;
  case IR_MULTIPLY:
    emitByte(chunk, OP_MULTIPLY);
    break;
  case IR_NEGATE:
    emitByte(chunk, OP_NEGATE);
    break;
  case IR_NOT:
    emitByte(chunk, OP_NOT);
    break;
  case IR_PRINT:
    emitByte(chunk, OP_PRINT);
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
  emitReturn(chunk);
  return chunk;
}
