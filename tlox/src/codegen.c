#import "codegen.h"
#include "chunk.h"
#import "memory.h"

static void emitByte(Chunk *chunk, uint8_t byte) {
  writeChunk(chunk, byte, 123); // FIXME
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

// Variables stuff
static int searchConstantsFor(Chunk *chunk, Value value) {
  ValueArray constants = chunk->constants;
  for (int i = 0; i < constants.count; i++) {
    Value constant = constants.values[i];
    if (valuesEqual(constant, value)) {
      return i;
    }
  }
  return -1;
}

static uint8_t identifierConstant(Chunk *chunk, Value name) {
  int constantIndex = searchConstantsFor(chunk, name);
  if (constantIndex != -1) {
    return constantIndex;
  }
  return makeConstant(chunk, name);
}
// End variables stuff

static void writeOperation(Operation *op, Chunk *chunk, Table *labels) {
  switch (op->opcode) {
  case IR_ADD:
    emitByte(chunk, OP_ADD);
    break;
  case IR_ASSIGN:
    emitConstant(chunk, op->first->val.literal); // TODO hardcode literals
    break;
  case IR_CODE_START:
    break;
  case IR_COND: {
    // Write label ID into instruction stream and later rewrite symbolic
    // addresses
    emitByte(chunk, OP_JUMP_IF_FALSE);
    emitByte(chunk, (op->second->val.label >> 8) & 0xff);
    emitByte(chunk, op->second->val.label & 0xff);
    emitByte(chunk, OP_POP);
    break;
  }
  case IR_GOTO: {
    emitByte(chunk, OP_JUMP);
    emitByte(chunk, (op->first->val.label >> 8) & 0xff);
    emitByte(chunk, op->first->val.label & 0xff);
    break;
  }
  case IR_LABEL: {
    tableSet(labels, NUMBER_VAL(op->first->val.label),
             NUMBER_VAL(chunk->count - 1));
    break;
  }
  case IR_ELSE_LABEL: {
    tableSet(labels, NUMBER_VAL(op->first->val.label),
             NUMBER_VAL(chunk->count - 1));
    emitByte(chunk, OP_POP);
    break;
  }
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
  case IR_DEFINE: {
    Value name = op->first->val.literal;
    int arg = identifierConstant(chunk, name);
    emitBytes(chunk, OP_DEFINE_GLOBAL, (uint8_t)arg);
    break;
  }
  case IR_VARIABLE: {
    Value name = op->first->val.literal;
    int arg = identifierConstant(chunk, name);
    emitBytes(chunk, OP_GET_GLOBAL, (uint8_t)arg);
    break;
  }
  case IR_VARIABLE_ASSIGN: {
    Value name = op->first->val.literal;
    int arg = identifierConstant(chunk, name);
    emitBytes(chunk, OP_SET_GLOBAL, (uint8_t)arg);
    emitByte(chunk, OP_POP);
    break;
  }
  default:
    printf("Unknown opcode %d\n", op->opcode);
  }
}

static void generateBasicBlockCode(Chunk *chunk, BasicBlock *bb,
                                   Table *labels) {
  Operation *curr = bb->ops;
  int i = 0;

  while (curr != NULL && i < bb->opsCount) {
    writeOperation(curr, chunk, labels);
    curr = curr->next;
    i++;
  }
}

static void rewriteLabels(Chunk *chunk, Table *labels) {
  // Iterate through and rewrite all JUMP addresses using stored addresses
  int index = 0;
  while (index < chunk->count) {
    if (chunk->code[index] == OP_JUMP ||
        chunk->code[index] == OP_JUMP_IF_FALSE) {
      uint8_t hi = chunk->code[index + 1];
      uint8_t lo = chunk->code[index + 2];
      LabelId labelId = (hi << 8) | lo;
      Value location;
      if (!tableGet(labels, NUMBER_VAL(labelId), &location)) {
        // error
        printf("No position found for label %llu\n", labelId);
      }
      int offset = (int)AS_NUMBER(location) - index - 2;
      chunk->code[index + 1] = (offset >> 8) & 0xff;
      chunk->code[index + 2] = offset & 0xff;
      index++;
    }
    index++;
  }
}

Chunk *generateChunk(CFG *cfg, Table *labels) {
  Chunk *chunk = allocateChunk();

  LinkedList *postOrdered = postOrderTraverse(cfg);
  Node *tail = postOrdered->tail;
  while (tail != NULL) {
    generateBasicBlockCode(chunk, tail->data, labels);
    tail = tail->prev;
  }

  rewriteLabels(chunk, labels);

  // TODO: should be pop after expression statements
  emitReturn(chunk);
  return chunk;
}
