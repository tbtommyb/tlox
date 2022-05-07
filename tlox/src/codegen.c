#include "codegen.h"
#include "assert.h"
#include "chunk.h"
#include "memory.h"

static void emitByte(Chunk *chunk, uint8_t byte) {
  writeChunk(chunk, byte, 123); // FIXME: line number would be from token
}

static void emitBytes(Chunk *chunk, uint8_t byte1, uint8_t byte2) {
  emitByte(chunk, byte1);
  emitByte(chunk, byte2);
}

static void emitReturn(Chunk *chunk) {
  emitByte(chunk, OP_NIL);
  emitByte(chunk, OP_RETURN);
}

static void emitConstant(Chunk *chunk, uint8_t position) {
  emitBytes(chunk, OP_CONSTANT, position);
}

Chunk *allocateChunk() {
  Chunk *chunk = (Chunk *)reallocate(NULL, 0, sizeof(Chunk));
  initChunk(chunk);
  return chunk;
}

static uint8_t makeConstant(Compiler *compiler, Chunk *chunk, Value value) {
  int constant = addConstant(chunk, value);
  if (constant > UINT8_MAX) {
    error(compiler, "Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static int searchConstantsFor(Chunk *chunk, Value value) {
  // FIXME look in symbol table instead of runtime
  ValueArray constants = chunk->constants;
  for (int i = 0; i < constants.count; i++) {
    Value constant = constants.values[i];
    if (valuesEqual(constant, value)) {
      return i;
    }
  }
  return -1;
}

static uint8_t identifierConstant(Compiler *compiler, Chunk *chunk,
                                  Value name) {
  int constantIndex = searchConstantsFor(chunk, name);
  if (constantIndex != -1) {
    return constantIndex;
  }
  return makeConstant(compiler, chunk, name);
}

static int resolveLocal(Compiler *compiler, Token *name) {
  for (int i = compiler->currentScope->localCount - 1; i >= 0; i--) {
    Local *local = &compiler->currentScope->locals[i];
    if (identifiersEqual(name, &local->name)) {
      return i;
    }
  }

  return -1;
}

static void writeOperation(Compiler *compiler, Operation *op, Chunk *chunk,
                           Table *labels) {
  switch (op->opcode) {
  case IR_ADD:
    emitByte(chunk, OP_ADD);
    break;
  case IR_CONSTANT: {
    Value literal = op->first->val.literal;
    uint8_t position = identifierConstant(compiler, chunk, literal);
    emitConstant(chunk, position);
    break;
  }
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
  case IR_NIL:
    emitByte(chunk, OP_NIL);
    break;
  case IR_NOT:
    emitByte(chunk, OP_NOT);
    break;
  case IR_POP:
    emitByte(chunk, OP_POP);
    break;
  case IR_PRINT:
    emitByte(chunk, OP_PRINT);
    break;
  case IR_DEFINE_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, chunk, name);

    emitBytes(chunk, OP_DEFINE_GLOBAL, position);
    break;
  }
  case IR_DEFINE_LOCAL: {
    Symbol symbol = op->first->val.symbol;
    Local *local =
        &compiler->currentScope->locals[compiler->currentScope->localCount++];
    local->name = symbol.name;
    local->depth = compiler->currentScope->scopeDepth;
    local->isCaptured = symbol.isCaptured;
    local->isConst = symbol.isConst; // TODO: remove isConst
    break;
  }
  case IR_GET_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, chunk, name);

    emitBytes(chunk, OP_GET_GLOBAL, position);
    break;
  }
  case IR_GET_LOCAL: {
    Symbol symbol = op->first->val.symbol;
    int position = resolveLocal(compiler, &symbol.name);

    assert(position != -1);

    emitBytes(chunk, OP_GET_LOCAL, (uint8_t)position);
    break;
  }
  case IR_SET_GLOBAL: {
    Value name = op->first->val.literal;
    Symbol symbol = op->second->val.symbol;
    uint8_t position = identifierConstant(compiler, chunk, name);

    emitBytes(chunk, OP_SET_GLOBAL, position);
    emitByte(chunk, OP_POP);
    break;
  }
  case IR_SET_LOCAL: {
    Symbol symbol = op->second->val.symbol;
    int position = resolveLocal(compiler, &symbol.name);

    assert(position != -1);

    emitBytes(chunk, OP_SET_LOCAL, (uint8_t)position);
    emitByte(chunk, OP_POP);
    break;
  }
  default:
    printf("Unknown opcode %d\n", op->opcode);
  }
}

static void generateBasicBlockCode(Compiler *compiler, Chunk *chunk,
                                   BasicBlock *bb, Table *labels) {
  Operation *curr = bb->ops;
  int i = 0;

  while (curr != NULL && i < bb->opsCount) {
    writeOperation(compiler, curr, chunk, labels);
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

// FIXME: rename
void generateChunk(Compiler *compiler, CFG *cfg, Table *labels, Chunk *chunk) {
  LinkedList *postOrdered = postOrderTraverse(cfg);
  Node *tail = postOrdered->tail;
  while (tail != NULL) {
    generateBasicBlockCode(compiler, chunk, tail->data, labels);
    tail = tail->prev;
  }

  rewriteLabels(chunk, labels);

  // TODO: should be pop after expression statements
  emitReturn(chunk);
}
