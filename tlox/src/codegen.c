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

static int resolveLocal(ExecutionContext *context, Token *name) {
  // FIXME: values read from stack are offset by one
  for (int i = context->localCount - 1; i >= 0; i--) {
    Local local = context->locals[i];
    if (identifiersEqual(name, &local.name)) {
      return i;
    }
  }

  return -1;
}

static int addUpvalue(ExecutionContext *context, ObjFunction *f, uint8_t index,
                      bool isLocal) {
  int upvalueCount = f->upvalueCount;

  for (int i = 0; i < upvalueCount; i++) {
    Upvalue *upvalue = &context->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    /* error("Too many closure variables in function."); */
    return 0;
  }

  context->upvalues[upvalueCount].isLocal = isLocal;
  context->upvalues[upvalueCount].index = index;
  return f->upvalueCount++;
}

static int resolveUpvalue(ExecutionContext *context, ObjFunction *f,
                          Token *name) {
  if (context->enclosing == NULL) {
    return -1;
  }

  int local = resolveLocal(context->enclosing, name);
  if (local != -1) {
    context->enclosing->locals[local].isCaptured = true;
    return addUpvalue(context, f, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(context->enclosing, f, name);
  if (upvalue != -1) {
    return addUpvalue(context, f, (uint8_t)upvalue, false);
  }

  return -1;
}

static void writeOperation(Compiler *compiler, Operation *op, ObjFunction *f,
                           Table *labels, ExecutionContext *context) {
  switch (op->opcode) {
  case IR_ADD:
    emitByte(&f->chunk, OP_ADD);
    break;
  case IR_CONSTANT: {
    Value literal = op->first->val.literal;
    uint8_t position = identifierConstant(compiler, &f->chunk, literal);
    emitConstant(&f->chunk, position);
    break;
  }
  case IR_CALL: {
    Value arity = op->first->val.literal;

    emitBytes(&f->chunk, OP_CALL, AS_NUMBER(arity));
    break;
  }
  case IR_CODE_START:
    break;
  case IR_COND: {
    // Write label ID into instruction stream and later rewrite symbolic
    // addresses
    emitByte(&f->chunk, OP_JUMP_IF_FALSE);
    emitByte(&f->chunk, (op->second->val.label >> 8) & 0xff);
    emitByte(&f->chunk, op->second->val.label & 0xff);
    emitByte(&f->chunk, OP_POP);
    break;
  }
  case IR_COND_NO_POP: {
    // Write label ID into instruction stream and later rewrite symbolic
    // addresses
    emitByte(&f->chunk, OP_JUMP_IF_FALSE);
    emitByte(&f->chunk, (op->second->val.label >> 8) & 0xff);
    emitByte(&f->chunk, op->second->val.label & 0xff);
    break;
  }
  case IR_GOTO: {
    emitByte(&f->chunk, OP_JUMP);
    emitByte(&f->chunk, (op->first->val.label >> 8) & 0xff);
    emitByte(&f->chunk, op->first->val.label & 0xff);
    break;
  }
  case IR_LOOP: {
    emitByte(&f->chunk, OP_LOOP);
    emitByte(&f->chunk, (op->first->val.label >> 8) & 0xff);
    emitByte(&f->chunk, op->first->val.label & 0xff);
    break;
  }
  case IR_LABEL: {
    tableSet(labels, NUMBER_VAL(op->first->val.label),
             NUMBER_VAL(f->chunk.count - 1));
    break;
  }
  case IR_ELSE_LABEL: {
    tableSet(labels, NUMBER_VAL(op->first->val.label),
             NUMBER_VAL(f->chunk.count - 1));
    emitByte(&f->chunk, OP_POP);
    break;
  }
  case IR_DIVIDE:
    emitByte(&f->chunk, OP_DIVIDE);
    break;
  case IR_SUBTRACT:
    emitByte(&f->chunk, OP_SUBTRACT);
    break;
  case IR_MODULO:
    emitByte(&f->chunk, OP_MODULO);
    break;
  case IR_MULTIPLY:
    emitByte(&f->chunk, OP_MULTIPLY);
    break;
  case IR_NEGATE:
    emitByte(&f->chunk, OP_NEGATE);
    break;
  case IR_NIL:
    emitByte(&f->chunk, OP_NIL);
    break;
  case IR_NOT:
    emitByte(&f->chunk, OP_NOT);
    break;
  case IR_NOT_EQUAL:
    emitBytes(&f->chunk, OP_EQUAL, OP_NOT);
    break;
  case IR_EQUAL:
    emitByte(&f->chunk, OP_EQUAL);
    break;
  case IR_GREATER:
    emitByte(&f->chunk, OP_GREATER);
    break;
  case IR_GREATER_EQUAL:
    emitBytes(&f->chunk, OP_LESS, OP_NOT);
    break;
  case IR_LESS:
    emitByte(&f->chunk, OP_LESS);
    break;
  case IR_LESS_EQUAL:
    emitBytes(&f->chunk, OP_GREATER, OP_NOT);
    break;
  case IR_PRINT:
    emitByte(&f->chunk, OP_PRINT);
    break;
  case IR_POP:
    emitByte(&f->chunk, OP_POP);
    break;
  case IR_DEFINE_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, &f->chunk, name);

    emitBytes(&f->chunk, OP_DEFINE_GLOBAL, position);
    break;
  }
  case IR_DEFINE_LOCAL: {
    Symbol symbol = op->first->val.symbol;
    Local *local = &context->locals[context->localCount++];
    local->name = symbol.name;
    local->depth = context->scopeDepth;
    local->isCaptured = symbol.isCaptured;
    break;
  }
  case IR_GET_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, &f->chunk, name);

    emitBytes(&f->chunk, OP_GET_GLOBAL, position);
    break;
  }
  case IR_GET_LOCAL: {
    Symbol symbol = op->first->val.symbol;

    OpCode op = OP_GET_LOCAL;
    int position = resolveLocal(context, &symbol.name);

    if (position == -1) {
      position = resolveUpvalue(context, f, &symbol.name);
      op = OP_GET_UPVALUE;
    }

    assert(position != -1);

    emitBytes(&f->chunk, op, (uint8_t)position);
    break;
  }
  case IR_SET_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value nameString =
        OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    // FIXME: not actually used?
    Register source = op->second->val.source;
    uint8_t position = identifierConstant(compiler, &f->chunk, nameString);

    emitBytes(&f->chunk, OP_SET_GLOBAL, position);
    break;
  }
  case IR_SET_LOCAL: {
    Symbol symbol = op->first->val.symbol;

    OpCode opcode = OP_SET_LOCAL;
    int position = resolveLocal(context, &symbol.name);

    if (position == -1) {
      position = resolveUpvalue(context, f, &symbol.name);
      opcode = OP_SET_UPVALUE;
    }

    assert(position != -1);

    emitBytes(&f->chunk, opcode, (uint8_t)position);
    break;
  }
  case IR_RETURN: {
    emitByte(&f->chunk, OP_RETURN);
    break;
  }
  case IR_BEGIN_SCOPE: {
    context->scopeDepth++;
    break;
  }
  case IR_FUNCTION: {
    Value wuPtr = op->first->val.literal;
    WorkUnit *wu = AS_POINTER(wuPtr);
    ObjFunction *childF = compileWorkUnit(compiler, wu, labels);
    int position = makeConstant(compiler, &f->chunk, OBJ_VAL(childF));

    OpCode op = OP_CONSTANT;
    if (childF->upvalueCount > 0) {
      // FIXME: handle methods and initialisers
      op = OP_CLOSURE;
    }
    emitBytes(&f->chunk, op, position);
    for (int i = 0; i < childF->upvalueCount; i++) {
      // Not sure going through wu->cfg->context is best/correct here
      emitByte(&f->chunk, wu->cfg->context->upvalues[i].isLocal ? 1 : 0);
      emitByte(&f->chunk, wu->cfg->context->upvalues[i].index);
    }

    if (context->enclosing == NULL) {
      int namePosition =
          identifierConstant(compiler, &f->chunk, OBJ_VAL(childF->name));
      emitBytes(&f->chunk, OP_DEFINE_GLOBAL, namePosition);
    } else {
      Local *local = &context->locals[context->localCount++];
      local->name = wu->name;
      local->depth = context->scopeDepth;
      local->isCaptured = false;
    }
    break;
  }
  case IR_END_SCOPE: {
    context->scopeDepth--;

    while (context->localCount > 0 &&
           context->locals[context->localCount - 1].depth >
               context->scopeDepth) {
      if (context->locals[context->localCount - 1].isCaptured) {
        emitByte(&f->chunk, OP_CLOSE_UPVALUE);
      } else {
        emitByte(&f->chunk, OP_POP);
      }
      context->localCount--;
    }
    break;
  }
  case IR_STMT_EXPR: {
    emitByte(&f->chunk, OP_POP);
    break;
  }
  default:
    printf("Unknown opcode %d\n", op->opcode);
  }
}

static void generateBasicBlockCode(Compiler *compiler, ObjFunction *f,
                                   BasicBlock *bb, Table *labels,
                                   ExecutionContext *context) {
  Operation *curr = bb->ops;
  int i = 0;

  while (curr != NULL && i < bb->opsCount) {
    writeOperation(compiler, curr, f, labels, context);
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
        return;
      }
      int offset = (int)AS_NUMBER(location) - index - 2;
      chunk->code[index + 1] = (offset >> 8) & 0xff;
      chunk->code[index + 2] = offset & 0xff;
      index++;
      index++;
    } else if (chunk->code[index] == OP_LOOP) {
      uint8_t hi = chunk->code[index + 1];
      uint8_t lo = chunk->code[index + 2];
      LabelId labelId = (hi << 8) | lo;
      Value location;
      if (!tableGet(labels, NUMBER_VAL(labelId), &location)) {
        // error
        printf("No position found for label %llu\n", labelId);
        return;
      }
      int offset = index - (int)AS_NUMBER(location) + 2;
      chunk->code[index + 1] = (offset >> 8) & 0xff;
      chunk->code[index + 2] = offset & 0xff;
      index++;
      index++;
    } else if (chunk->code[index] == OP_GET_LOCAL ||
               chunk->code[index] == OP_DEFINE_GLOBAL ||
               chunk->code[index] == OP_GET_GLOBAL ||
               chunk->code[index] == OP_SET_GLOBAL ||
               chunk->code[index] == OP_SET_LOCAL ||
               chunk->code[index] == OP_CALL) {
      index++;
    }
    index++;
  }
}

// FIXME: rename
void generateChunk(Compiler *compiler, CFG *cfg, Table *labels,
                   ObjFunction *f) {
  LinkedList *postOrdered = postOrderTraverseBasicBlock(cfg);
  Node *tail = postOrdered->tail;
  while (tail != NULL) {
    generateBasicBlockCode(compiler, f, tail->data, labels, cfg->context);
    tail = tail->prev;
  }

  rewriteLabels(&f->chunk, labels);
}

ObjFunction *compileWorkUnit(Compiler *compiler, WorkUnit *wu, Table *labels) {
  ObjFunction *f = newFunction();
  f->name = copyString(wu->name.start, wu->name.length);
  wu->f = f;

  generateChunk(compiler, wu->cfg, labels, f);
  emitReturn(&f->chunk);

  return f;
}
