#include "codegen.h"
#include "assert.h"
#include "chunk.h"
#include "memory.h"

static void generateBasicBlockCode(Compiler *compiler, ObjFunction *f,
                                   BasicBlock *bb, Table *labels,
                                   ExecutionContext *context);

static void emitByte(Chunk *chunk, uint8_t byte, int lineno) {
  writeChunk(chunk, byte, lineno);
}

static void emitBytes(Chunk *chunk, uint8_t byte1, uint8_t byte2, int lineno) {
  emitByte(chunk, byte1, lineno);
  emitByte(chunk, byte2, lineno);
}

static void emitReturn(FunctionType functionType, Chunk *chunk, int lineno) {
  if (functionType == TYPE_INITIALIZER) {
    emitBytes(chunk, OP_GET_LOCAL, 0, lineno);
  } else {
    emitByte(chunk, OP_NIL, lineno);
  }
  emitByte(chunk, OP_RETURN, lineno);
}

static void emitConstant(Chunk *chunk, uint8_t position, int lineno) {
  emitBytes(chunk, OP_CONSTANT, position, lineno);
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

static int addUpvalue(ExecutionContext *context, uint8_t index, bool isLocal) {
  int upvalueCount = context->upvalueCount;

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
  return context->upvalueCount++;
}

static int resolveUpvalue(ExecutionContext *context, Token *name) {
  if (context->enclosing == NULL) {
    return -1;
  }

  int local = resolveLocal(context->enclosing, name);
  if (local != -1) {
    context->enclosing->locals[local].isCaptured = true;
    return addUpvalue(context, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(context->enclosing, name);
  if (upvalue != -1) {
    return addUpvalue(context, (uint8_t)upvalue, false);
  }

  return -1;
}

static void writeOperation(Compiler *compiler, Operation *op, ObjFunction *f,
                           Table *labels, ExecutionContext *context) {
  switch (op->opcode) {
  case IR_ADD:
    emitByte(&f->chunk, OP_ADD, op->token->line);
    break;
  case IR_CONSTANT: {
    Value literal = op->first->val.literal;
    uint8_t position = identifierConstant(compiler, &f->chunk, literal);
    emitConstant(&f->chunk, position, op->token->line);
    break;
  }
  case IR_CALL: {
    Value arity = op->first->val.literal;

    emitBytes(&f->chunk, OP_CALL, AS_NUMBER(arity), op->token->line);
    break;
  }
  case IR_CODE_START:
    break;
  // FIXME: better names for these two IR instructions
  case IR_COND: {
    // Write label ID into instruction stream and later rewrite symbolic
    // addresses
    emitByte(&f->chunk, OP_JUMP_IF_FALSE, op->token->line);
    emitByte(&f->chunk, (op->second->val.label >> 8) & 0xff, op->token->line);
    emitByte(&f->chunk, op->second->val.label & 0xff, op->token->line);
    emitByte(&f->chunk, OP_POP, op->token->line);
    break;
  }
  case IR_COND_NO_POP: {
    // Write label ID into instruction stream and later rewrite symbolic
    // addresses
    emitByte(&f->chunk, OP_JUMP_IF_FALSE, op->token->line);
    emitByte(&f->chunk, (op->second->val.label >> 8) & 0xff, op->token->line);
    emitByte(&f->chunk, op->second->val.label & 0xff, op->token->line);
    break;
  }
  case IR_GOTO: {
    emitByte(&f->chunk, OP_JUMP, op->token->line);
    emitByte(&f->chunk, (op->first->val.label >> 8) & 0xff, op->token->line);
    emitByte(&f->chunk, op->first->val.label & 0xff, op->token->line);
    break;
  }
  case IR_LOOP: {
    emitByte(&f->chunk, OP_LOOP, op->token->line);
    emitByte(&f->chunk, (op->first->val.label >> 8) & 0xff, op->token->line);
    emitByte(&f->chunk, op->first->val.label & 0xff, op->token->line);
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
    emitByte(&f->chunk, OP_POP, op->token->line);
    break;
  }
  case IR_DIVIDE:
    emitByte(&f->chunk, OP_DIVIDE, op->token->line);
    break;
  case IR_SUBTRACT:
    emitByte(&f->chunk, OP_SUBTRACT, op->token->line);
    break;
  case IR_MODULO:
    emitByte(&f->chunk, OP_MODULO, op->token->line);
    break;
  case IR_MULTIPLY:
    emitByte(&f->chunk, OP_MULTIPLY, op->token->line);
    break;
  case IR_NEGATE:
    emitByte(&f->chunk, OP_NEGATE, op->token->line);
    break;
  case IR_NIL:
    emitByte(&f->chunk, OP_NIL, op->token->line);
    break;
  case IR_NOT:
    emitByte(&f->chunk, OP_NOT, op->token->line);
    break;
  case IR_NOT_EQUAL:
    emitBytes(&f->chunk, OP_EQUAL, OP_NOT, op->token->line);
    break;
  case IR_EQUAL:
    emitByte(&f->chunk, OP_EQUAL, op->token->line);
    break;
  case IR_GREATER:
    emitByte(&f->chunk, OP_GREATER, op->token->line);
    break;
  case IR_GREATER_EQUAL:
    emitBytes(&f->chunk, OP_LESS, OP_NOT, op->token->line);
    break;
  case IR_LESS:
    emitByte(&f->chunk, OP_LESS, op->token->line);
    break;
  case IR_LESS_EQUAL:
    emitBytes(&f->chunk, OP_GREATER, OP_NOT, op->token->line);
    break;
  case IR_PRINT:
    emitByte(&f->chunk, OP_PRINT, op->token->line);
    break;
  case IR_POP:
    emitByte(&f->chunk, OP_POP, op->token->line);
    break;
  case IR_INVOKE: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, &f->chunk, name);

    emitBytes(&f->chunk, OP_INVOKE, position, op->token->line);
    emitByte(&f->chunk, AS_NUMBER(op->second->val.literal), op->token->line);
    break;
  }
  case IR_DEFINE_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value name = OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    uint8_t position = identifierConstant(compiler, &f->chunk, name);

    emitBytes(&f->chunk, OP_DEFINE_GLOBAL, position, op->token->line);
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

    emitBytes(&f->chunk, OP_GET_GLOBAL, position, op->token->line);
    break;
  }
  case IR_GET_LOCAL: {
    Symbol symbol = op->first->val.symbol;

    OpCode opcode = OP_GET_LOCAL;
    int position = resolveLocal(context, &symbol.name);

    if (position == -1) {
      position = resolveUpvalue(context, &symbol.name);
      opcode = OP_GET_UPVALUE;
    }

    assert(position != -1);

    emitBytes(&f->chunk, opcode, (uint8_t)position, op->token->line);
    break;
  }
  case IR_SET_GLOBAL: {
    Symbol symbol = op->first->val.symbol;
    Value nameString =
        OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    // FIXME: not actually used?
    Register source = op->second->val.source;
    uint8_t position = identifierConstant(compiler, &f->chunk, nameString);

    emitBytes(&f->chunk, OP_SET_GLOBAL, position, op->token->line);
    break;
  }
  case IR_SET_LOCAL: {
    Symbol symbol = op->first->val.symbol;

    OpCode opcode = OP_SET_LOCAL;
    int position = resolveLocal(context, &symbol.name);

    if (position == -1) {
      position = resolveUpvalue(context, &symbol.name);
      opcode = OP_SET_UPVALUE;
    }

    assert(position != -1);

    emitBytes(&f->chunk, opcode, (uint8_t)position, op->token->line);
    break;
  }
  case IR_RETURN: {
    emitByte(&f->chunk, OP_RETURN, op->token->line);
    break;
  }
  case IR_RETURN_FROM_INIT: {
    emitBytes(&f->chunk, OP_GET_LOCAL, 0, op->token->line);
    emitByte(&f->chunk, OP_RETURN, op->token->line);
    break;
  }
  case IR_BEGIN_SCOPE: {
    context->scopeDepth++;
    break;
  }
  case IR_CLASS: {
    Value wuPtr = op->first->val.literal;
    WorkUnit *wu = AS_POINTER(wuPtr);

    /* ObjFunction *childF = compileWorkUnit(compiler, wu, labels); */
    ObjString *name = copyString(wu->name.start, wu->name.length);
    /* childF->name = name; */
    int position = identifierConstant(compiler, &f->chunk, OBJ_VAL(name));
    emitBytes(&f->chunk, OP_CLASS, position, op->token->line);

    if (context->scopeDepth == 0) {
      int namePosition = identifierConstant(compiler, &f->chunk, OBJ_VAL(name));
      emitBytes(&f->chunk, OP_DEFINE_GLOBAL, namePosition, op->token->line);
      emitBytes(&f->chunk, OP_GET_GLOBAL, namePosition, op->token->line);
    } else {
      int position = context->localCount;
      Local *local = &context->locals[context->localCount++];
      local->name = wu->name;
      local->depth = context->scopeDepth;
      local->isCaptured = false;
      emitBytes(&f->chunk, OP_GET_LOCAL, (uint8_t)position, op->token->line);
    }

    LinkedList *postOrdered = postOrderTraverseBasicBlock(wu->cfg);
    Node *tail = postOrdered->tail;
    while (tail != NULL) {
      generateBasicBlockCode(compiler, f, tail->data, labels, wu->cfg->context);
      tail = tail->prev;
    }
    wu->f = NULL; // TODO: document/clarify

    emitByte(&f->chunk, OP_POP, op->token->line);
    break;
  }
  case IR_METHOD: {
    // FIXME: fold into IR_FUNCTION
    Value wuPtr = op->first->val.literal;
    WorkUnit *wu = AS_POINTER(wuPtr);
    ObjFunction *childF = compileWorkUnit(compiler, wu, labels);

    // Test fix hack
    // Need to rethink where to store upvalues
    childF->upvalueCount = wu->cfg->context->upvalueCount;
    childF->arity = wu->cfg->arity;
    int position = makeConstant(compiler, &f->chunk, OBJ_VAL(childF));

    emitBytes(&f->chunk, OP_CLOSURE, position, op->token->line);
    // FIXME: I don't think this works for the general case
    // Need to look in parent contexts too?
    for (int i = 0; i < wu->cfg->context->upvalueCount; i++) {
      emitByte(&f->chunk, context->upvalues[i].isLocal ? 1 : 0,
               op->token->line);
      emitByte(&f->chunk, context->upvalues[i].index, op->token->line);
    }
    int namePosition =
        identifierConstant(compiler, &f->chunk, OBJ_VAL(childF->name));
    emitBytes(&f->chunk, OP_METHOD, namePosition, op->token->line);
    break;
  }
  case IR_FUNCTION: {
    Value wuPtr = op->first->val.literal;
    WorkUnit *wu = AS_POINTER(wuPtr);
    ObjFunction *childF = compileWorkUnit(compiler, wu, labels);

    // Test fix hack
    // Need to rethink where to store upvalues
    childF->upvalueCount = wu->cfg->context->upvalueCount;
    childF->arity = wu->cfg->arity;
    int position = makeConstant(compiler, &f->chunk, OBJ_VAL(childF));

    // FIXME: handle methods and initialisers
    emitBytes(&f->chunk, OP_CLOSURE, position, op->token->line);
    for (int i = 0; i < wu->cfg->context->upvalueCount; i++) {
      emitByte(&f->chunk, wu->cfg->context->upvalues[i].isLocal ? 1 : 0,
               op->token->line);
      emitByte(&f->chunk, wu->cfg->context->upvalues[i].index, op->token->line);
    }

    // Fixes issue when using scopes at top level. Do we need both checks here?
    // Test fix hack
    if (context->enclosing == NULL && context->scopeDepth == 0) {
      int namePosition =
          identifierConstant(compiler, &f->chunk, OBJ_VAL(childF->name));
      emitBytes(&f->chunk, OP_DEFINE_GLOBAL, namePosition, op->token->line);
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
        emitByte(&f->chunk, OP_CLOSE_UPVALUE, op->token->line);
      } else {
        emitByte(&f->chunk, OP_POP, op->token->line);
      }
      context->localCount--;
    }
    break;
  }
  case IR_STMT_EXPR: {
    emitByte(&f->chunk, OP_POP, op->token->line);
    break;
  }
  case IR_SET_PROPERTY: {
    Symbol symbol = op->first->val.symbol;
    Value nameString =
        OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    int namePosition = identifierConstant(compiler, &f->chunk, nameString);
    emitBytes(&f->chunk, OP_SET_PROPERTY, namePosition, op->token->line);
    break;
  }
  case IR_GET_PROPERTY: {
    Symbol symbol = op->first->val.symbol;
    Value nameString =
        OBJ_VAL(copyString(symbol.name.start, symbol.name.length));
    int namePosition = identifierConstant(compiler, &f->chunk, nameString);
    emitBytes(&f->chunk, OP_GET_PROPERTY, namePosition, op->token->line);
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
    switch (chunk->code[index]) {
    case OP_CONSTANT_LONG: {
      index += 4;
      break;
    }
    case OP_JUMP:
    case OP_JUMP_IF_FALSE: {
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
      index += 3;
      break;
    }
    case OP_LOOP: {
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
      index += 3;
      break;
    }
    case OP_INVOKE: {
      index += 3;
      break;
    }
    case OP_CLOSURE: {
      index++;
      uint8_t constant = chunk->code[index++];
      ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
      for (int j = 0; j < function->upvalueCount; j++) {
        index += 2;
      }
      break;
    }
    case OP_ARRAY:
    case OP_DEFINE_GLOBAL:
    case OP_METHOD:
    case OP_GET_LOCAL:
    case OP_SET_LOCAL:
    case OP_GET_UPVALUE:
    case OP_SET_UPVALUE:
    case OP_GET_GLOBAL:
    case OP_SET_GLOBAL:
    case OP_GET_PROPERTY:
    case OP_SET_PROPERTY:
    case OP_GET_SUPER:
    case OP_SUPER_INVOKE:
    case OP_CONSTANT:
    case OP_CLASS:
    case OP_CALL: {
      index += 2;
      break;
    }
    default: {
      index += 1;
    }
    }
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
  // FIXME: lineno is wrong
  emitReturn(wu->node->functionType, &f->chunk, wu->name.line);

  return f;
}
