#include "cfg.h"
#include "compiler.h"
#include "execution_context.h"
#include "memory.h"
#include "scanner.h"
#include "util.h"

#include <assert.h>
#include <stdlib.h>

static IRList *ir_create(AstNode *node);

// TODO: make thread safe
static Register currentRegister = 1;
static BasicBlockId currentBasicBlockId = 0;
static OperationId currentOperationId = 1;
static LabelId currentLabelId = 0;

static Register getRegister() { return currentRegister++; }
static BasicBlockId getBasicBlockId() { return currentBasicBlockId++; }
static OperationId getOperationId() { return currentOperationId++; }
static LabelId getLabelId() { return currentLabelId++; }

static void cfg_construct(Compiler *compiler, WorkUnit *wu, Scope *scope,
                          Token name);
static Operation *ast_walk(Compiler *compiler, IRList *ir, AstNode *node,
                           Scope *activeScope, WorkUnit *activeWorkUnit);

static WorkUnit *wu_allocate(ExecutionContext *ec, AstNode *node, Token name,
                             FunctionType functionType) {
  WorkUnit *wu = (WorkUnit *)reallocate(NULL, 0, sizeof(WorkUnit));
  wu->activeContext = ec_allocate();
  wu->activeContext->enclosing = ec;
  wu->childWorkUnits = linkedList_allocate();
  wu->node = node;
  wu->cfg = NULL;
  wu->f = NULL;
  wu->name = name;
  wu->functionType = functionType;
  return wu;
}

static IRList *ir_allocate() {
  IRList *ir = (IRList *)reallocate(NULL, 0, sizeof(IRList));
  ir->ops = linkedList_allocate();

  return ir;
}

static BasicBlock *bb_allocate() {
  BasicBlock *bb = (BasicBlock *)reallocate(NULL, 0, sizeof(BasicBlock));
  bb->id = getBasicBlockId();
  for (int i = 0; i < EDGE_COUNT; i++) {
    bb->edges[i] = NULL;
  }
  bb->ir = ir_allocate();

  return bb;
}

static CFG *cfg_allocate(Token name) {
  CFG *cfg = (CFG *)reallocate(NULL, 0, sizeof(CFG));
  cfg->start = NULL;
  cfg->name = name;

  return cfg;
}

static void ir_write(IRList *ir, Operation *op) {
  linkedList_append(ir->ops, op);
}

static int ir_length(IRList *ir) { return ir->ops->length; }

static void bb_write(BasicBlock *bb, Operation *op) { ir_write(bb->ir, op); }

static int bb_ops_count(BasicBlock *bb) { return ir_length(bb->ir); }

static BasicBlock *bb_fetch_or_allocate(Table *labels, LabelId id) {
  Value pointer;
  BasicBlock *nextBasicBlock = NULL;
  if (tableGet(labels, NUMBER_VAL(id), &pointer)) {
    nextBasicBlock = AS_POINTER(pointer);
  } else {
    nextBasicBlock = bb_allocate();
    tableSet(labels, NUMBER_VAL(id), POINTER_VAL(nextBasicBlock));
  }
  return nextBasicBlock;
}

// TODO: Return CFG?
static BasicBlock *ir_split(IRList *ir, Table *labels) {
  BasicBlock *root = bb_allocate();

  BasicBlock *currentBasicBlock = root;
  Node *irListNode = ir->ops->head;

  while (irListNode != NULL) {
    Operation *currentOp = irListNode->data;

    switch (currentOp->opcode) {
    case IR_LABEL:
    case IR_ELSE_LABEL: {
      LabelId labelId = currentOp->first->val.label;
      BasicBlock *next = bb_fetch_or_allocate(labels, labelId);

      currentBasicBlock->edges[0] = next;
      currentBasicBlock = next;
      break;
    }
    case IR_COND: {
      Operation *ifBranchNode = (Operation *)irListNode->next->data;
      // We assume that instructions using COND have been written so that
      // every branch starts with a label
      assert(ifBranchNode->opcode == IR_LABEL);

      LabelId ifLabelId = ifBranchNode->first->val.label;
      BasicBlock *ifBasicBlock = bb_fetch_or_allocate(labels, ifLabelId);

      LabelId elseLabelId = currentOp->second->val.label;
      BasicBlock *elseBasicBlock = bb_fetch_or_allocate(labels, elseLabelId);

      currentBasicBlock->edges[0] = ifBasicBlock;
      currentBasicBlock->edges[1] = elseBasicBlock;
      break;
    }
    case IR_GOTO: {
      LabelId labelId = currentOp->first->val.label;
      BasicBlock *next = bb_fetch_or_allocate(labels, labelId);

      currentBasicBlock->edges[0] = next;
      break;
    }
    default: {
    }
    }
    bb_write(currentBasicBlock, currentOp);
    irListNode = irListNode->next;
  }

  return root;
}

static void cfg_construct(Compiler *compiler, WorkUnit *wu, Scope *scope,
                          Token name) {
  IRList *ir = ir_create(wu->node);

  wu->cfg = cfg_allocate(name);
  // generate flat IR list by walking AST
  ast_walk(compiler, ir, wu->node, scope, wu);
  // split IR list into CFG
  wu->cfg->start = ir_split(ir, compiler->labels);
}

static WorkUnit *wu_construct(Compiler *compiler, ExecutionContext *ec,
                              AstNode *node, Scope *scope, Token name,
                              FunctionType functionType) {
  WorkUnit *wu = wu_allocate(ec, node, name, functionType);
  cfg_construct(compiler, wu, scope, name);
  return wu;
}

static Operation *operation_allocate() {
  Operation *op = (Operation *)reallocate(NULL, 0, sizeof(Operation));
  op->id = 0;
  op->token = NULL;
  op->first = NULL;
  op->second = NULL;

  return op;
}

static Operand *operand_allocate(OperandType type) {
  Operand *operand = (Operand *)reallocate(NULL, 0, sizeof(Operand));
  operand->type = type;

  return operand;
}

static IROp tokenToBinaryOp(TokenType token) {
  switch (token) {
  case TOKEN_BANG:
    return IR_NOT;
  case TOKEN_BANG_EQUAL:
    return IR_NOT_EQUAL;
  case TOKEN_EQUAL_EQUAL:
    return IR_EQUAL;
  case TOKEN_GREATER:
    return IR_GREATER;
  case TOKEN_GREATER_EQUAL:
    return IR_GREATER_EQUAL;
  case TOKEN_LESS:
    return IR_LESS;
  case TOKEN_LESS_EQUAL:
    return IR_LESS_EQUAL;
  case TOKEN_MINUS:
    return IR_SUBTRACT;
  case TOKEN_PERCENT:
    return IR_MODULO;
  case TOKEN_PLUS:
    return IR_ADD;
  case TOKEN_SLASH:
    return IR_DIVIDE;
  case TOKEN_STAR:
    return IR_MULTIPLY;
  default:
    return IR_UNKNOWN;
  }
}

static IROp tokenToUnaryOp(TokenType token) {
  switch (token) {
  case TOKEN_MINUS:
    return IR_NEGATE;
  case TOKEN_BANG:
    return IR_NOT;
  default:
    return IR_UNKNOWN;
  }
}

static Operand *literal_operand_create(Value value) {
  Operand *operand = operand_allocate(OPERAND_LITERAL);
  operand->val.literal = value;

  return operand;
}

// TODO: cache operands rather than recreating
static Operand *register_operand_create(Register reg) {
  Operand *operand = operand_allocate(OPERAND_REG);
  operand->val.source = reg;

  return operand;
}

static Operand *label_operand_create(LabelId id) {
  Operand *operand = operand_allocate(OPERAND_LABEL);
  operand->val.label = id;

  return operand;
}

static Operand *symbol_operand_create(Symbol *symbol) {
  Operand *operand = operand_allocate(OPERAND_SYMBOL);
  operand->val.symbol = symbol;

  return operand;
}

static Operand *token_operand_create(Token token) {
  Operand *operand = operand_allocate(OPERAND_TOKEN);
  operand->val.token = token;

  return operand;
}

// Make a separate function for statements that doesn't output to register?
Operation *operation_create(Token *token, IROp opcode, Operand *first,
                            Operand *second) {
  Operation *op = operation_allocate();

  op->destination = getRegister();
  op->token = token;
  op->id = getOperationId();
  op->opcode = opcode;
  op->first = first;
  op->second = second;

  return op;
}

static Operation *start_op_create() {
  Operation *op = operation_allocate();

  op->destination = 0;
  op->opcode = IR_CODE_START;

  return op;
}

static Operation *goto_op_create(Token *token, LabelId labelId) {
  Operation *op = operation_allocate();

  op->id = getOperationId();
  op->opcode = IR_GOTO;
  op->first = label_operand_create(labelId);
  op->token = token;

  return op;
}

static Operation *label_op_create(Token *token, LabelId labelId, IROp opcode) {
  Operation *op = operation_allocate();

  op->id = getOperationId();
  op->opcode = opcode;
  op->first = label_operand_create(labelId);
  op->token = token;

  return op;
}

static int walkParams(Compiler *compiler, IRList *ir, AstNode *node,
                      WorkUnit *activeWorkUnit, Scope *activeScope,
                      const LinkedList *params) {
  int arity = 0;
  Node *paramNode = params->head;
  while (paramNode != NULL) {
    arity++;
    ast_walk(compiler, ir, paramNode->data,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
    paramNode = paramNode->next;
  }
  return arity;
}

static Operation *ast_walk(Compiler *compiler, IRList *ir, AstNode *node,
                           Scope *activeScope, WorkUnit *activeWorkUnit) {
  assert(activeScope != NULL);
  if (node == NULL) {
    return NULL;
  }

  Operation *op = NULL;

  switch (node->type) {
  case EXPR_LITERAL: {
    Operand *constantValue =
        literal_operand_create(AS_LITERAL_EXPR(node)->literal);

    op = operation_create(&node->token, IR_CONSTANT, constantValue, NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_NIL: {
    op = operation_create(&node->token, IR_NIL, NULL, NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_UNARY: {
    UnaryExprAstNode *expr = AS_UNARY_EXPR(node);
    Operation *right =
        ast_walk(compiler, ir, expr->right, activeScope, activeWorkUnit);
    Operand *value = register_operand_create(right->destination);

    op = operation_create(&node->token, tokenToUnaryOp(expr->op), value, NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_BINARY: {
    BinaryExprAstNode *expr = AS_BINARY_EXPR(node);

    Operation *left = ast_walk(compiler, ir, expr->branches.left, activeScope,
                               activeWorkUnit);
    if (left == NULL) {
      errorAt(compiler, &node->token,
              "Failed to build left branch of binary expression.");
      break;
    }

    Operation *right = ast_walk(compiler, ir, expr->branches.right, activeScope,
                                activeWorkUnit);
    if (right == NULL) {
      errorAt(compiler, &node->token,
              "Failed to build right branch of binary expression.");
      break;
    }

    op = operation_create(&node->token, tokenToBinaryOp(expr->op),
                          register_operand_create(left->destination),
                          register_operand_create(right->destination));
    ir_write(ir, op);
    break;
  }
  case EXPR_AND: {
    AndExprAstNode *expr = AS_AND_EXPR(node);
    LabelId afterLabelId = getLabelId();
    LabelId trueLabelId = getLabelId();

    Operation *left = ast_walk(compiler, ir, expr->branches.left, activeScope,
                               activeWorkUnit);

    op = operation_create(&node->token, IR_COND,
                          register_operand_create(left->destination),
                          label_operand_create(afterLabelId));
    ir_write(ir, op);

    op = label_op_create(&node->token, trueLabelId, IR_LABEL);
    ir_write(ir, op);

    op = ast_walk(compiler, ir, expr->branches.right, activeScope,
                  activeWorkUnit);
    ir_write(ir, op);

    op = label_op_create(&node->token, afterLabelId, IR_LABEL);
    ir_write(ir, op);

    break;
  }
  case EXPR_OR: {
    OrExprAstNode *expr = AS_OR_EXPR(node);
    LabelId afterLabelId = getLabelId();
    LabelId elseLabelId = getLabelId();

    Operation *left = ast_walk(compiler, ir, expr->branches.left, activeScope,
                               activeWorkUnit);

    op = operation_create(&node->token, IR_COND_NO_POP,
                          register_operand_create(left->destination),
                          label_operand_create(elseLabelId));
    ir_write(ir, op);

    op = goto_op_create(&node->token, afterLabelId);
    ir_write(ir, op);

    op = label_op_create(&node->token, elseLabelId, IR_LABEL);
    ir_write(ir, op);

    // FIXME: pop shouldn't live in IR
    op = operation_create(&node->token, IR_POP, NULL, NULL);
    ir_write(ir, op);

    Operation *right = ast_walk(compiler, ir, expr->branches.right, activeScope,
                                activeWorkUnit);

    op = label_op_create(&node->token, afterLabelId, IR_LABEL);
    ir_write(ir, op);

    break;
  }
  case EXPR_VARIABLE: {
    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "VarExpr: Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol->type == SCOPE_GLOBAL ? IR_GET_GLOBAL : IR_GET_LOCAL;

    op = operation_create(&node->token, opcode,
                          token_operand_create(node->token), NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_THIS: {
    op = operation_create(&node->token, IR_GET_LOCAL,
                          token_operand_create(syntheticToken("this")), NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_PRINT: {
    PrintStmtAstNode *stmt = AS_PRINT_STMT(node);

    op = ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);
    Operand *value = register_operand_create(op->destination);

    op = operation_create(&node->token, IR_PRINT, value, NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_IF: {
    IfStmtAstNode *stmt = AS_IF_STMT(node);
    LabelId thenLabelId = getLabelId();
    LabelId elseLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();
    bool elseBranchPresent = stmt->branches.elseB != NULL;

    op = operation_create(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    ir_write(ir, op);

    Operation *expr =
        ast_walk(compiler, ir, stmt->condition, activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_COND,
                          register_operand_create(expr->destination),
                          label_operand_create(elseLabelId));
    ir_write(ir, op);

    op = label_op_create(&node->token, thenLabelId, IR_LABEL);
    ir_write(ir, op);

    ast_walk(compiler, ir, stmt->branches.then, activeScope, activeWorkUnit);

    op = goto_op_create(&node->token, afterLabelId);
    ir_write(ir, op);

    op = label_op_create(&node->token, elseLabelId, IR_LABEL);
    ir_write(ir, op);

    // FIXME: remove pop from IR
    op = operation_create(&node->token, IR_POP, NULL, NULL);
    ir_write(ir, op);

    if (elseBranchPresent) {
      ast_walk(compiler, ir, stmt->branches.elseB, activeScope, activeWorkUnit);
    }

    op = label_op_create(&node->token, afterLabelId, IR_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_END_SCOPE, NULL, NULL);
    ir_write(ir, op);

    break;
  }
  case STMT_WHILE: {
    WhileStmtAstNode *stmt = AS_WHILE_STMT(node);
    LabelId exprLabelId = getLabelId();
    LabelId ifLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();

    op = label_op_create(&node->token, exprLabelId, IR_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    ir_write(ir, op);

    Operation *expr =
        ast_walk(compiler, ir, stmt->condition, activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_COND,
                          register_operand_create(expr->destination),
                          label_operand_create(afterLabelId));
    ir_write(ir, op);

    op = label_op_create(&node->token, ifLabelId, IR_LABEL);
    ir_write(ir, op);

    ast_walk(compiler, ir, stmt->branches.then, activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_LOOP,
                          label_operand_create(exprLabelId), NULL);
    ir_write(ir, op);

    op = label_op_create(&node->token, afterLabelId, IR_ELSE_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_END_SCOPE, NULL, NULL);
    ir_write(ir, op);

    break;
  }
  case STMT_FOR: {
    ForStmtAstNode *stmt = AS_FOR_STMT(node);
    LabelId preLabelId = getLabelId();
    LabelId condLabelId = getLabelId();
    LabelId bodyLabelId = getLabelId();
    LabelId postLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();
    LabelId loopLabelId = getLabelId();

    op = label_op_create(&node->token, preLabelId, IR_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    ir_write(ir, op);

    if (stmt->branches.pre != NULL) {
      ast_walk(compiler, ir, stmt->branches.pre,
               node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
    }

    op = label_op_create(&node->token, condLabelId, IR_LABEL);
    ir_write(ir, op);

    if (stmt->branches.cond != NULL) {
      Operation *condExpr = ast_walk(
          compiler, ir, stmt->branches.cond,
          node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
      op = operation_create(&node->token, IR_COND,
                            register_operand_create(condExpr->destination),
                            label_operand_create(afterLabelId));
      ir_write(ir, op);
    }

    op = label_op_create(&node->token, bodyLabelId, IR_LABEL);
    ir_write(ir, op);

    ast_walk(compiler, ir, stmt->branches.body,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);

    op = goto_op_create(&node->token, loopLabelId);
    ir_write(ir, op);

    if (stmt->branches.post != NULL) {
      op = label_op_create(&node->token, postLabelId, IR_LABEL);
      ir_write(ir, op);

      ast_walk(compiler, ir, stmt->branches.post,
               node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
      // FIXME: don't like having POP in IR
      op = operation_create(&node->token, IR_POP, NULL, NULL);
      ir_write(ir, op);

      op = operation_create(&node->token, IR_LOOP,
                            label_operand_create(condLabelId), NULL);
      ir_write(ir, op);
    }

    op = label_op_create(&node->token, loopLabelId, IR_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_LOOP,
                          label_operand_create(stmt->branches.post != NULL
                                                   ? postLabelId
                                                   : condLabelId),
                          NULL);
    ir_write(ir, op);

    op =
        label_op_create(&node->token, afterLabelId,
                        stmt->branches.cond != NULL ? IR_ELSE_LABEL : IR_LABEL);
    ir_write(ir, op);

    op = operation_create(&node->token, IR_END_SCOPE, NULL, NULL);
    ir_write(ir, op);

    break;
  }
  case STMT_DEFINE: {
    DefineStmtAstNode *stmt = AS_DEFINE_STMT(node);
    if (stmt->expr == NULL) {
      op = operation_create(&node->token, IR_NIL, NULL, NULL);
      ir_write(ir, op);
    } else {
      op = ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);
    }

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "DefineStmt: Symbol is not defined in current scope.");
      break;
    }

    IROp opcode =
        symbol->type == SCOPE_GLOBAL ? IR_DEFINE_GLOBAL : IR_DEFINE_LOCAL;

    op = operation_create(&node->token, opcode, symbol_operand_create(symbol),
                          NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_ASSIGN: {
    AssignStmtAstNode *stmt = AS_ASSIGN_STMT(node);
    op = ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "AssignStmt: Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol->type == SCOPE_GLOBAL ? IR_SET_GLOBAL : IR_SET_LOCAL;

    op = operation_create(&node->token, opcode,
                          token_operand_create(node->token),
                          register_operand_create(op->destination));
    ir_write(ir, op);
    break;
  }
  case STMT_MODULE: {
    ModuleStmtAstNode *stmt = AS_MODULE_STMT(node);
    Node *stmtNode = (Node *)stmt->stmts->head;

    while (stmtNode != NULL) {
      ast_walk(compiler, ir, stmtNode->data, activeScope, activeWorkUnit);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    BlockStmtAstNode *stmt = AS_BLOCK_STMT(node);
    op = operation_create(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    ir_write(ir, op);

    Node *blockNode = (Node *)stmt->stmts->head;

    while (blockNode != NULL) {
      ast_walk(compiler, ir, blockNode->data,
               node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
      blockNode = blockNode->next;
    }

    op = operation_create(&node->token, IR_END_SCOPE, NULL, NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_FUNCTION: {
    FunctionStmtAstNode *stmt = AS_FUNCTION_STMT(node);
    AstNode *body = AS_AST_NODE(stmt->expr);

    WorkUnit *wu =
        wu_construct(compiler, activeWorkUnit->activeContext, body, body->scope,
                     node->token, AS_FUNCTION_EXPR(stmt->expr)->functionType);
    linkedList_append(activeWorkUnit->childWorkUnits, wu);

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "FuncStmt: Symbol is not defined in current scope.");
      break;
    }
    Operand *pointer = literal_operand_create(POINTER_VAL(wu));
    Operand *arityOperand = literal_operand_create(NUMBER_VAL(symbol->arity));
    op = operation_create(&node->token, IR_FUNCTION, pointer, arityOperand);
    ir_write(ir, op);
    break;
  }
  case EXPR_FUNCTION: {
    FunctionExprAstNode *expr = AS_FUNCTION_EXPR(node);

    for (int arity = 0; arity < expr->arity; arity++) {
      Symbol *symbol = NULL;
      Token name = expr->params[arity];
      if (!scope_search(node->scope, name.start, name.length, &symbol)) {
        errorAt(compiler, &name,
                "FuncExpr: Symbol is not defined in current scope.");
        break;
      }
      Operand *pointer = literal_operand_create(POINTER_VAL(node->scope));
      op = operation_create(&node->token, IR_DEFINE_LOCAL,
                            symbol_operand_create(symbol), pointer);
      ir_write(ir, op);
    }

    op = operation_create(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    ir_write(ir, op);

    BlockStmtAstNode *blockStmt = expr->body;
    Node *blockNode = (Node *)blockStmt->stmts->head;

    while (blockNode != NULL) {
      ast_walk(compiler, ir, blockNode->data, AS_AST_NODE(expr->body)->scope,
               activeWorkUnit);
      blockNode = blockNode->next;
    }
    break;
  }
  case STMT_RETURN: {
    ReturnStmtAstNode *stmt = AS_RETURN_STMT(node);
    if (activeScope->type == TYPE_INITIALIZER) {
      op = operation_create(&node->token, IR_RETURN_FROM_INIT, NULL, NULL);
      ir_write(ir, op);
      break;
    }
    Operation *expr = NULL;
    if (stmt->expr == NULL) {
      expr = operation_create(&node->token, IR_NIL, NULL, NULL);
      ir_write(ir, expr);
    } else {
      expr = ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);
    }
    op = operation_create(&node->token, IR_RETURN,
                          register_operand_create(expr->destination), NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_EXPR: {
    ExprStmtAstNode *stmt = AS_EXPR_STMT(node);

    ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_STMT_EXPR, NULL, NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_GET_PROPERTY: {
    ast_walk(compiler, ir, AS_GET_PROPERTY_EXPR(node)->target, activeScope,
             activeWorkUnit);

    op = operation_create(&node->token, IR_GET_PROPERTY,
                          token_operand_create(node->token), NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_SET_PROPERTY: {
    SetPropertyStmtAstNode *stmt = AS_SET_PROPERTY_STMT(node);

    ast_walk(compiler, ir, stmt->target, activeScope, activeWorkUnit);
    ast_walk(compiler, ir, stmt->expr, activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_SET_PROPERTY,
                          token_operand_create(node->token), NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_CALL: {
    CallExprAstNode *expr = AS_CALL_EXPR(node);
    ast_walk(compiler, ir, expr->target,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);

    int arity = walkParams(compiler, ir, node, activeWorkUnit, activeScope,
                           expr->params);
    Operand *arityOperand = literal_operand_create(NUMBER_VAL(arity));

    op = operation_create(&node->token, IR_CALL, arityOperand, NULL);
    ir_write(ir, op);
    break;
  }
  case EXPR_INVOKE: {
    InvocationExprAstNode *expr = AS_INVOCATION_EXPR(node);
    ast_walk(compiler, ir, expr->target,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);

    int arity = walkParams(compiler, ir, node, activeWorkUnit, activeScope,
                           expr->params);
    Operand *arityOperand = literal_operand_create(NUMBER_VAL(arity));

    op = operation_create(&node->token, IR_INVOKE,
                          token_operand_create(node->token), arityOperand);
    ir_write(ir, op);
    break;
  }
  case EXPR_SUPER_INVOKE: {
    SuperInvocationExprAstNode *expr = AS_SUPER_INVOCATION_EXPR(node);
    op = operation_create(&node->token, IR_GET_LOCAL,
                          token_operand_create(syntheticToken("this")), NULL);
    ir_write(ir, op);

    ast_walk(compiler, ir, expr->target,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);

    int arity = walkParams(compiler, ir, node, activeWorkUnit, activeScope,
                           expr->params);
    Operand *arityOperand = literal_operand_create(NUMBER_VAL(arity));

    op = operation_create(&node->token, IR_SUPER_INVOKE,
                          token_operand_create(node->token), arityOperand);
    ir_write(ir, op);
    break;
  }
  case EXPR_SUPER: {
    SuperExprAstNode *expr = AS_SUPER_EXPR(node);
    Token this = syntheticToken("this");

    op = operation_create(&node->token, IR_GET_LOCAL,
                          token_operand_create(this), NULL);
    ir_write(ir, op);

    ast_walk(compiler, ir, expr->target,
             node->scope != NULL ? node->scope : activeScope, activeWorkUnit);

    op = operation_create(&node->token, IR_SUPER,
                          token_operand_create(node->token), NULL);
    ir_write(ir, op);
    break;
  }
  case STMT_CLASS: {
    ClassStmtAstNode *stmt = AS_CLASS_STMT(node);

    WorkUnit *wu = wu_construct(compiler, activeWorkUnit->activeContext,
                                AS_AST_NODE(stmt->body),
                                node->scope != NULL ? node->scope : activeScope,
                                node->token, TYPE_CLASS);
    linkedList_append(activeWorkUnit->childWorkUnits, wu);

    Operand *pointer = literal_operand_create(POINTER_VAL(wu));
    if (OPTIONAL_HAS_VALUE(stmt->superclass)) {
      op = operation_create(
          &node->token, IR_CLASS, pointer,
          token_operand_create(OPTIONAL_VALUE(stmt->superclass)));
    } else {
      op = operation_create(&node->token, IR_CLASS, pointer, NULL);
    }
    ir_write(ir, op);

    break;
  }
  case STMT_CLASS_BODY: {
    ClassBodyStmtAstNode *stmt = AS_CLASS_BODY_STMT(node);
    Node *methodNode = (Node *)stmt->stmts->head;
    while (methodNode != NULL) {
      ast_walk(compiler, ir, methodNode->data,
               node->scope != NULL ? node->scope : activeScope, activeWorkUnit);
      methodNode = methodNode->next;
    }
    break;
  }
  case STMT_METHOD: {
    MethodStmtAstNode *stmt = AS_METHOD_STMT(node);
    AstNode *body = AS_AST_NODE(stmt->body);

    WorkUnit *wu =
        wu_construct(compiler, activeWorkUnit->activeContext, body, body->scope,
                     node->token, AS_FUNCTION_EXPR(stmt->body)->functionType);
    linkedList_append(activeWorkUnit->childWorkUnits, wu);

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "MethodStmt: Symbol is not defined in current scope.");
      break;
    }
    Operand *pointer = literal_operand_create(POINTER_VAL(wu));
    Operand *arityOperand = literal_operand_create(NUMBER_VAL(symbol->arity));
    op = operation_create(&node->token, IR_METHOD, pointer, arityOperand);
    ir_write(ir, op);
    break;
  }
  }
  return op;
}

static IRList *ir_create(AstNode *node) {
  if (node == NULL) {
    return NULL;
  }
  IRList *ir = ir_allocate();
  Operation *start = start_op_create();
  ir_write(ir, start);

  return ir;
}

char *operandString(Operand *operand) {
  if (operand == NULL) {
    return "";
  }
  switch (operand->type) {
  case OPERAND_LITERAL: {
    return writeValue(operand->val.literal);
  }
  case OPERAND_REG: {
    int length = snprintf(NULL, 0, "t%llu", operand->val.source);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "t%llu", operand->val.source);
    return str;
  }
  case OPERAND_LABEL: {

    int length = snprintf(NULL, 0, "L%llu", operand->val.label);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "L%llu", operand->val.label);
    return str;
  }
  case OPERAND_SYMBOL: {
    Token name = operand->val.symbol->name;
    int length = snprintf(NULL, 0, "%.*s", name.length, name.start);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "%.*s", name.length, name.start);
    return str;
  }
  case OPERAND_TOKEN: {
    Token name = operand->val.token;
    int length = snprintf(NULL, 0, "%.*s", name.length, name.start);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "%.*s", name.length, name.start);
    return str;
  }
  }
}

char *opcodeString(IROp opcode) {
  switch (opcode) {
  case IR_ADD:
    return "+";
  case IR_CALL:
    return "call";
  case IR_CLASS:
    return "class";
  case IR_METHOD:
    return "method";
  case IR_CONSTANT:
    return "constant";
  case IR_CODE_START:
    return "<start>";
  case IR_COND:
    return "cond";
  case IR_COND_NO_POP:
    return "cond_np";
  case IR_DIVIDE:
    return "/";
  case IR_EQUAL:
    return "==";
  case IR_GREATER:
    return ">";
  case IR_GREATER_EQUAL:
    return ">=";
  case IR_LESS:
    return "<";
  case IR_LESS_EQUAL:
    return "<=";
  case IR_NOT:
    return "!";
  case IR_NOT_EQUAL:
    return "!=";
  case IR_MODULO:
    return "%";
  case IR_MULTIPLY:
    return "*";
  case IR_NEGATE:
    return "-";
  case IR_NIL:
    return "nil";
  case IR_PRINT:
    return "print";
  case IR_GOTO:
    return "goto";
  case IR_LABEL:
    return "label";
  case IR_LOOP:
    return "loop";
  case IR_DEFINE_LOCAL:
    return "local_define";
  case IR_DEFINE_GLOBAL:
    return "global_define";
  case IR_GET_GLOBAL:
    return "global_var";
  case IR_SET_GLOBAL:
    return "global_assign";
  case IR_GET_LOCAL:
    return "local_var";
  case IR_SET_LOCAL:
    return "local_assign";
  case IR_GET_PROPERTY:
    return "get_prop";
  case IR_SET_PROPERTY:
    return "set_prop";
  case IR_POP:
    return "pop";
  case IR_RETURN:
    return "return_init";
  case IR_RETURN_FROM_INIT:
    return "return";
  case IR_BEGIN_SCOPE:
    return "begin_scope";
  case IR_END_SCOPE:
    return "end_scope";
  case IR_FUNCTION:
    return "function";
  case IR_SUBTRACT:
    return "-";
  case IR_STMT_EXPR:
    return "stmt_expr";
  case IR_INVOKE:
    return "invoke";
  case IR_SUPER_INVOKE:
    return "super_invoke";
  case IR_SUPER:
    return "super";
  case IR_UNKNOWN:
  default:
    return "?";
  }
}

void bb_walk_dfs(BasicBlock *bb, Table *visitedSet, LinkedList *ordered) {
  tableSet(visitedSet, NUMBER_VAL(bb->id), TRUE_VAL);
  Value unused;
  if (bb->edges[0] != NULL) {
    if (!tableGet(visitedSet, NUMBER_VAL(bb->edges[0]->id), &unused)) {
      bb_walk_dfs(bb->edges[0], visitedSet, ordered);
    }
  }
  if (bb->edges[1] != NULL) {
    if (!tableGet(visitedSet, NUMBER_VAL(bb->edges[1]->id), &unused)) {
      bb_walk_dfs(bb->edges[1], visitedSet, ordered);
    }
  }
  linkedList_append(ordered, bb);
}

LinkedList *cfg_post_order_traverse(CFG *cfg) {
  Table visitedSet;
  initTable(&visitedSet);

  LinkedList *ordered = linkedList_allocate();

  bb_walk_dfs(cfg->start, &visitedSet, ordered);

  freeTable(&visitedSet);

  return ordered;
}

// TODO: Free strings from writeString
void bb_print(BasicBlock *bb) {
  int opcount = ir_length(bb->ir);

  printf("BB #%llu", bb->id);
  printf(" | Edge(s): ");
  for (int i = 0; i < EDGE_COUNT; i++) {
    if (bb->edges[i] == NULL) {
      printf("_ ");
    } else {
      printf("B%llu ", bb->edges[i]->id);
    }
  }
  // Slightly incorrect due to labels
  printf("| Op count: %d\n", opcount);

  // FIXME: gross
  Node *curr = bb->ir->ops->head;

  while (curr != NULL) {
    Operation *op = curr->data;
    if (op->opcode == IR_LABEL || op->opcode == IR_ELSE_LABEL) {
      printf("%4llu| LABEL %llu\n", op->id, op->first->val.label);
    } else if (op->destination == 0) {
      printf("%4llu| %s %s %s\n", op->id, opcodeString(op->opcode),
             operandString(op->first), operandString(op->second));
    } else if (op->opcode == IR_FUNCTION || op->opcode == IR_METHOD ||
               op->opcode == IR_CLASS) {
      Value wuPtr = op->first->val.literal;
      WorkUnit *wu = AS_POINTER(wuPtr);
      printf("%4llu| %s %.*s %s\n", op->id, opcodeString(op->opcode),
             wu->name.length, wu->name.start, "");
    } else {
      printf("%4llu| t%llu := %s %s %s\n", op->id, op->destination,
             opcodeString(op->opcode), operandString(op->first),
             operandString(op->second));
    }
    curr = curr->next;
  }
}

void cfg_print(CFG *cfg) {
  printf("CFG: %.*s\n", cfg->name.length, cfg->name.start);
  LinkedList *ordered = cfg_post_order_traverse(cfg);
  Node *tail = ordered->tail;
  while (tail != NULL) {
    bb_print(tail->data);
    tail = tail->prev;
  }
}

void wu_print_all(WorkUnit *wu) {
  Node *child = wu->childWorkUnits->head;
  while (child != NULL) {
    wu_print_all(child->data);
    child = child->next;
  }
  cfg_print(wu->cfg);
}

WorkUnit *wu_create_main(Compiler *compiler, AstNode *root) {
  Token name = syntheticToken("script");
  WorkUnit *main_wu = wu_allocate(NULL, root, name, TYPE_SCRIPT);
  cfg_construct(compiler, main_wu, root->scope, name);

  return main_wu;
}
