#include "cfg.h"
#include "compiler.h"
#include "execution_context.h"
#include "memory.h"
#include "scanner.h"
#include "util.h"

#include <stdlib.h>

Table labelBasicBlockMapping;
BasicBlock *newBasicBlock(AstNode *node);

// TODO: make thread safe
static Register currentRegister = 1;
static BasicBlockId currentBasicBlockId = 0;
static OperationId currentOperationId = 1;
static LabelId currentLabelId = 0;

static Register getRegister() { return currentRegister++; }
static BasicBlockId getBasicBlockId() { return currentBasicBlockId++; }
static OperationId getOperationId() { return currentOperationId++; }
static LabelId getLabelId() { return currentLabelId++; }

static CFG *newCFG(Compiler *compiler, WorkUnit *wu);

static WorkUnit *wu_allocate(ExecutionContext *ec, AstNode *node, Token name) {
  WorkUnit *wu = (WorkUnit *)reallocate(NULL, 0, sizeof(WorkUnit));
  wu->enclosing = ec;
  wu->node = node;
  wu->cfg = NULL;
  wu->f = NULL;
  wu->name = name;
  return wu;
}

static Operation *allocateOperation() {
  Operation *op = (Operation *)reallocate(NULL, 0, sizeof(Operation));
  op->id = 0;
  op->token = NULL;
  op->first = NULL;
  op->second = NULL;
  op->next = NULL;

  return op;
}

static BasicBlock *allocateBasicBlock() {
  BasicBlock *bb = (BasicBlock *)reallocate(NULL, 0, sizeof(BasicBlock));
  bb->opsCount = 0;
  bb->ops = NULL;
  bb->curr = NULL;
  bb->id = getBasicBlockId();
  bb->trueEdge = NULL;
  bb->falseEdge = NULL;
  bb->labelId = -1;

  return bb;
}

static Operand *allocateOperand(OperandType type) {
  Operand *operand = (Operand *)reallocate(NULL, 0, sizeof(Operand));
  operand->type = type;

  return operand;
}

static CFG *allocateCFG(Token name) {
  CFG *cfg = (CFG *)reallocate(NULL, 0, sizeof(CFG));
  cfg->start = NULL;
  cfg->name = name;
  cfg->context = ec_allocate();
  cfg->childFunctions = linkedList_allocate();
  cfg->arity = 0;

  return cfg;
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

static Operand *newLiteralOperand(Value value) {
  Operand *operand = allocateOperand(OPERAND_LITERAL);
  operand->val.literal = value;

  return operand;
}

// TODO: cache operands rather than recreating
static Operand *newRegisterOperand(Register reg) {
  Operand *operand = allocateOperand(OPERAND_REG);
  operand->val.source = reg;

  return operand;
}

static Operand *newLabelOperand(LabelId id) {
  Operand *operand = allocateOperand(OPERAND_LABEL);
  operand->val.label = id;

  return operand;
}

static Operand *newSymbolOperand(Symbol *symbol) {
  Operand *operand = allocateOperand(OPERAND_SYMBOL);
  operand->val.symbol = symbol;

  return operand;
}

static Operand *newTokenOperand(Token token) {
  Operand *operand = allocateOperand(OPERAND_TOKEN);
  operand->val.token = token;

  return operand;
}

// Make a separate function for statements that doesn't output to register?
Operation *newOperation(Token *token, IROp opcode, Operand *first,
                        Operand *second) {
  Operation *op = allocateOperation();

  op->destination = getRegister();
  op->token = token;
  op->id = getOperationId();
  op->opcode = opcode;
  op->first = first;
  op->second = second;

  return op;
}

static Operation *newStartOperation() {
  Operation *op = allocateOperation();

  op->destination = 0;
  op->opcode = IR_CODE_START;

  return op;
}

static Operation *newGotoOperation(Token *token, LabelId labelId) {
  Operation *op = allocateOperation();

  op->id = getOperationId();
  op->opcode = IR_GOTO;
  op->first = newLabelOperand(labelId);
  op->token = token;

  return op;
}

static Operation *newLabelOperation(Token *token, LabelId labelId,
                                    IROp opcode) {
  Operation *op = allocateOperation();

  op->id = getOperationId();
  op->opcode = opcode;
  op->first = newLabelOperand(labelId);
  op->token = token;

  return op;
}

static void write(BasicBlock *bb, Operation *op) {
  bb->curr->next = op;
  bb->curr = op;
}

static Operation *walkAst(Compiler *compiler, BasicBlock *bb, AstNode *node,
                          Scope *activeScope, CFG *activeCFG) {
  if (node == NULL) {
    return NULL;
  }

  bb->opsCount++;
  Operation *op = NULL;

  switch (node->type) {
  case EXPR_LITERAL: {
    Operand *constantValue = newLiteralOperand(AS_LITERAL_EXPR(node)->literal);

    op = newOperation(&node->token, IR_CONSTANT, constantValue, NULL);
    write(bb, op);
    break;
  }
  case EXPR_NIL: {
    op = newOperation(&node->token, IR_NIL, NULL, NULL);
    write(bb, op);
    break;
  }
  case EXPR_UNARY: {
    UnaryExprAstNode *expr = AS_UNARY_EXPR(node);
    Operation *right =
        walkAst(compiler, bb, expr->right, activeScope, activeCFG);
    Operand *value = newRegisterOperand(right->destination);

    op = newOperation(&node->token, tokenToUnaryOp(expr->op), value, NULL);
    write(bb, op);
    break;
  }
  case EXPR_BINARY: {
    BinaryExprAstNode *expr = AS_BINARY_EXPR(node);

    Operation *left =
        walkAst(compiler, bb, expr->branches.left, activeScope, activeCFG);
    if (left == NULL) {
      errorAt(compiler, &node->token,
              "Failed to build left branch of binary expression.");
      break;
    }

    Operation *right =
        walkAst(compiler, bb, expr->branches.right, activeScope, activeCFG);
    if (right == NULL) {
      errorAt(compiler, &node->token,
              "Failed to build right branch of binary expression.");
      break;
    }

    op = newOperation(&node->token, tokenToBinaryOp(expr->op),
                      newRegisterOperand(left->destination),
                      newRegisterOperand(right->destination));
    write(bb, op);
    break;
  }
  case EXPR_AND: {
    AndExprAstNode *expr = AS_AND_EXPR(node);
    LabelId afterLabelId = getLabelId();
    LabelId trueLabelId = getLabelId();

    Operation *left =
        walkAst(compiler, bb, expr->branches.left, activeScope, activeCFG);

    op = newOperation(&node->token, IR_COND,
                      newRegisterOperand(left->destination),
                      newLabelOperand(afterLabelId));
    write(bb, op);

    op = newLabelOperation(&node->token, trueLabelId, IR_LABEL);
    write(bb, op);

    op = walkAst(compiler, bb, expr->branches.right, activeScope, activeCFG);
    write(bb, op);

    op = newLabelOperation(&node->token, afterLabelId, IR_LABEL);
    write(bb, op);

    break;
  }
  case EXPR_OR: {
    OrExprAstNode *expr = AS_OR_EXPR(node);
    LabelId afterLabelId = getLabelId();
    LabelId elseLabelId = getLabelId();

    Operation *left =
        walkAst(compiler, bb, expr->branches.left, activeScope, activeCFG);

    op = newOperation(&node->token, IR_COND_NO_POP,
                      newRegisterOperand(left->destination),
                      newLabelOperand(elseLabelId));
    write(bb, op);

    op = newGotoOperation(&node->token, afterLabelId);
    write(bb, op);

    op = newLabelOperation(&node->token, elseLabelId, IR_LABEL);
    write(bb, op);

    // FIXME: pop shouldn't live in IR
    op = newOperation(&node->token, IR_POP, NULL, NULL);
    write(bb, op);

    Operation *right =
        walkAst(compiler, bb, expr->branches.right, activeScope, activeCFG);

    op = newLabelOperation(&node->token, afterLabelId, IR_LABEL);
    write(bb, op);

    break;
  }
  case EXPR_VARIABLE: {
    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol->type == SCOPE_GLOBAL ? IR_GET_GLOBAL : IR_GET_LOCAL;

    op = newOperation(&node->token, opcode, newTokenOperand(node->token), NULL);
    write(bb, op);
    break;
  }
  case EXPR_THIS: {
    op = newOperation(&node->token, IR_GET_LOCAL,
                      newTokenOperand(syntheticToken("this")), NULL);
    write(bb, op);
    break;
  }
  case STMT_PRINT: {
    PrintStmtAstNode *stmt = AS_PRINT_STMT(node);

    op = walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);
    Operand *value = newRegisterOperand(op->destination);

    op = newOperation(&node->token, IR_PRINT, value, NULL);
    write(bb, op);
    break;
  }
  case STMT_IF: {
    IfStmtAstNode *stmt = AS_IF_STMT(node);
    LabelId elseLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();
    LabelId skipLabelId = getLabelId();
    bool elseBranchPresent = stmt->branches.elseB != NULL;

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    Operation *expr =
        walkAst(compiler, bb, stmt->condition, activeScope, activeCFG);

    op = newOperation(
        &node->token, IR_COND, newRegisterOperand(expr->destination),
        newLabelOperand(elseBranchPresent ? elseLabelId : afterLabelId));
    write(bb, op);

    walkAst(compiler, bb, stmt->branches.then, activeScope, activeCFG);

    op = newGotoOperation(&node->token, skipLabelId);
    write(bb, op);

    if (elseBranchPresent) {
      op = newLabelOperation(&node->token, elseLabelId, IR_LABEL);
      write(bb, op);

      // FIXME: remove pop from IR
      op = newOperation(&node->token, IR_POP, NULL, NULL);
      write(bb, op);

      walkAst(compiler, bb, stmt->branches.elseB, activeScope, activeCFG);
    }

    op = newLabelOperation(&node->token, afterLabelId, IR_LABEL);
    write(bb, op);

    if (!elseBranchPresent) {
      // FIXME: remove pop from IR
      op = newOperation(&node->token, IR_POP, NULL, NULL);
      write(bb, op);
    }

    op = newLabelOperation(&node->token, skipLabelId, IR_LABEL);
    write(bb, op);

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);

    break;
  }
  case STMT_WHILE: {
    WhileStmtAstNode *stmt = AS_WHILE_STMT(node);
    LabelId exprLabelId = getLabelId();
    LabelId ifLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    op = newLabelOperation(&node->token, exprLabelId, IR_LABEL);
    write(bb, op);

    Operation *expr =
        walkAst(compiler, bb, stmt->condition, activeScope, activeCFG);

    op = newOperation(&node->token, IR_COND,
                      newRegisterOperand(expr->destination),
                      newLabelOperand(afterLabelId));
    write(bb, op);

    op = newLabelOperation(&node->token, ifLabelId, IR_LABEL);
    write(bb, op);

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    walkAst(compiler, bb, stmt->branches.then, activeScope, activeCFG);

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);

    op =
        newOperation(&node->token, IR_LOOP, newLabelOperand(exprLabelId), NULL);
    write(bb, op);

    op = newLabelOperation(&node->token, afterLabelId, IR_ELSE_LABEL);
    write(bb, op);

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);

    break;
  }
  case STMT_FOR: {
    ForStmtAstNode *stmt = AS_FOR_STMT(node);
    LabelId preLabelId = getLabelId();
    LabelId condLabelId = getLabelId();
    LabelId bodyLabelId = getLabelId();
    LabelId postLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    op = newLabelOperation(&node->token, preLabelId, IR_LABEL);
    write(bb, op);

    if (stmt->branches.pre != NULL) {
      walkAst(compiler, bb, stmt->branches.pre, node->scope, activeCFG);
    }

    op = newLabelOperation(&node->token, condLabelId, IR_LABEL);
    write(bb, op);

    if (stmt->branches.cond != NULL) {
      Operation *condExpr =
          walkAst(compiler, bb, stmt->branches.cond, node->scope, activeCFG);
      op = newOperation(&node->token, IR_COND,
                        newRegisterOperand(condExpr->destination),
                        newLabelOperand(afterLabelId));
      write(bb, op);
    }

    if (stmt->branches.post != NULL) {
      op = newGotoOperation(&node->token, bodyLabelId);
      write(bb, op);

      op = newLabelOperation(&node->token, postLabelId, IR_LABEL);
      write(bb, op);

      walkAst(compiler, bb, stmt->branches.post, node->scope, activeCFG);
      // FIXME: don't like having POP in IR
      op = newOperation(&node->token, IR_POP, NULL, NULL);
      write(bb, op);

      op = newOperation(&node->token, IR_LOOP, newLabelOperand(condLabelId),
                        NULL);
      write(bb, op);
    }

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    op = newLabelOperation(&node->token, bodyLabelId, IR_LABEL);
    write(bb, op);

    walkAst(compiler, bb, stmt->branches.body, node->scope, activeCFG);

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);

    op =
        newOperation(&node->token, IR_LOOP,
                     newLabelOperand(stmt->branches.post != NULL ? postLabelId
                                                                 : condLabelId),
                     NULL);
    write(bb, op);

    op = newLabelOperation(&node->token, afterLabelId,
                           stmt->branches.cond != NULL ? IR_ELSE_LABEL
                                                       : IR_LABEL);
    write(bb, op);

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);

    break;
  }
  case STMT_DEFINE: {
    DefineStmtAstNode *stmt = AS_DEFINE_STMT(node);
    if (stmt->expr == NULL) {
      op = newOperation(&node->token, IR_NIL, NULL, NULL);
    } else {
      op = walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);
    }
    write(bb, op);

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode =
        symbol->type == SCOPE_GLOBAL ? IR_DEFINE_GLOBAL : IR_DEFINE_LOCAL;

    op = newOperation(&node->token, opcode, newSymbolOperand(symbol), NULL);
    write(bb, op);
    break;
  }
  case STMT_ASSIGN: {
    AssignStmtAstNode *stmt = AS_ASSIGN_STMT(node);
    op = walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);

    Symbol *symbol = NULL;
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol->type == SCOPE_GLOBAL ? IR_SET_GLOBAL : IR_SET_LOCAL;

    op = newOperation(&node->token, opcode, newTokenOperand(node->token),
                      newRegisterOperand(op->destination));
    write(bb, op);
    break;
  }
  case STMT_MODULE: {
    ModuleStmtAstNode *stmt = AS_MODULE_STMT(node);
    Node *stmtNode = (Node *)stmt->stmts->head;

    while (stmtNode != NULL) {
      walkAst(compiler, bb, stmtNode->data, activeScope, activeCFG);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    BlockStmtAstNode *stmt = AS_BLOCK_STMT(node);
    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    Node *blockNode = (Node *)stmt->stmts->head;

    while (blockNode != NULL) {
      walkAst(compiler, bb, blockNode->data, node->scope, activeCFG);
      blockNode = blockNode->next;
    }

    op = newOperation(&node->token, IR_END_SCOPE, NULL, NULL);
    write(bb, op);
    break;
  }
  case STMT_FUNCTION: {
    FunctionStmtAstNode *stmt = AS_FUNCTION_STMT(node);
    // FIXME: bit of a hack to get the name working
    AS_AST_NODE(stmt->expr)->token = node->token;

    WorkUnit *wu =
        wu_allocate(activeCFG->context, AS_AST_NODE(stmt->expr), node->token);
    wu->functionType = AS_FUNCTION_EXPR(stmt->expr)->functionType;
    newCFG(compiler, wu);
    linkedList_append(activeCFG->childFunctions, wu);

    Operand *pointer = newLiteralOperand(POINTER_VAL(wu));
    op = newOperation(&node->token, IR_FUNCTION, pointer, NULL);
    write(bb, op);
    break;
  }
  case EXPR_FUNCTION: {
    FunctionExprAstNode *expr = AS_FUNCTION_EXPR(node);

    for (int arity = 0; arity < expr->arity; arity++) {
      Symbol *symbol = NULL;
      Token name = expr->params[arity];
      if (!scope_search(node->scope, name.start, name.length, &symbol)) {
        errorAt(compiler, &name, "Symbol is not defined in current scope.2");
        break;
      }
      Operand *pointer = newLiteralOperand(POINTER_VAL(node->scope));
      op = newOperation(&node->token, IR_DEFINE_LOCAL, newSymbolOperand(symbol),
                        pointer);
      write(bb, op);
    }
    activeCFG->arity = expr->arity; // FIXME: unused?

    op = newOperation(&node->token, IR_BEGIN_SCOPE, NULL, NULL);
    write(bb, op);

    BlockStmtAstNode *blockStmt = expr->body;
    Node *blockNode = (Node *)blockStmt->stmts->head;

    while (blockNode != NULL) {
      walkAst(compiler, bb, blockNode->data, AS_AST_NODE(expr->body)->scope,
              activeCFG);
      blockNode = blockNode->next;
    }
    break;
  }
  case STMT_RETURN: {
    ReturnStmtAstNode *stmt = AS_RETURN_STMT(node);
    if (activeScope->type == TYPE_INITIALIZER) {
      op = newOperation(&node->token, IR_RETURN_FROM_INIT, NULL, NULL);
      write(bb, op);
      break;
    }
    Operation *expr = NULL;
    if (stmt->expr == NULL) {
      expr = newOperation(&node->token, IR_NIL, NULL, NULL);
      write(bb, expr);
    } else {
      expr = walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);
    }
    op = newOperation(&node->token, IR_RETURN,
                      newRegisterOperand(expr->destination), NULL);
    write(bb, op);
    break;
  }
  case STMT_EXPR: {
    ExprStmtAstNode *stmt = AS_EXPR_STMT(node);

    walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);

    op = newOperation(&node->token, IR_STMT_EXPR, NULL, NULL);
    write(bb, op);
    break;
  }
  case EXPR_GET_PROPERTY: {
    walkAst(compiler, bb, AS_GET_PROPERTY_EXPR(node)->target, activeScope,
            activeCFG);

    op = newOperation(&node->token, IR_GET_PROPERTY,
                      newTokenOperand(node->token), NULL);
    write(bb, op);
    break;
  }
  case STMT_SET_PROPERTY: {
    SetPropertyStmtAstNode *stmt = AS_SET_PROPERTY_STMT(node);

    walkAst(compiler, bb, stmt->target, activeScope, activeCFG);
    walkAst(compiler, bb, stmt->expr, activeScope, activeCFG);

    op = newOperation(&node->token, IR_SET_PROPERTY,
                      newTokenOperand(node->token), NULL);
    write(bb, op);
    break;
  }
  case EXPR_CALL: {
    CallExprAstNode *expr = AS_CALL_EXPR(node);
    walkAst(compiler, bb, expr->target, node->scope, activeCFG);

    int arity = 0;
    Node *paramNode = (Node *)expr->params->head;
    while (paramNode != NULL) {
      arity++;
      walkAst(compiler, bb, paramNode->data, node->scope, activeCFG);
      paramNode = paramNode->next;
    }
    Operand *arityOperand = newLiteralOperand(NUMBER_VAL(arity));

    op = newOperation(&node->token, IR_CALL, arityOperand, NULL);
    write(bb, op);
    break;
  }
  case EXPR_INVOKE: {
    InvocationExprAstNode *expr = AS_INVOCATION_EXPR(node);
    walkAst(compiler, bb, expr->target, node->scope, activeCFG);

    int arity = 0;
    Node *paramNode = (Node *)expr->params->head;
    while (paramNode != NULL) {
      arity++;
      walkAst(compiler, bb, paramNode->data, node->scope, activeCFG);
      paramNode = paramNode->next;
    }
    Operand *arityOperand = newLiteralOperand(NUMBER_VAL(arity));

    op = newOperation(&node->token, IR_INVOKE, newTokenOperand(node->token),
                      arityOperand);
    write(bb, op);
    break;
  }
  case EXPR_SUPER_INVOKE: {
    SuperInvocationExprAstNode *expr = AS_SUPER_INVOCATION_EXPR(node);
    op = newOperation(&node->token, IR_GET_LOCAL,
                      newTokenOperand(syntheticToken("this")), NULL);
    write(bb, op);

    walkAst(compiler, bb, expr->target, node->scope, activeCFG);

    int arity = 0;
    Node *paramNode = (Node *)expr->params->head;
    while (paramNode != NULL) {
      arity++;
      walkAst(compiler, bb, paramNode->data, node->scope, activeCFG);
      paramNode = paramNode->next;
    }
    Operand *arityOperand = newLiteralOperand(NUMBER_VAL(arity));

    op = newOperation(&node->token, IR_SUPER_INVOKE,
                      newTokenOperand(node->token), arityOperand);
    write(bb, op);
    break;
  }
  case EXPR_SUPER: {
    SuperExprAstNode *expr = AS_SUPER_EXPR(node);
    Token this = syntheticToken("this");

    op = newOperation(&node->token, IR_GET_LOCAL, newTokenOperand(this), NULL);
    write(bb, op);

    walkAst(compiler, bb, expr->target, node->scope, activeCFG);

    op = newOperation(&node->token, IR_SUPER, newTokenOperand(node->token),
                      NULL);
    write(bb, op);
    break;
  }
  case STMT_CLASS: {
    ClassStmtAstNode *stmt = AS_CLASS_STMT(node);
    // FIXME: bit of a hack to get the name working
    AS_AST_NODE(stmt->body)->token = node->token;
    WorkUnit *wu =
        wu_allocate(activeCFG->context, AS_AST_NODE(stmt->body), node->token);
    wu->functionType = TYPE_CLASS;
    newCFG(compiler, wu);
    linkedList_append(activeCFG->childFunctions, wu);

    Operand *pointer = newLiteralOperand(POINTER_VAL(wu));
    if (OPTIONAL_HAS_VALUE(stmt->superclass)) {
      op = newOperation(&node->token, IR_CLASS, pointer,
                        newTokenOperand(OPTIONAL_VALUE(stmt->superclass)));
    } else {
      op = newOperation(&node->token, IR_CLASS, pointer, NULL);
    }
    write(bb, op);

    break;
  }
  case STMT_CLASS_BODY: {
    ClassBodyStmtAstNode *stmt = AS_CLASS_BODY_STMT(node);
    Node *methodNode = (Node *)stmt->stmts->head;
    while (methodNode != NULL) {
      walkAst(compiler, bb, methodNode->data, node->scope, activeCFG);
      methodNode = methodNode->next;
    }
    break;
  }
  case STMT_METHOD: {
    MethodStmtAstNode *stmt = AS_METHOD_STMT(node);

    // FIXME: bit of a hack to get the name working
    AS_AST_NODE(stmt->body)->token = node->token;

    WorkUnit *wu =
        wu_allocate(activeCFG->context, AS_AST_NODE(stmt->body), node->token);
    wu->functionType = AS_FUNCTION_EXPR(stmt->body)->functionType;
    newCFG(compiler, wu);
    linkedList_append(activeCFG->childFunctions, wu);

    Operand *pointer = newLiteralOperand(POINTER_VAL(wu));
    op = newOperation(&node->token, IR_METHOD, pointer, NULL);
    write(bb, op);
    break;
  }
  }
  return op;
}

// TODO: Create IRList or similar (linked list of 3AC IR)
// so that BasicBlock is only created when actually making blocks
// Rename too
BasicBlock *newBasicBlock(AstNode *node) {
  if (node == NULL) {
    return NULL;
  }
  BasicBlock *bb = allocateBasicBlock();
  bb->ops = newStartOperation();
  bb->curr = bb->ops;

  return bb;
}

// TODO: make beautiful
void constructCFG(CFG *cfg, BasicBlock *irList) {
  cfg->start = allocateBasicBlock();

  initTable(&labelBasicBlockMapping);

  BasicBlock *currentBB = cfg->start;
  currentBB->ops = irList->ops;
  Operation *currentOp = irList->ops;

  while (currentOp != NULL) {
    currentBB->opsCount++;

    if (currentOp->opcode == IR_COND) {
      Value ifBranchPtr;
      // TODO seeking next here is a code smell
      Operation *ifBranchLabelInstr = currentOp->next;
      while (ifBranchLabelInstr->first == NULL) {
        ifBranchLabelInstr = ifBranchLabelInstr->next;
      }
      LabelId ifBranchHead = ifBranchLabelInstr->first->val.label;
      BasicBlock *ifBranchBB = NULL;
      if (tableGet(&labelBasicBlockMapping, NUMBER_VAL(ifBranchHead),
                   &ifBranchPtr)) {
        ifBranchBB = AS_POINTER(ifBranchPtr);
      } else {
        ifBranchBB = allocateBasicBlock();
        tableSet(&labelBasicBlockMapping, NUMBER_VAL(ifBranchHead),
                 POINTER_VAL(ifBranchBB));
      }

      Value elseBranchPtr;
      LabelId elseBranchHead = currentOp->second->val.label;
      BasicBlock *elseBranchBB = NULL;
      if (tableGet(&labelBasicBlockMapping, NUMBER_VAL(elseBranchHead),
                   &elseBranchPtr)) {
        elseBranchBB = AS_POINTER(elseBranchPtr);
      } else {
        elseBranchBB = allocateBasicBlock();
        tableSet(&labelBasicBlockMapping, NUMBER_VAL(elseBranchHead),
                 POINTER_VAL(elseBranchBB));
      }

      currentBB->trueEdge = ifBranchBB;
      currentBB->falseEdge = elseBranchBB;
    } else if (currentOp->opcode == IR_LABEL ||
               currentOp->opcode == IR_ELSE_LABEL) {
      Value labelBBPtr;
      BasicBlock *labelBB = NULL;
      LabelId labelId = currentOp->first->val.label;
      if (tableGet(&labelBasicBlockMapping, NUMBER_VAL(labelId), &labelBBPtr)) {
        labelBB = AS_POINTER(labelBBPtr);
        labelBB->labelId = labelId;
      } else {
        labelBB = allocateBasicBlock();
        labelBB->labelId = labelId;
        tableSet(&labelBasicBlockMapping, NUMBER_VAL(labelId),
                 POINTER_VAL(labelBB));
      }
      currentBB->trueEdge = labelBB;
      currentBB = labelBB;
      currentBB->ops = currentOp->next;
    } else if (currentOp->opcode == IR_GOTO) {
      Value elseBranchPtr;
      LabelId elseBranchHead = currentOp->first->val.label;
      BasicBlock *elseBranchBB = NULL;
      if (tableGet(&labelBasicBlockMapping, NUMBER_VAL(elseBranchHead),
                   &elseBranchPtr)) {
        elseBranchBB = AS_POINTER(elseBranchPtr);
      } else {
        elseBranchBB = allocateBasicBlock();
        tableSet(&labelBasicBlockMapping, NUMBER_VAL(elseBranchHead),
                 POINTER_VAL(elseBranchBB));
      }

      currentBB->trueEdge = elseBranchBB;
    } else if (currentOp->next != NULL &&
               (currentOp->next->opcode == IR_LABEL ||
                currentOp->next->opcode == IR_ELSE_LABEL)) {
      Value labelBBPtr;
      BasicBlock *labelBB = NULL;
      LabelId labelId = currentOp->next->first->val.label;
      if (tableGet(&labelBasicBlockMapping, NUMBER_VAL(labelId), &labelBBPtr)) {
        labelBB = AS_POINTER(labelBBPtr);
      } else {
        labelBB = allocateBasicBlock();
        tableSet(&labelBasicBlockMapping, NUMBER_VAL(labelId),
                 POINTER_VAL(labelBB));
      }
      currentBB->trueEdge = labelBB;
    }
    currentOp = currentOp->next;
  }

  freeTable(&labelBasicBlockMapping);
}

char *operandString(Operand *operand) {
  if (operand == NULL) {
    return "";
  }
  if (operand->type == OPERAND_LITERAL) {
    return writeValue(operand->val.literal);
  }
  if (operand->type == OPERAND_REG) {
    int length = snprintf(NULL, 0, "t%llu", operand->val.source);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "t%llu", operand->val.source);
    return str;
  }
  if (operand->type == OPERAND_LABEL) {
    int length = snprintf(NULL, 0, "L%llu", operand->val.label);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "L%llu", operand->val.label);
    return str;
  }
  if (operand->type == OPERAND_SYMBOL) {
    Token name = operand->val.symbol->name;
    int length = snprintf(NULL, 0, "%.*s", name.length, name.start);
    char *str = malloc(length + 1); // FIXME: free somewhere
    snprintf(str, length + 1, "%.*s", name.length, name.start);
    return str;
  }
  return "?";
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
    return "l define";
  case IR_DEFINE_GLOBAL:
    return "g define";
  case IR_GET_GLOBAL:
    return "g var";
  case IR_SET_GLOBAL:
    return "g assign";
  case IR_GET_LOCAL:
    return "l var";
  case IR_SET_LOCAL:
    return "l assign";
  case IR_GET_PROPERTY:
    return "get prop";
  case IR_SET_PROPERTY:
    return "set prop";
  case IR_POP:
    return "pop";
  case IR_RETURN:
    return "return";
  case IR_RETURN_FROM_INIT:
    return "return";
  case IR_BEGIN_SCOPE:
    return "begin scope";
  case IR_END_SCOPE:
    return "end scope";
  case IR_FUNCTION:
    return "function";
  case IR_SUBTRACT:
    return "-";
  case IR_STMT_EXPR:
    return "stmt expr";
  case IR_INVOKE:
    return "invoke";
  case IR_SUPER_INVOKE:
    return "super invoke";
  case IR_SUPER:
    return "super";
  case IR_UNKNOWN:
  default:
    return "?";
  }
}

void dfsWalk(BasicBlock *bb, Table *visitedSet, LinkedList *ordered) {
  tableSet(visitedSet, NUMBER_VAL(bb->id), TRUE_VAL);
  Value unused;
  if (bb->falseEdge != NULL) {
    if (!tableGet(visitedSet, NUMBER_VAL(bb->falseEdge->id), &unused)) {
      dfsWalk(bb->falseEdge, visitedSet, ordered);
    }
  }
  if (bb->trueEdge != NULL) {
    if (!tableGet(visitedSet, NUMBER_VAL(bb->trueEdge->id), &unused)) {
      dfsWalk(bb->trueEdge, visitedSet, ordered);
    }
  }
  linkedList_append(ordered, bb);
}

LinkedList *postOrderTraverseBasicBlock(CFG *cfg) {
  Table visitedSet;
  initTable(&visitedSet);

  LinkedList *ordered = linkedList_allocate();

  dfsWalk(cfg->start, &visitedSet, ordered);

  freeTable(&visitedSet);

  return ordered;
}

// TODO: print in less structured format that doesn't need alignment
// Free strings from writeString
void printBasicBlock(BasicBlock *bb) {
  Operation *curr = bb->ops;

  if (bb->labelId == -1) {
    printf("Basic block %llu", bb->id);
  } else {
    printf("Basic block %llu (L%lld)", bb->id, bb->labelId);
  }
  if (bb->trueEdge) {
    printf(" | Edges: %llu", bb->trueEdge->id);
  }
  if (bb->falseEdge) {
    printf(", %llu", bb->falseEdge->id);
  }
  printf("\n");
  // Slightly incorrect due to labels
  printf("Op count: %d\n", bb->opsCount);
  int i = 0;
  while (curr != NULL && i < bb->opsCount) {
    if (curr->opcode == IR_LABEL || curr->opcode == IR_ELSE_LABEL) {
      // Don't bother printing labels
    } else if (curr->destination == 0) {
      printf("%4llu: [       | %14s | %6s | %6s ]\n", curr->id,
             opcodeString(curr->opcode), operandString(curr->first),
             operandString(curr->second));
    } else if (curr->opcode == IR_FUNCTION || curr->opcode == IR_METHOD ||
               curr->opcode == IR_CLASS) {
      Value wuPtr = curr->first->val.literal;
      WorkUnit *wu = AS_POINTER(wuPtr);
      printf("%4llu: [       | %14s | %.*s | %6s ]\n", curr->id,
             opcodeString(curr->opcode), wu->name.length, wu->name.start, "");
    } else {
      printf("%4llu: [ t%-4llu | %14s | %6s | %6s ]\n", curr->id,
             curr->destination, opcodeString(curr->opcode),
             operandString(curr->first), operandString(curr->second));
    }
    curr = curr->next;
    i++;
  }
}

void printCFG(CFG *cfg) {
  printf("CFG: %.*s\n", cfg->name.length, cfg->name.start);
  LinkedList *ordered = postOrderTraverseBasicBlock(cfg);
  Node *tail = ordered->tail;
  while (tail != NULL) {
    printBasicBlock(tail->data);
    tail = tail->prev;
  }
}

void printWorkUnits(WorkUnit *root) {
  Node *child = root->cfg->childFunctions->head;
  while (child != NULL) {
    WorkUnit *wu = child->data;
    printWorkUnits(wu);
    child = child->next;
  }
  printCFG(root->cfg);
}

static CFG *newCFG(Compiler *compiler, WorkUnit *wu) {
  BasicBlock *irList = newBasicBlock(wu->node);

  CFG *cfg = allocateCFG(wu->node->token);
  cfg->context->enclosing = wu->enclosing;
  // generate flat IR list by walking AST
  walkAst(compiler, irList, wu->node, wu->node->scope, cfg);
  // split IR list into CFG
  constructCFG(cfg, irList);

  wu->cfg = cfg; // temp location

  return cfg;
}

WorkUnit *createMainWorkUnit(Compiler *compiler, AstNode *root) {
  WorkUnit *main_wu = wu_allocate(NULL, root, syntheticToken("script"));
  main_wu->functionType = TYPE_SCRIPT; // or TYPE_FUNCTION?
  main_wu->cfg = newCFG(compiler, main_wu);

  return main_wu;
}
