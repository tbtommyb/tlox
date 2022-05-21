#include "cfg.h"
#include "compiler.h"
#include "execution_context.h"
#include "memory.h"
#include "scanner.h"

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

// TEMP
char *valueToString(Value value) {
  char *output = (char *)malloc(128);
  switch (value.type) {
  case VAL_BOOL:
    snprintf(output, 128, AS_BOOL(value) ? "true" : "false");
    break;
  case VAL_NIL:
    snprintf(output, 128, "nil");
    break;
  case VAL_NUMBER:
    snprintf(output, 128, "%g", AS_NUMBER(value));
    break;
  case VAL_EMPTY:
    snprintf(output, 128, "<empty>");
    break;
  case VAL_OBJ: {
    if (IS_STRING(value)) {
      snprintf(output, 128, "%s", AS_STRING(value)->chars);
    }
    break;
  }
  }
  return output;
}
// END TEMP
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

  return cfg;
}

static IROp tokenToOp(TokenType token) {
  switch (token) {
  case TOKEN_PLUS:
    return IR_ADD;
  case TOKEN_MINUS:
    return IR_SUBTRACT;
  case TOKEN_BANG:
    return IR_NOT;
  case TOKEN_SLASH:
    return IR_DIVIDE;
  case TOKEN_STAR:
    return IR_MULTIPLY;
  case TOKEN_PERCENT:
    return IR_MODULO;
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

static Operand *newSymbolOperand(Symbol symbol) {
  Operand *operand = allocateOperand(OPERAND_SYMBOL);
  operand->val.symbol = symbol;

  return operand;
}

// Make a separate function for statements that doesn't output to register?
Operation *newOperation(IROp opcode, Operand *first, Operand *second) {
  Operation *op = allocateOperation();

  op->destination = getRegister();
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

static Operation *newGotoOperation(LabelId labelId) {
  Operation *op = allocateOperation();

  op->id = getOperationId();
  op->opcode = IR_GOTO;
  op->first = newLabelOperand(labelId);

  return op;
}

static Operation *newLabelOperation(LabelId labelId, IROp opcode) {
  Operation *op = allocateOperation();

  op->id = getOperationId();
  op->opcode = opcode;
  op->first = newLabelOperand(labelId);

  return op;
}

static Operation *tailOf(Operation *node) {
  Operation *tail = node;
  while (tail->next != NULL) {
    tail = tail->next;
  }
  return tail;
}

static Operation *walkAst(Compiler *compiler, BasicBlock *bb, AstNode *node,
                          Scope *activeScope, LinkedList *pendingNodes,
                          CFG *activeCFG) {
  if (node == NULL) {
    return NULL;
  }

  bb->opsCount++;
  Operation *op = NULL;

  switch (node->type) {
  case EXPR_LITERAL: {
    Operand *constantValue = newLiteralOperand(node->literal);
    op = newOperation(IR_CONSTANT, constantValue, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_UNARY: {
    Operation *right = walkAst(compiler, bb, node->branches.right, activeScope,
                               pendingNodes, activeCFG);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(tokenToUnaryOp(node->op), value, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_BINARY: {
    Operation *left = walkAst(compiler, bb, node->branches.left, activeScope,
                              pendingNodes, activeCFG);
    Operation *leftTail = tailOf(left);

    Operation *right = walkAst(compiler, bb, node->branches.right, activeScope,
                               pendingNodes, activeCFG);
    Operation *rightTail = tailOf(right);

    op = newOperation(tokenToOp(node->op),
                      newRegisterOperand(leftTail->destination),
                      newRegisterOperand(rightTail->destination));

    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_VARIABLE: {
    Value nameString =
        OBJ_VAL(copyString(node->token.start, node->token.length));
    Operand *name = newLiteralOperand(nameString);

    Symbol symbol = {0};
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol.type == SCOPE_GLOBAL ? IR_GET_GLOBAL : IR_GET_LOCAL;

    op = newOperation(opcode, newSymbolOperand(symbol), NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_PRINT: {
    walkAst(compiler, bb, node->expr, activeScope, pendingNodes, activeCFG);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(IR_PRINT, value, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_IF: {
    // TODO: tidy up implementation here
    Operation *expr =
        walkAst(compiler, bb, node->expr, activeScope, pendingNodes, activeCFG);
    LabelId ifLabelId = getLabelId();
    LabelId elseLabelId = getLabelId();
    LabelId afterLabelId = getLabelId();
    op = newOperation(IR_COND, newRegisterOperand(bb->curr->destination),
                      newLabelOperand(elseLabelId));
    bb->curr->next = op;
    bb->curr = op;

    Operation *ifLabel = newLabelOperation(ifLabelId, IR_LABEL);
    bb->curr->next = ifLabel;
    bb->curr = ifLabel;
    walkAst(compiler, bb, node->branches.left, activeScope, pendingNodes,
            activeCFG);
    Operation *afterOp = newGotoOperation(afterLabelId);
    bb->curr->next = afterOp;
    bb->curr = afterOp;
    Operation *elseLabel = newLabelOperation(elseLabelId, IR_ELSE_LABEL);
    bb->curr->next = elseLabel;
    bb->curr = elseLabel;
    walkAst(compiler, bb, node->branches.right, activeScope, pendingNodes,
            activeCFG);
    Operation *afterLabel = newLabelOperation(afterLabelId, IR_LABEL);
    bb->curr->next = afterLabel;
    bb->curr = afterLabel;
    break;
  }
  case STMT_DEFINE_CONST:
  case STMT_DEFINE: {
    if (node->expr == NULL) {
      op = newOperation(IR_NIL, NULL, NULL);
      bb->curr->next = op;
      bb->curr = op;
    } else {
      op = walkAst(compiler, bb, node->expr, activeScope, pendingNodes,
                   activeCFG);
    }

    Symbol symbol = {0};
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode =
        symbol.type == SCOPE_GLOBAL ? IR_DEFINE_GLOBAL : IR_DEFINE_LOCAL;

    // FIXME: Skipping register here. Need to fix this
    Operand *scopeOperand = newLiteralOperand(POINTER_VAL(activeScope));
    op = newOperation(opcode, newSymbolOperand(symbol), scopeOperand);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_ASSIGN: {
    Value nameString =
        OBJ_VAL(copyString(node->token.start, node->token.length));
    Operand *name = newLiteralOperand(nameString);
    op =
        walkAst(compiler, bb, node->expr, activeScope, pendingNodes, activeCFG);

    Symbol symbol = {0};
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    IROp opcode = symbol.type == SCOPE_GLOBAL ? IR_SET_GLOBAL : IR_SET_LOCAL;

    op = newOperation(opcode, name, newSymbolOperand(symbol));
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_MODULE: {
    Node *stmtNode = (Node *)node->stmts->head;

    while (stmtNode != NULL) {
      walkAst(compiler, bb, stmtNode->data, activeScope, pendingNodes,
              activeCFG);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    op = newOperation(IR_BEGIN_SCOPE, NULL, NULL);
    bb->curr->next = op;
    bb->curr = op;

    Node *blockNode = (Node *)node->stmts->head;

    while (blockNode != NULL) {
      walkAst(compiler, bb, blockNode->data, node->scope, pendingNodes,
              activeCFG);
      blockNode = blockNode->next;
    }

    op = newOperation(IR_END_SCOPE, NULL, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_FUNCTION: {
    // FIXME: bit of a hack to get the name working
    node->expr->token = node->token;

    WorkUnit *wu = wu_allocate(activeCFG->context, node->expr, node->token);
    linkedList_append(pendingNodes, wu);
    linkedList_append(activeCFG->childFunctions, wu);
    break;
  }
  case EXPR_FUNCTION: {
    Node *paramNode = (Node *)node->params->head;
    while (paramNode != NULL) {
      Symbol symbol = {0};
      Token *name = paramNode->data;
      if (!scope_search(node->scope, name->start, name->length, &symbol)) {
        errorAt(compiler, name, "Symbol is not defined in current scope.");
        break;
      }
      Operand *pointer = newLiteralOperand(POINTER_VAL(node->scope));
      op = newOperation(IR_DEFINE_LOCAL, newSymbolOperand(symbol), pointer);
      bb->curr->next = op;
      bb->curr = op;
      paramNode = paramNode->next;
    }
    walkAst(compiler, bb, node->expr, activeScope, pendingNodes, activeCFG);
    break;
  }
  case STMT_RETURN: {
    Operation *expr =
        walkAst(compiler, bb, node->expr, activeScope, pendingNodes, activeCFG);
    op = newOperation(IR_RETURN, newRegisterOperand(expr->destination), NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_CALL: {
    Symbol symbol = {0};
    if (!scope_search(activeScope, node->token.start, node->token.length,
                      &symbol)) {
      errorAt(compiler, &node->token,
              "Symbol is not defined in current scope.");
      break;
    }

    Operand *constantValue = newSymbolOperand(symbol);
    IROp opcode = symbol.type == SCOPE_GLOBAL ? IR_GET_GLOBAL : IR_GET_LOCAL;
    op = newOperation(opcode, constantValue, NULL);
    bb->curr->next = op;
    bb->curr = op;

    int arity = 0;
    Node *paramNode = (Node *)node->params->head;
    while (paramNode != NULL) {
      arity++;
      walkAst(compiler, bb, paramNode->data, node->scope, pendingNodes,
              activeCFG);
      paramNode = paramNode->next;
    }
    Operand *arityOperand = newLiteralOperand(NUMBER_VAL(arity));

    op = newOperation(IR_CALL, arityOperand, NULL);
    bb->curr->next = op;
    bb->curr = op;
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
void constructCFG(CFG *cfg, BasicBlock *irList, Token name, Scope *scope) {
  cfg->start = allocateBasicBlock();

  initTable(&labelBasicBlockMapping);

  BasicBlock *currentBB = cfg->start;
  currentBB->ops = irList->ops;
  Operation *currentOp = irList->ops;

  while (currentOp != NULL) {
    currentBB->opsCount++;

    if (currentOp->opcode == IR_COND) {
      Value ifBranchPtr;
      LabelId ifBranchHead = currentOp->next->first->val.label;
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
    } else if (currentOp->next != NULL && currentOp->next->opcode == IR_LABEL) {
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
    return valueToString(operand->val.literal);
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
    Token name = operand->val.symbol.name;
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
  case IR_CONSTANT:
    return "constant";
  case IR_CODE_START:
    return "<start>";
  case IR_COND:
    return "cond";
  case IR_DIVIDE:
    return "/";
  case IR_SUBTRACT:
    return "-";
  case IR_MODULO:
    return "%";
  case IR_MULTIPLY:
    return "*";
  case IR_NEGATE:
    return "-";
  case IR_NIL:
    return "nil";
  case IR_NOT:
    return "!";
  case IR_POP:
    return "pop";
  case IR_PRINT:
    return "print";
  case IR_GOTO:
    return "goto";
  case IR_LABEL:
    return "label";
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
  case IR_RETURN:
    return "return";
  case IR_BEGIN_SCOPE:
    return "begin scope";
  case IR_END_SCOPE:
    return "end scope";
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

static CFG *newCFG(Compiler *compiler, CompilerState *state, WorkUnit *wu,
                   LinkedList *pendingNodes) {
  BasicBlock *irList = newBasicBlock(wu->node);

  // Initialise context. Move somewhere else once functions supported
  /* Symbol symbol = {0}; */
  /* if (compiler->type != TYPE_FUNCTION) { */
  /*   if (!st_get(compiler->currentScope->st, "this", 0, &symbol)) { */
  /*     error(compiler, "'this' is not defined in current scope."); */
  /*     return NULL; */
  /*   } */
  /* } else { */
  /*   if (!st_get(compiler->currentScope->st, "", 0, &symbol)) { */
  /*     error(compiler, "'\"\"' is not defined in current scope."); */
  /*     return NULL; */
  /*   } */
  /* } */
  /* Operation *op = newOperation(IR_DEFINE_LOCAL, newSymbolOperand(symbol),
   * NULL); */
  /* irList->curr->next = op; */
  /* irList->curr = op; */

  CFG *cfg = allocateCFG(wu->node->token);
  cfg->context->enclosing = wu->enclosing;
  // generate flat IR list by walking AST
  walkAst(compiler, irList, wu->node, wu->node->scope, pendingNodes, cfg);
  // split IR list into CFG
  constructCFG(cfg, irList, wu->node->token, wu->node->scope);

  cfg->context->localCount++; // this or function

  return cfg;
}

WorkUnit *createWorkUnits(Compiler *compiler, CompilerState *state,
                          AstNode *root) {
  LinkedList *pendingNodes = linkedList_allocate();
  WorkUnit *main =
      wu_allocate(NULL, root, (Token){.start = "main", .length = 4});
  linkedList_append(pendingNodes, main);

  Node *curr = pendingNodes->head;
  while (curr != NULL) {
    WorkUnit *wu = (WorkUnit *)curr->data;
    wu->cfg = newCFG(compiler, state, wu, pendingNodes);

    curr = curr->next;
  }
  return main;
}
