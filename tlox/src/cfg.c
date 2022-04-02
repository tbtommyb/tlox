#include "cfg.h"
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
  }
  return output;
}
// END TEMP

static Operation *allocateOperation() {
  Operation *op = (Operation *)reallocate(NULL, 0, sizeof(Operation));
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

static CFG *allocateCFG() {
  CFG *cfg = (CFG *)reallocate(NULL, 0, sizeof(CFG));
  cfg->start = NULL;

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

Operand *newLiteralOperand(Value value) {
  Operand *operand = allocateOperand(OPERAND_LITERAL);
  operand->val.literal = value;

  return operand;
}

// TODO: cache operands rather than recreating
Operand *newRegisterOperand(Register reg) {
  Operand *operand = allocateOperand(OPERAND_REG);
  operand->val.source = reg;

  return operand;
}

Operand *newLabelOperand(LabelId id) {
  Operand *operand = allocateOperand(OPERAND_LABEL);
  operand->val.label = id;

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

static Operation *walkAst(BasicBlock *bb, AstNode *node) {
  if (node == NULL) {
    return NULL;
  }

  bb->opsCount++;
  Operation *op = NULL;

  switch (node->type) {
  case EXPR_LITERAL: {
    Operand *value = newLiteralOperand(node->literal);
    op = newOperation(IR_ASSIGN, value, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_UNARY: {
    Operation *right = walkAst(bb, node->branches.right);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(tokenToUnaryOp(node->op), value, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case EXPR_BINARY: {
    Operation *left = walkAst(bb, node->branches.left);
    Operation *leftTail = tailOf(left);

    Operation *right = walkAst(bb, node->branches.right);
    Operation *rightTail = tailOf(right);

    op = newOperation(tokenToOp(node->op),
                      newRegisterOperand(leftTail->destination),
                      newRegisterOperand(rightTail->destination));

    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_PRINT: {
    walkAst(bb, node->expr);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(IR_PRINT, value, NULL);
    bb->curr->next = op;
    bb->curr = op;
    break;
  }
  case STMT_IF: {
    // TODO: tidy up implementation here
    Operation *expr = walkAst(bb, node->expr);
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
    walkAst(bb, node->branches.left);
    Operation *afterOp = newGotoOperation(afterLabelId);
    bb->curr->next = afterOp;
    bb->curr = afterOp;
    Operation *elseLabel = newLabelOperation(elseLabelId, IR_ELSE_LABEL);
    bb->curr->next = elseLabel;
    bb->curr = elseLabel;
    walkAst(bb, node->branches.right);
    Operation *afterLabel = newLabelOperation(afterLabelId, IR_LABEL);
    bb->curr->next = afterLabel;
    bb->curr = afterLabel;
    break;
  }
  case STMT_MODULE: {
    Node *stmtNode = (Node *)node->stmts->head;

    while (stmtNode != NULL) {
      // FIXME: create new BB per statement
      walkAst(bb, stmtNode->data);
      stmtNode = stmtNode->next;
    }
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

  walkAst(bb, node);

  return bb;
}

// TODO: make beautiful
CFG *constructCFG(BasicBlock *irList) {
  CFG *cfg = allocateCFG();
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

  return cfg;
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
  return "?";
}

char *opcodeString(IROp opcode) {
  switch (opcode) {
  case IR_ADD:
    return "+";
  case IR_ASSIGN:
    return "<-";
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
  case IR_NOT:
    return "!";
  case IR_PRINT:
    return "print";
  case IR_GOTO:
    return "goto";
  case IR_LABEL:
    return "label";
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

LinkedList *postOrderTraverse(BasicBlock *bb) {
  Table visitedSet;
  initTable(&visitedSet);

  LinkedList *ordered = linkedList_allocate();

  dfsWalk(bb, &visitedSet, ordered);

  freeTable(&visitedSet);

  return ordered;
}

void printBasicBlock(BasicBlock *bb) {
  Operation *curr = bb->ops;

  if (bb->labelId == -1) {
    printf("Basic block %llu (main)", bb->id);
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
      printf("%4llu: [       | %8s | %6s | %6s ]\n", curr->id,
             opcodeString(curr->opcode), operandString(curr->first),
             operandString(curr->second));
    } else {
      printf("%4llu: [ t%-4llu | %8s | %6s | %6s ]\n", curr->id,
             curr->destination, opcodeString(curr->opcode),
             operandString(curr->first), operandString(curr->second));
    }
    curr = curr->next;
    i++;
  }
}

void printCFG(CFG *cfg) {
  LinkedList *ordered = postOrderTraverse(cfg->start);
  Node *tail = ordered->tail;
  while (tail != NULL) {
    printBasicBlock(tail->data);
    tail = tail->prev;
  }
}

CFG *newCFG(AstNode *root) {
  BasicBlock *irList = newBasicBlock(root);

  CFG *cfg = constructCFG(irList);

  return cfg;
}
