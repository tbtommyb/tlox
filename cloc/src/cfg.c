#include "cfg.h"
#include "memory.h"
#include "scanner.h"

#include <stdlib.h>

BasicBlock *newBasicBlock(AstNode *node);

// TODO: make thread safe
static Register currentRegister = 0;
static BasicBlockId currentBasicBlockId = 0;

static Register getRegister() { return currentRegister++; }
static BasicBlockId getBasicBlockId() { return currentBasicBlockId++; }

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

// Make a separate function for statements that doesn't output to register?
Operation *newOperation(IROp opcode, Operand *first, Operand *second) {
  Operation *op = allocateOperation();

  op->destination = getRegister();
  op->opcode = opcode;
  op->first = first;
  op->second = second;
  op->next = NULL;

  return op;
}

static Operation *newStartOperation() {
  Operation *op = allocateOperation();

  op->destination = 0;
  op->opcode = IR_CODE_START;
  op->first = NULL;
  op->second = NULL;
  op->next = NULL;

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
    break;
  }
  case EXPR_UNARY: {
    Operation *right = walkAst(bb, node->branches.right);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(tokenToUnaryOp(node->op), value, NULL);
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

    break;
  }
  case STMT_PRINT: {
    walkAst(bb, node->expr);
    Operand *value = newRegisterOperand(bb->curr->destination);
    op = newOperation(IR_PRINT, value, NULL);
    break;
  }
  case STMT_IF: {
    Operation *expr = walkAst(bb, node->expr);
    op = newOperation(IR_COND, newRegisterOperand(bb->curr->destination), NULL);
    bb->trueEdge = newBasicBlock(node->branches.left);
    bb->falseEdge = newBasicBlock(node->branches.right);
    break;
  }
  }
  bb->curr->next = op;
  bb->curr = op;
  return op;
}

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
  case IR_UNKNOWN:
  default:
    return "?";
  }
}

void printBasicBlock(BasicBlock *bb) {
  Operation *curr = bb->ops;

  printf("Basic block %llu", bb->id);
  if (bb->trueEdge) {
    printf(" | Edges: %llu", bb->trueEdge->id);
  }
  if (bb->falseEdge) {
    printf(", %llu", bb->falseEdge->id);
  }
  printf("\n");
  printf("Op count: %d\n", bb->opsCount);
  while (curr != NULL) {
    printf("[ t%llu | %8s | %6s | %6s ]\n", curr->destination,
           opcodeString(curr->opcode), operandString(curr->first),
           operandString(curr->second));
    curr = curr->next;
  }
  if (bb->trueEdge != NULL) {
    printBasicBlock(bb->trueEdge);
  }
  if (bb->falseEdge != NULL) {
    printBasicBlock(bb->falseEdge);
  }
}

CFG *newCFG(AstNode *root) {
  CFG *cfg = allocateCFG();

  cfg->start = newBasicBlock(root);

  return cfg;
}
