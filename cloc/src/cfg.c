#include "cfg.h"
#include "memory.h"
#include "scanner.h"

#include <stdlib.h>

static Register currentRegister = 0;

static Register getRegister() { return currentRegister++; }

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
  bb->ops = NULL;

  return bb;
}

static Operand *allocateOperand(OperandType type) {
  Operand *operand = (Operand *)reallocate(NULL, 0, sizeof(Operand));
  operand->type = type;

  return operand;
}

static CfgOp tokenToCfgOp(TokenType token) {
  switch (token) {
  case TOKEN_PLUS:
    return CFG_ADD;
  default:
    return CFG_UNKNOWN;
  }
}

Operand *newLiteralOperand(Value value) {
  Operand *operand = allocateOperand(OPERAND_LITERAL);
  operand->val.literal = value;

  return operand;
}

Operation *newOperation(CfgOp opcode, Operand *first, Operand *second) {
  Operation *op = allocateOperation();

  op->destination = getRegister();
  op->opcode = opcode;
  op->first = first;
  op->second = second;

  return op;
}

static Operation *walkAst(AstNode *node) {
  if (node->type == EXPR_LITERAL) {
    Operand *value = newLiteralOperand(node->literal);
    Operation *op = newOperation(CFG_ASSIGN, value, NULL);
    return op;
  }
  return NULL;
}

BasicBlock *newBasicBlock(AstNode *node) {
  BasicBlock *bb = allocateBasicBlock();

  bb->ops = walkAst(node);

  return bb;
}

char *operandString(Operand *operand) {
  if (operand == NULL) {
    return "";
  }
  if (operand->type == OPERAND_LITERAL) {
    return valueToString(operand->val.literal);
  }
  return "?";
}

char *opcodeString(CfgOp opcode) {
  switch (opcode) {
  case CFG_ASSIGN:
    return "<-";
  case CFG_ADD:
    return "+";
  case CFG_UNKNOWN:
  default:
    return "?";
  }
}

void printBasicBlock(BasicBlock *bb) {
  Operation *curr = bb->ops;

  while (curr != NULL) {
    printf("[ t%llu | %s | %s | %s ]\n", curr->destination,
           opcodeString(curr->opcode), operandString(curr->first),
           operandString(curr->second));
    curr = curr->next;
  }
}
