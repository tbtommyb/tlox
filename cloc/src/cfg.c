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

  return op;
}

static Operation *tailOf(Operation *node) {
  Operation *tail = node;
  while (tail->next != NULL) {
    tail = tail->next;
  }
  return tail;
}

static Operation *walkAst(AstNode *node) {
  if (node == NULL) {
    return NULL;
  }
  if (node->type == EXPR_LITERAL) {
    Operand *value = newLiteralOperand(node->literal);

    Operation *op = newOperation(IR_ASSIGN, value, NULL);

    return op;
  }
  if (node->type == EXPR_UNARY) {
    Operation *right = walkAst(node->branches.right);
    Operation *rightTail = tailOf(right);

    Operand *value = newRegisterOperand(rightTail->destination);
    Operation *op = newOperation(tokenToUnaryOp(node->op), value, NULL);
    rightTail->next = op;

    return right;
  }
  if (node->type == EXPR_BINARY) {
    Operation *left = walkAst(node->branches.left);
    Operation *leftTail = tailOf(left);

    Operation *right = walkAst(node->branches.right);
    Operation *rightTail = tailOf(right);

    Operation *op = newOperation(tokenToOp(node->op),
                                 newRegisterOperand(leftTail->destination),
                                 newRegisterOperand(rightTail->destination));

    leftTail->next = right;
    rightTail->next = op;

    return left;
  }
  if (node->type == STMT_PRINT) {
    Operation *left = walkAst(node->branches.left);
    Operation *leftTail = tailOf(left);

    Operand *value = newRegisterOperand(leftTail->destination);
    Operation *op = newOperation(IR_PRINT, value, NULL);

    leftTail->next = op;

    return left;
  }
  return NULL;
}

BasicBlock *newBasicBlock(AstNode *node) {
  BasicBlock *bb = allocateBasicBlock();

  Operation *curr = walkAst(node);
  bb->ops = curr;

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

  while (curr != NULL) {
    printf("[ t%llu | %6s | %2s | %2s ]\n", curr->destination,
           opcodeString(curr->opcode), operandString(curr->first),
           operandString(curr->second));
    curr = curr->next;
  }
}

CFG *newCFG(AstNode *root) {
  CFG *cfg = allocateCFG();

  cfg->start = newBasicBlock(root);

  return cfg;
}
