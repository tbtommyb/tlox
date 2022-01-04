#ifndef clox_cfg_h
#define clox_cfg_h

#include "ast.h"
#include "scanner.h"
#include "value.h"
#include <stdint.h>

typedef enum OperandType { OPERAND_LITERAL, OPERAND_REG } OperandType;

typedef enum IROp {
  IR_UNKNOWN,
  IR_ADD,
  IR_ASSIGN,
  IR_DIVIDE,
  IR_SUBTRACT,
  IR_MODULO,
  IR_MULTIPLY,
  IR_NEGATE,
  IR_NOT,
  IR_PRINT,
} IROp;

typedef uint64_t Register;

typedef struct Operand {
  OperandType type;
  union {
    Value literal;
    Register source;
  } val;
} Operand;

typedef struct Operation Operation;

struct Operation {
  IROp opcode;
  Register destination;
  Operand *first;
  Operand *second;
  Operation *next;
};

typedef struct BasicBlock {
  Operation *ops;
} BasicBlock;

typedef struct CFG {
  BasicBlock *start;
} CFG;

BasicBlock *newBasicBlock(AstNode *node);
CFG *newCFG(AstNode *root);
Operation *newOperation(IROp opcode, Operand *first, Operand *second);
Operand *newLiteralOperand(Value value);
Operand *newRegisterOperand(Register reg);
void printBasicBlock(BasicBlock *bb);

#endif
