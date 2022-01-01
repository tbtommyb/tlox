#ifndef clox_cfg_h
#define clox_cfg_h

#include "ast.h"
#include "scanner.h"
#include "value.h"
#include <stdint.h>

typedef enum OperandType { OPERAND_LITERAL, OPERAND_REG } OperandType;

// TODO: is this useful?
typedef enum CfgOp { CFG_UNKNOWN, CFG_ASSIGN, CFG_ADD } CfgOp;

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
  CfgOp opcode;
  Register destination;
  Operand *first;
  Operand *second;
  Operation *next;
  Operation *prev;
};

typedef struct BasicBlock {
  Operation *ops;
} BasicBlock;

BasicBlock *newBasicBlock(AstNode *node);
Operation *newOperation();
Operand *newLiteralOperand(Value value);
Operand *newRegisterOperand(Register reg);
void printBasicBlock(BasicBlock *bb);

#endif
