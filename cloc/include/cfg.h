#ifndef clox_cfg_h
#define clox_cfg_h

#include "ast.h"
#include "scanner.h"
#include "value.h"
#include <stdint.h>

typedef enum OperandType { OPERAND_LITERAL, OPERAND_REG } OperandType;

typedef enum IROp {
  CFG_UNKNOWN,
  CFG_ADD,
  CFG_ASSIGN,
  CFG_DIVIDE,
  CFG_SUBTRACT,
  CFG_MODULO,
  CFG_MULTIPLY,
  CFG_NEGATE,
  CFG_NOT,
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
  Operation *prev;
};

typedef struct BasicBlock {
  Operation *ops;
} BasicBlock;

typedef struct CFG {
  BasicBlock *start;
} CFG;

BasicBlock *newBasicBlock(AstNode *node);
CFG *newCFG(AstNode *root);
Operation *newOperation(IROp opcode, Operand *first, Operand *second,
                        Operation *prev);
Operand *newLiteralOperand(Value value);
Operand *newRegisterOperand(Register reg);
void printBasicBlock(BasicBlock *bb);

#endif
