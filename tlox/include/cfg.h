#ifndef clox_cfg_h
#define clox_cfg_h

#include "ast.h"
#include "scanner.h"
#include "value.h"
#include <stdint.h>

typedef enum OperandType {
  OPERAND_LITERAL,
  OPERAND_REG,
  OPERAND_LABEL
} OperandType;

typedef enum IROp {
  IR_UNKNOWN,
  IR_ADD,
  IR_ASSIGN,
  IR_COND,
  IR_DIVIDE,
  IR_SUBTRACT,
  IR_MODULO,
  IR_MULTIPLY,
  IR_NEGATE,
  IR_NOT,
  IR_PRINT,
  IR_CODE_START,
  IR_GOTO,
  IR_LABEL
} IROp;

typedef uint64_t Register;
typedef uint64_t BasicBlockId;
typedef uint64_t OperationId;
typedef uint64_t LabelId; // TODO: make 16_t

typedef struct Operand {
  OperandType type;
  union {
    Value literal;
    Register source;
    LabelId label;
  } val;
} Operand;

typedef struct Operation Operation;

// TODO: create different Operation types
struct Operation {
  OperationId id;
  IROp opcode;
  Register destination;
  Operand *first;
  Operand *second;
  Operation *next;
};

typedef struct BasicBlock {
  BasicBlockId id;
  int opsCount;
  Operation *ops;
  Operation *curr;
  struct BasicBlock *trueEdge;
  struct BasicBlock *falseEdge;
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
