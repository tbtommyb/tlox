#ifndef clox_cfg_h
#define clox_cfg_h

#include "ast.h"
#include "compiler.h"
#include "execution_context.h"
#include "scanner.h"
#include "scope.h"
#include "value.h"
#include <stdint.h>

typedef enum OperandType {
  OPERAND_LITERAL,
  OPERAND_REG,
  OPERAND_LABEL,
  OPERAND_SYMBOL
} OperandType;

typedef enum IROp {
  IR_UNKNOWN,
  IR_ADD,
  IR_CALL,
  IR_CONSTANT,
  IR_COND,
  IR_COND_NO_POP,
  IR_DIVIDE,
  IR_BEGIN_SCOPE,
  IR_END_SCOPE,
  IR_EQUAL,
  IR_FUNCTION,
  IR_GREATER,
  IR_GREATER_EQUAL,
  IR_LESS,
  IR_LESS_EQUAL,
  IR_LOOP,
  IR_SUBTRACT,
  IR_MODULO,
  IR_MULTIPLY,
  IR_NEGATE,
  IR_NIL,
  IR_NOT,
  IR_NOT_EQUAL,
  IR_POP,
  IR_PRINT,
  IR_RETURN,
  IR_CODE_START,
  IR_GOTO,
  IR_LABEL,
  IR_ELSE_LABEL,
  IR_DEFINE_GLOBAL,
  IR_DEFINE_LOCAL,
  IR_GET_GLOBAL,
  IR_GET_LOCAL,
  IR_SET_GLOBAL,
  IR_SET_LOCAL,
  IR_STMT_EXPR
} IROp;

typedef uint64_t Register;
typedef uint64_t BasicBlockId;
typedef uint64_t OperationId;
typedef int64_t LabelId; // TODO: make 16_t

typedef struct Operand {
  OperandType type;
  union {
    Value literal;
    Register source;
    LabelId label;
    Symbol symbol;
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
  LabelId labelId;
  int opsCount;    // Keep this?
  Operation *ops;  // Redo as LinkedList
  Operation *curr; // Keep this?
  struct BasicBlock *trueEdge;
  struct BasicBlock *falseEdge;
} BasicBlock;

typedef struct CFG {
  BasicBlock *start;
  Token name;
  ExecutionContext *context;
  LinkedList *childFunctions; // move to wu
  int arity;                  // temp location
} CFG;

typedef struct WorkUnit {
  ExecutionContext *enclosing;
  AstNode *node;
  Token name;
  CFG *cfg;
  ObjFunction *f;
} WorkUnit;

BasicBlock *newBasicBlock(AstNode *node);
Operation *newOperation(IROp opcode, Operand *first, Operand *second);
WorkUnit *createWorkUnit(Compiler *compiler, AstNode *root);

LinkedList *postOrderTraverseBasicBlock(CFG *cfg);
void printCFG(CFG *cfg);
void printWorkUnits(WorkUnit *root);

#endif
