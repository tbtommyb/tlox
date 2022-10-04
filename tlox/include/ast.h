#ifndef clox_ast_h
#define clox_ast_h

#include "common.h"
#include "linked_list.h"
#include "scanner.h"
#include "scope.h"
#include "symbol_table.h"
#include "util.h"
#include "value.h"

typedef OPTIONAL(Token) OptionalToken;
OptionalToken optionalTokenInit();
void optionalTokenSet(OptionalToken *ot, Token value);

#define AS_AST_NODE(value) ((AstNode *)(value))
#define AST_NODE_TYPE(node) ((node)->type)
#define IS_BINARY_EXPR(value) isNodeType(value, EXPR_BINARY)
#define AS_BINARY_EXPR(value) ((BinaryExprAstNode *)(value))
#define IS_LITERAL_EXPR(value) isNodeType(value, EXPR_LITERAL)
#define AS_LITERAL_EXPR(value) ((LiteralExprAstNode *)(value))
#define IS_MODULE_STMT(value) isNodeType(value, STMT_MODULE)
#define AS_MODULE_STMT(value) ((ModuleStmtAstNode *)(value))
#define IS_EXPR_STMT(value) isNodeType(value, STMT_EXPR)
#define AS_EXPR_STMT(value) ((ExprStmtAstNode *)(value))
#define IS_PRINT_STMT(value) isNodeType(value, STMT_PRINT)
#define AS_PRINT_STMT(value) ((PrintStmtAstNode *)(value))
#define IS_NIL_EXPR(value) isNodeType(value, EXPR_NIL)
#define AS_NIL_EXPR(value) ((NilExprAstNode *)(value))
#define IS_UNARY_EXPR(value) isNodeType(value, EXPR_UNARY)
#define AS_UNARY_EXPR(value) ((UnaryExprAstNode *)(value))
#define IS_AND_EXPR(value) isNodeType(value, EXPR_AND)
#define AS_AND_EXPR(value) ((AndExprAstNode *)(value))
#define IS_OR_EXPR(value) isNodeType(value, EXPR_OR)
#define AS_OR_EXPR(value) ((OrExprAstNode *)(value))
#define IS_VARIABLE_EXPR(value) isNodeType(value, EXPR_VARIABLE)
#define AS_VARIABLE_EXPR(value) ((VariableExprAstNode *)(value))
#define IS_ASSIGN_STMT(value) isNodeType(value, STMT_ASSIGN)
#define AS_ASSIGN_STMT(value) ((AssignStmtAstNode *)(value))
#define IS_DEFINE_STMT(value) isNodeType(value, STMT_DEFINE)
#define AS_DEFINE_STMT(value) ((DefineStmtAstNode *)(value))
#define IS_BLOCK_STMT(value) isNodeType(value, STMT_BLOCK)
#define AS_BLOCK_STMT(value) ((BlockStmtAstNode *)(value))
#define IS_IF_STMT(value) isNodeType(value, STMT_IF)
#define AS_IF_STMT(value) ((IfStmtAstNode *)(value))
#define IS_WHILE_STMT(value) isNodeType(value, STMT_WHILE)
#define AS_WHILE_STMT(value) ((WhileStmtAstNode *)(value))
#define IS_FOR_STMT(value) isNodeType(value, STMT_FOR)
#define AS_FOR_STMT(value) ((ForStmtAstNode *)(value))
#define IS_FUNCTION_STMT(value) isNodeType(value, STMT_FUNCTION)
#define AS_FUNCTION_STMT(value) ((FunctionStmtAstNode *)(value))
#define IS_FUNCTION_EXPR(value) isNodeType(value, EXPR_FUNCTION)
#define AS_FUNCTION_EXPR(value) ((FunctionExprAstNode *)(value))
#define IS_CALL_EXPR(value) isNodeType(value, EXPR_CALL)
#define AS_CALL_EXPR(value) ((CallExprAstNode *)(value))
#define IS_RETURN_STMT(value) isNodeType(value, STMT_RETURN)
#define AS_RETURN_STMT(value) ((ReturnStmtAstNode *)(value))
#define IS_CLASS_STMT(value) isNodeType(value, STMT_CLASS)
#define AS_CLASS_STMT(value) ((ClassStmtAstNode *)(value))
#define IS_CLASS_BODY_STMT(value) isNodeType(value, STMT_CLASS_BODY)
#define AS_CLASS_BODY_STMT(value) ((ClassBodyStmtAstNode *)(value))
#define IS_METHOD_STMT(value) isNodeType(value, STMT_METHOD)
#define AS_METHOD_STMT(value) ((MethodStmtAstNode *)(value))
#define IS_GET_PROPERTY_EXPR(value) isNodeType(value, EXPR_GET_PROPERTY)
#define AS_GET_PROPERTY_EXPR(value) ((GetPropertyExprAstNode *)(value))
#define IS_SUPER_EXPR(value) isNodeType(value, EXPR_SUPER)
#define AS_SUPER_EXPR(value) ((SuperExprAstNode *)(value))
#define IS_SUPER_INVOCATION_EXPR(value) isNodeType(value, EXPR_SUPER_INVOCATION)
#define AS_SUPER_INVOCATION_EXPR(value) ((SuperInvocationExprAstNode *)(value))
#define IS_INVOCATION_EXPR(value) isNodeType(value, EXPR_INVOCATION)
#define AS_INVOCATION_EXPR(value) ((InvocationExprAstNode *)(value))
#define IS_THIS_EXPR(value) isNodeType(value, EXPR_THIS)
#define AS_THIS_EXPR(value) ((ThisExprAstNode *)(value))
#define IS_SET_PROPERTY_STMT(value) isNodeType(value, STMT_SET_PROPERTY)
#define AS_SET_PROPERTY_STMT(value) ((SetPropertyStmtAstNode *)(value))

typedef enum {
  EXPR_AND,
  EXPR_BINARY,
  EXPR_CALL,
  EXPR_FUNCTION,
  EXPR_GET_PROPERTY,
  EXPR_INVOKE,
  EXPR_LITERAL,
  EXPR_NIL,
  EXPR_OR,
  EXPR_SUPER,
  EXPR_SUPER_INVOKE,
  EXPR_THIS,
  EXPR_UNARY,
  EXPR_VARIABLE,
  STMT_ASSIGN,
  STMT_BLOCK,
  STMT_CLASS,
  STMT_CLASS_BODY,
  STMT_DEFINE,
  STMT_EXPR,
  STMT_FOR,
  STMT_FUNCTION,
  STMT_IF,
  STMT_METHOD,
  STMT_MODULE,
  STMT_PRINT,
  STMT_RETURN,
  STMT_SET_PROPERTY,
  STMT_WHILE
} NodeType;

typedef struct {
  NodeType type;
  Token token;
  Scope *scope;
} AstNode;

typedef struct {
  AstNode base;
  TokenType op;
  struct {
    AstNode *left;
    AstNode *right;
  } branches;
} BinaryExprAstNode;

typedef struct {
  AstNode base;
  Value literal;
} LiteralExprAstNode;

typedef struct {
  AstNode base;
  LinkedList *stmts;
} ModuleStmtAstNode;

typedef ModuleStmtAstNode BlockStmtAstNode;

typedef struct {
  AstNode base;
  AstNode *expr;
} ExprStmtAstNode;

typedef ExprStmtAstNode ReturnStmtAstNode;
typedef ExprStmtAstNode PrintStmtAstNode;
typedef ExprStmtAstNode AssignStmtAstNode;

typedef struct {
  AstNode base;
} NilExprAstNode;

typedef NilExprAstNode VariableExprAstNode;

typedef struct {
  AstNode base;
  TokenType op;
  AstNode *right;
} UnaryExprAstNode;

typedef struct {
  AstNode base;
  struct {
    AstNode *left;
    AstNode *right;
  } branches;
} AndExprAstNode;

typedef AndExprAstNode OrExprAstNode;

typedef BlockStmtAstNode ClassBodyStmtAstNode;

typedef struct {
  AstNode base;
  ClassBodyStmtAstNode *body;
  OptionalToken superclass;
} ClassStmtAstNode;

typedef struct {
  AstNode base;
  int arity;
  BlockStmtAstNode *body;
  FunctionType functionType;
  Token params[256];
} FunctionExprAstNode;

typedef struct {
  AstNode base;
  FunctionExprAstNode *expr;
} FunctionStmtAstNode;

typedef struct {
  AstNode base;
  FunctionExprAstNode *body;
} MethodStmtAstNode;

typedef struct {
  AstNode base;
  LinkedList *params;
  AstNode *target;
} CallExprAstNode;

typedef struct {
  AstNode base;
  AstNode *target;
} GetPropertyExprAstNode;

typedef struct {
  AstNode base;
  AstNode *expr;
  bool isConst;
} DefineStmtAstNode;

typedef struct {
  AstNode base;
  AstNode *condition;
  struct {
    AstNode *then;
    AstNode *elseB;
  } branches;
} IfStmtAstNode;

typedef struct {
  AstNode base;
  AstNode *condition;
  struct {
    AstNode *then;
  } branches;
} WhileStmtAstNode;

typedef struct {
  AstNode base;
  struct {
    AstNode *pre;
    AstNode *cond;
    AstNode *post;
    AstNode *body;
  } branches;
} ForStmtAstNode;

typedef struct {
  AstNode base;
  AstNode *target;
} SuperExprAstNode;

typedef struct {
  AstNode base;
  AstNode *target;
  LinkedList *params;
} SuperInvocationExprAstNode;

typedef struct {
  AstNode base;
  AstNode *target;
  LinkedList *params;
} InvocationExprAstNode;

typedef struct {
  AstNode base;
} ThisExprAstNode;

typedef struct {
  AstNode base;
  AstNode *target;
  AstNode *expr;
} SetPropertyStmtAstNode;

AndExprAstNode *newAndExpr(Token token);
OrExprAstNode *newOrExpr(Token token);
LiteralExprAstNode *newLiteralExpr(Token token, Value value);
NilExprAstNode *newNilExpr(Token token);
BinaryExprAstNode *newBinaryExpr(Token token, AstNode *left, AstNode *right,
                                 TokenType operator);
UnaryExprAstNode *newUnaryExpr(Token token, AstNode *right, TokenType operator);
VariableExprAstNode *newVariableExpr(Token token);
FunctionExprAstNode *newFunctionExpr(Token token, FunctionType functionType);
FunctionStmtAstNode *newFunctionStmt(Token token,
                                     FunctionExprAstNode *funcExpr);
CallExprAstNode *newCallExpr(Token token);
InvocationExprAstNode *newInvocationExpr(Token token);
GetPropertyExprAstNode *newGetPropertyExpr(Token token);
ThisExprAstNode *newThisExpr(Token token);
ClassStmtAstNode *newClassStmt(Token token);
ClassBodyStmtAstNode *newClassBodyStmt(Token token);
SuperExprAstNode *newSuperExpr(Token method);
SuperInvocationExprAstNode *newSuperInvocationExpr(Token method);
DefineStmtAstNode *newDefineStmt(Token token, bool isConst, AstNode *expr);
AssignStmtAstNode *newAssignStmt(Token token, AstNode *expr);
PrintStmtAstNode *newPrintStmt(Token token, AstNode *expr);
IfStmtAstNode *newIfStmt(Token token, AstNode *condition, AstNode *thenBranch,
                         AstNode *elseBranch);
WhileStmtAstNode *newWhileStmt(Token token, AstNode *condition,
                               AstNode *thenBranch);

ForStmtAstNode *newForStmt(Token token, AstNode *preNode,
                           AstNode *conditionNode, AstNode *postNode,
                           AstNode *bodyNode);
ModuleStmtAstNode *newModuleStmt(Token token);
MethodStmtAstNode *newMethodStmt(Token token, FunctionExprAstNode *expr);
BlockStmtAstNode *newBlockStmt(Token token);
ReturnStmtAstNode *newReturnStmt(Token token, AstNode *expr);
ExprStmtAstNode *newExprStmt(Token token, AstNode *expr);
SetPropertyStmtAstNode *newSetPropertyStmt(Token token, AstNode *expr);

void printAST(const AstNode *root, int indentation);

static inline bool isNodeType(AstNode *node, NodeType type) {
  return node->type == type;
}

#endif
