#ifndef clox_ast_h
#define clox_ast_h

#include "common.h"
#include "linked_list.h"
#include "scanner.h"
#include "scope.h"
#include "symbol_table.h"
#include "value.h"

typedef enum {
  EXPR_BINARY,
  EXPR_CALL,
  EXPR_FUNCTION,
  EXPR_LITERAL,
  EXPR_UNARY,
  EXPR_VARIABLE,
  STMT_ASSIGN,
  STMT_BLOCK,
  STMT_DEFINE,
  STMT_DEFINE_CONST,
  STMT_EXPR,
  STMT_FUNCTION,
  STMT_IF,
  STMT_MODULE,
  STMT_PRINT,
  STMT_RETURN,
  STMT_WHILE
} NodeType;

typedef struct AstNode AstNode;

// TODO: add line numbers for errors
// Separate out Expr and Stmts and sub-types
struct AstNode {
  NodeType type;
  Value literal;
  TokenType op;
  AstNode *expr;
  struct {
    AstNode *left;
    AstNode *right;
  } branches;
  LinkedList *stmts;
  LinkedList *params;
  Token token;
  Scope *scope;
  int arity;
};

static inline bool isNodeType(AstNode node, NodeType type) {
  return node.type == type;
}

AstNode *newLiteralExpr(Value value);
AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType operator);
AstNode *newUnaryExpr(AstNode *right, TokenType operator);
AstNode *newVariableExpr(Token token);
AstNode *newFunctionExpr();
AstNode *newCallExpr(Token name);
AstNode *newDefineStmt(Token token, AstNode *expr);
AstNode *newConstDefineStmt(Token token, AstNode *expr);
AstNode *newAssignStmt(Token token, AstNode *expr);
AstNode *newPrintStmt(AstNode *expr);
AstNode *newIfStmt(AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch);
AstNode *newWhileStmt(AstNode *condition, AstNode *thenBranch);
AstNode *newModuleStmt();
AstNode *newBlockStmt();
AstNode *newFunctionStmt(Token name, AstNode *funcExpr);
AstNode *newReturnStmt(AstNode *expr);
AstNode *newExprStmt(AstNode *expr);

void printAST(AstNode root, int indentation);

#endif
