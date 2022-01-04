#ifndef clox_ast_h
#define clox_ast_h

#include "scanner.h"
#include "value.h"

typedef enum { EXPR_BINARY, EXPR_LITERAL, EXPR_UNARY, STMT_PRINT } NodeType;

typedef struct AstNode AstNode;

// TODO: add line numbers for errors
// Separate out Expr and Stmts and sub-types
struct AstNode {
  NodeType type;
  Value literal;
  TokenType op;
  struct {
    AstNode *left;
    AstNode *right;
  } branches;
};

static inline bool isNodeType(AstNode node, NodeType type) {
  return node.type == type;
}

AstNode *newLiteralExpr(Value value);
AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType operator);
AstNode *newUnaryExpr(AstNode *right, TokenType operator);
AstNode *newPrintStmt(AstNode *expr);

void printAST(AstNode root, int indentation);

#endif
