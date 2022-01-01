#ifndef clox_ast_h
#define clox_ast_h

#include "scanner.h"
#include "value.h"

typedef enum { EXPR_BINARY, EXPR_LITERAL, EXPR_UNARY } NodeType;

typedef struct AstNode AstNode;

// TODO: add line numbers for errors
struct AstNode {
  NodeType type;
  Value literal;
  TokenType op; // replace me.
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

void printAST(AstNode root, int indentation);

#endif
