#ifndef clox_ast_h
#define clox_ast_h

#include "scanner.h"
#include "value.h"

typedef enum { NODE_EXPR } NodeType;

typedef enum { EXPR_BINARY, EXPR_LITERAL } ExprType;

typedef struct Expr Expr;

#define NODE_TYPE(node) ((node).type)
#define EXPR_TYPE(node) (AS_EXPR(node)->type)

#define IS_EXPR(node) ((node).type == NODE_EXPR)
#define AS_EXPR(node) ((node).as.expr)

#define IS_BINARY(node) isExprType(node, EXPR_BINARY)
#define IS_LITERAL(node) isExprType(node, EXPR_LITERAL)

#define AS_BINARY(node) ((BinaryExpr *)AS_EXPR(node))
#define AS_LITERAL(node) ((LiteralExpr *)AS_EXPR(node))

#define EXPR_NODE(node) ((Node){NODE_EXPR, {.expr = (Expr *)node}})

typedef struct {
  NodeType type;
  union {
    Expr *expr;
  } as;
} Node;

struct Expr {
  ExprType type;
};

typedef struct {
  Expr expr;
  Expr *left;
  Expr *right;
  Token op;
} BinaryExpr;

typedef struct {
  Expr expr;
  Value value;
} LiteralExpr;

static inline bool isExprType(Node node, ExprType type) {
  return IS_EXPR(node) && AS_EXPR(node)->type == type;
}

LiteralExpr *newLiteralExpr(Value value);
BinaryExpr *newBinaryExpr(Expr *left, Expr *right, Token operator);

void printAST(Node root, int indentation);
void testAST();

#endif
