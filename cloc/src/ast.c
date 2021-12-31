#include "ast.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

#define ALLOCATE_EXPR(type, exprType)                                          \
  (type *)allocateExpr(sizeof(type), exprType)

static Expr *allocateExpr(size_t size, ExprType type) {
  Expr *expr = (Expr *)reallocate(NULL, 0, size);
  expr->type = type;
  return expr;
}

LiteralExpr *newLiteralExpr(Value value) {
  LiteralExpr *literal = ALLOCATE_EXPR(LiteralExpr, EXPR_LITERAL);
  literal->value = value;
  return literal;
}

BinaryExpr *newBinaryExpr(Expr *left, Expr *right, Token operator) {
  BinaryExpr *binary = ALLOCATE_EXPR(BinaryExpr, EXPR_BINARY);
  binary->left = left;
  binary->right = right;
  binary->op = operator;
  return binary;
}

void printAST(Node node, int indentation) {
  switch (NODE_TYPE(node)) {
  case NODE_EXPR:
    switch (EXPR_TYPE(node)) {
    case EXPR_LITERAL:
      printf("%*sExpr Lit: ", indentation, "");
      printValue(stdout, AS_LITERAL(node)->value);
      printf("%*s\n", indentation, "");
      break;
    case EXPR_BINARY: {
      BinaryExpr *expr = AS_BINARY(node);
      printf("%*sExpr Binary\n", indentation, "");
      printf("%*sOp: %.*s\n", indentation, "", expr->op.length, expr->op.start);
      printf("%*sLeft:\n", indentation, "");
      printAST(EXPR_NODE(expr->left), indentation + 2);
      printf("%*sRight:\n", indentation, "");
      printAST(EXPR_NODE(expr->right), indentation + 2);
      break;
    }
    }
  }
}

void testAST() {
  LiteralExpr *one = newLiteralExpr(NUMBER_VAL(1));
  LiteralExpr *two = newLiteralExpr(NUMBER_VAL(2));
  Token op;
  op.type = TOKEN_STAR;
  op.start = "*";
  op.length = 1;
  op.line = 1;
  BinaryExpr *mult = newBinaryExpr((Expr *)one, (Expr *)two, op);

  LiteralExpr *three = newLiteralExpr(NUMBER_VAL(3));
  Token plus;
  plus.type = TOKEN_PLUS;
  plus.start = "+";
  plus.length = 1;
  plus.line = 1;
  BinaryExpr *add = newBinaryExpr((Expr *)three, (Expr *)mult, plus);

  printAST(EXPR_NODE(add), 0);
}
