#include "ast.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

static AstNode *allocateAstNode(NodeType type) {
  AstNode *node = (AstNode *)reallocate(NULL, 0, sizeof(AstNode));
  node->type = type;
  node->literal = EMPTY_VAL;
  node->op = TOKEN_UNKNOWN;
  node->branches.left = NULL;
  node->branches.right = NULL;

  return node;
}

AstNode *newLiteralExpr(Value value) {
  AstNode *node = allocateAstNode(EXPR_LITERAL);
  node->literal = value;
  return node;
}

AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType operator) {
  AstNode *node = allocateAstNode(EXPR_BINARY);
  node->branches.left = left;
  node->branches.right = right;
  node->op = operator;
  return node;
}

AstNode *newUnaryExpr(AstNode *right, TokenType operator) {
  AstNode *node = allocateAstNode(EXPR_UNARY);
  node->branches.right = right;
  node->op = operator;
  return node;
}

const char *tokenTypeStr(TokenType op) {
  if (op == TOKEN_BANG) {
    return "!";
  }
  if (op == TOKEN_MINUS) {
    return "-";
  }
  return "?";
}

void printAST(AstNode node, int indentation) {
  switch (node.type) {
  case EXPR_LITERAL: {
    printf("%*sExpr Lit: ", indentation, "");
    printValue(stdout, node.literal);
    printf("%*s\n", indentation, "");
    break;
  }
  case EXPR_UNARY: {
    printf("%*sExpr Unary\n", indentation, "");
    printf("%*sOp: %s\n", indentation, "", tokenTypeStr(node.op));
    printf("%*sRight:\n", indentation, "");
    printAST(*node.branches.right, indentation + 2);
  }
  case EXPR_BINARY: {
    printf("%*sExpr Binary\n", indentation, "");
    printf("%*sOp: %s\n", indentation, "", tokenTypeStr(node.op));
    printf("%*sLeft:\n", indentation, "");
    printAST(*node.branches.left, indentation + 2);
    printf("%*sRight:\n", indentation, "");
    printAST(*node.branches.right, indentation + 2);
    break;
  }
  }
}

/* void testAST() { */
/*   LiteralExpr *one = newLiteralExpr(NUMBER_VAL(1)); */
/*   LiteralExpr *two = newLiteralExpr(NUMBER_VAL(2)); */
/*   Token op; */
/*   op.type = TOKEN_STAR; */
/*   op.start = "*"; */
/*   op.length = 1; */
/*   op.line = 1; */
/*   BinaryExpr *mult = newBinaryExpr((Expr *)one, (Expr *)two, op); */

/*   LiteralExpr *three = newLiteralExpr(NUMBER_VAL(3)); */
/*   Token plus; */
/*   plus.type = TOKEN_PLUS; */
/*   plus.start = "+"; */
/*   plus.length = 1; */
/*   plus.line = 1; */
/*   BinaryExpr *add = newBinaryExpr((Expr *)three, (Expr *)mult, plus); */

/*   printAST(EXPR_NODE(add), 0); */
/* } */
