#include "ast.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

static AstNode *allocateAstNode(NodeType type) {
  AstNode *node = (AstNode *)reallocate(NULL, 0, sizeof(AstNode));
  node->type = type;
  node->literal = EMPTY_VAL;
  node->op = TOKEN_UNKNOWN;
  node->expr = NULL;
  node->branches.left = NULL;
  node->branches.right = NULL;

  return node;
}

AstNode *newLiteralExpr(Value value) {
  AstNode *node = allocateAstNode(EXPR_LITERAL);
  node->literal = value;
  return node;
}

AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType op) {
  AstNode *node = allocateAstNode(EXPR_BINARY);
  node->branches.left = left;
  node->branches.right = right;
  node->op = op;
  return node;
}

AstNode *newUnaryExpr(AstNode *right, TokenType op) {
  AstNode *node = allocateAstNode(EXPR_UNARY);
  node->branches.right = right;
  node->op = op;
  return node;
}

AstNode *newPrintStmt(AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_PRINT);
  node->expr = expr;
  return node;
}

AstNode *newIfStmt(AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch) {
  AstNode *node = allocateAstNode(STMT_IF);
  node->expr = condition;
  node->branches.left = thenBranch;
  node->branches.right = elseBranch;
  return node;
}

const char *tokenTypeStr(TokenType op) {
  switch (op) {
  case TOKEN_BANG:
    return "!";
  case TOKEN_PLUS:
    return "+";
  case TOKEN_MINUS:
    return "-";
  case TOKEN_SLASH:
    return "/";
  case TOKEN_STAR:
    return "*";
  case TOKEN_PERCENT:
    return "%";
  default:
    return "?";
  }
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
    break;
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
  case STMT_IF: {
    printf("%*sStmt If\n", indentation, "");
    printf("%*sCondition:\n", indentation, "");
    printAST(*node.expr, indentation + 2);
    printf("%*sThen:\n", indentation, "");
    printAST(*node.branches.left, indentation + 2);
    if (node.branches.right) {
      printf("%*sElse:\n", indentation, "");
      printAST(*node.branches.right, indentation + 2);
    }
    break;
  }
  case STMT_PRINT: {
    printf("%*sStmt Print\n", indentation, "");
    printf("%*sExpr:\n", indentation, "");
    printAST(*node.expr, indentation + 2);
    break;
  }
  }
}
