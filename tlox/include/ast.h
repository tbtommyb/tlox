#ifndef clox_ast_h
#define clox_ast_h

#include "linked_list.h"
#include "scanner.h"
#include "value.h"

typedef enum {
  EXPR_BINARY,
  EXPR_LITERAL,
  EXPR_UNARY,
  EXPR_VARIABLE,
  STMT_ASSIGN,
  STMT_BLOCK,
  STMT_DEFINE,
  STMT_DEFINE_CONST,
  STMT_IF,
  STMT_MODULE,
  STMT_PRINT
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
  Token token;
};

static inline bool isNodeType(AstNode node, NodeType type) {
  return node.type == type;
}

AstNode *newLiteralExpr(Value value);
AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType operator);
AstNode *newUnaryExpr(AstNode *right, TokenType operator);
AstNode *newVariableExpr(Token token);
AstNode *newDefineStmt(Token token, AstNode *expr);
AstNode *newConstDefineStmt(Token token, AstNode *expr);
AstNode *newAssignStmt(Token token, AstNode *expr);
AstNode *newPrintStmt(AstNode *expr);
AstNode *newIfStmt(AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch);
AstNode *newModuleStmt();
AstNode *newBlockStmt();

void printAST(AstNode root, int indentation);

#endif
