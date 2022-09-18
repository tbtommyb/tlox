#ifndef clox_ast_h
#define clox_ast_h

#include "common.h"
#include "linked_list.h"
#include "scanner.h"
#include "scope.h"
#include "symbol_table.h"
#include "value.h"

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
  STMT_DEFINE_CONST,
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

typedef struct AstNode AstNode;

// Separate out Expr and Stmts and sub-types
struct AstNode {
  NodeType type;
  Value literal;
  TokenType op;
  AstNode *preExpr;
  AstNode *condExpr;
  AstNode *expr;
  AstNode *postExpr;
  struct {
    AstNode *left;
    AstNode *right;
  } branches;
  LinkedList *stmts;
  LinkedList *params;
  Token token;
  Token superclass;
  Scope *scope;
  int arity;
  FunctionType functionType;
  bool isPropertyInvocation;
};

static inline bool isNodeType(AstNode node, NodeType type) {
  return node.type == type;
}

AstNode *newAndExpr(Token token);
AstNode *newOrExpr(Token token);
AstNode *newLiteralExpr(Token token, Value value);
AstNode *newNilExpr(Token token);
AstNode *newBinaryExpr(Token token, AstNode *left, AstNode *right,
                       TokenType operator);
AstNode *newUnaryExpr(Token token, AstNode *right, TokenType operator);
AstNode *newVariableExpr(Token token);
AstNode *newFunctionExpr(Token token, FunctionType functionType);
AstNode *newCallExpr(Token token);
AstNode *newInvocationExpr(Token token);
AstNode *newGetPropertyExpr(Token token);
AstNode *newThisExpr(Token token);
AstNode *newClassStmt(Token token);
AstNode *newClassBodyStmt(Token token);
AstNode *newSuperExpr(Token method);
AstNode *newSuperInvocationExpr(Token method);
AstNode *newDefineStmt(Token token, AstNode *expr);
AstNode *newConstDefineStmt(Token token, AstNode *expr);
AstNode *newAssignStmt(Token token, AstNode *expr);
AstNode *newPrintStmt(Token token, AstNode *expr);
AstNode *newIfStmt(Token token, AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch);
AstNode *newWhileStmt(Token token, AstNode *condition, AstNode *thenBranch);
AstNode *newForStmt(Token token, AstNode *initNode, AstNode *conditionNode,
                    AstNode *postNode, AstNode *bodyNode);
AstNode *newModuleStmt(Token token);
AstNode *newMethodStmt(Token token, AstNode *expr);
AstNode *newBlockStmt(Token token);
AstNode *newFunctionStmt(Token token, AstNode *funcExpr);
AstNode *newReturnStmt(Token token, AstNode *expr);
AstNode *newExprStmt(Token token, AstNode *expr);
AstNode *newSetPropertyStmt(Token token, AstNode *expr);

void printAST(const AstNode *root, int indentation);

#endif
