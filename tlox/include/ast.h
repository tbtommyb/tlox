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
  EXPR_OR,
  EXPR_BINARY,
  EXPR_CALL,
  EXPR_FUNCTION,
  EXPR_GET_PROPERTY,
  EXPR_INVOKE,
  EXPR_LITERAL,
  EXPR_NIL,
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
  STMT_FUNCTION,
  STMT_FOR,
  STMT_IF,
  STMT_METHOD,
  STMT_MODULE,
  STMT_PRINT,
  STMT_RETURN,
  STMT_SET_PROPERTY,
  STMT_WHILE
} NodeType;

typedef struct AstNode AstNode;

// TODO: add line numbers for errors
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
  LinkedList *methods;
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

AstNode *newAndExpr();
AstNode *newOrExpr();
AstNode *newLiteralExpr(Value value);
AstNode *newNilExpr();
AstNode *newBinaryExpr(AstNode *left, AstNode *right, TokenType operator);
AstNode *newUnaryExpr(AstNode *right, TokenType operator);
AstNode *newVariableExpr(Token token);
AstNode *newFunctionExpr(FunctionType functionType);
AstNode *newCallExpr();
AstNode *newInvocationExpr(Token name);
AstNode *newGetPropertyExpr(Token name);
AstNode *newThisExpr(Token this);
AstNode *newClassStmt(Token name);
AstNode *newClassBodyStmt();
AstNode *newDefineStmt(Token token, AstNode *expr);
AstNode *newConstDefineStmt(Token token, AstNode *expr);
AstNode *newAssignStmt(Token token, AstNode *expr);
AstNode *newPrintStmt(AstNode *expr);
AstNode *newIfStmt(AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch);
AstNode *newWhileStmt(AstNode *condition, AstNode *thenBranch);
AstNode *newForStmt(AstNode *initNode, AstNode *conditionNode,
                    AstNode *postNode, AstNode *bodyNode);
AstNode *newModuleStmt();
AstNode *newMethodStmt(Token name, AstNode *expr);
AstNode *newBlockStmt();
AstNode *newFunctionStmt(Token name, AstNode *funcExpr);
AstNode *newReturnStmt(AstNode *expr);
AstNode *newExprStmt(AstNode *expr);
AstNode *newSetPropertyStmt(Token property, AstNode *expr);

void printAST(AstNode root, int indentation);

#endif
