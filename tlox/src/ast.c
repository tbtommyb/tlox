#include "ast.h"
#include "compiler.h"
#include "linked_list.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

// TODO: integrate with garbage collector
static AstNode *allocateAstNode(Token token, NodeType type) {
  AstNode *node = (AstNode *)reallocate(NULL, 0, sizeof(AstNode));
  node->type = type;
  node->token = token;
  node->literal = EMPTY_VAL;
  node->op = TOKEN_UNKNOWN;
  node->expr = NULL;
  node->branches.left = NULL;
  node->branches.right = NULL;
  node->stmts = NULL;
  node->params = NULL;
  node->arity = 0;
  node->scope = NULL;

  // TODO: these empty values are code smell
  node->superclass = (Token){0};
  node->functionType = (FunctionType){0};

  return node;
}

AstNode *newLiteralExpr(Token token, Value value) {
  AstNode *node = allocateAstNode(token, EXPR_LITERAL);
  node->literal = value;
  return node;
}

AstNode *newNilExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_NIL);
  node->token = token;
  return node;
}

AstNode *newBinaryExpr(Token token, AstNode *left, AstNode *right,
                       TokenType op) {
  AstNode *node = allocateAstNode(token, EXPR_BINARY);
  node->branches.left = left;
  node->branches.right = right;
  node->op = op;
  return node;
}

AstNode *newUnaryExpr(Token token, AstNode *right, TokenType op) {
  AstNode *node = allocateAstNode(token, EXPR_UNARY);
  node->branches.right = right;
  node->op = op;
  return node;
}

AstNode *newAndExpr(Token token) { return allocateAstNode(token, EXPR_AND); }

AstNode *newOrExpr(Token token) { return allocateAstNode(token, EXPR_OR); }

AstNode *newVariableExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_VARIABLE);
  node->token = token;
  return node;
}

AstNode *newFunctionExpr(Token token, FunctionType functionType) {
  AstNode *node = allocateAstNode(token, EXPR_FUNCTION);
  node->params = linkedList_allocate();
  node->functionType = functionType;
  return node;
}

AstNode *newCallExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_CALL);
  node->params = linkedList_allocate();
  return node;
}

AstNode *newInvocationExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_INVOKE);
  node->params = linkedList_allocate();
  return node;
}

AstNode *newThisExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_THIS);
  return node;
}

AstNode *newSuperInvocationExpr(Token method) {
  AstNode *node = allocateAstNode(method, EXPR_SUPER_INVOKE);
  node->params = linkedList_allocate();
  return node;
}

AstNode *newSuperExpr(Token method) {
  AstNode *node = allocateAstNode(method, EXPR_SUPER);
  return node;
}

AstNode *newGetPropertyExpr(Token token) {
  AstNode *node = allocateAstNode(token, EXPR_GET_PROPERTY);
  return node;
}

AstNode *newDefineStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_DEFINE);
  node->expr = expr;
  return node;
}

AstNode *newAssignStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_ASSIGN);
  node->expr = expr;
  return node;
}

AstNode *newConstDefineStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_DEFINE_CONST);
  node->expr = expr;
  return node;
}

AstNode *newPrintStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_PRINT);
  node->expr = expr;
  return node;
}

AstNode *newIfStmt(Token token, AstNode *condition, AstNode *thenBranch,
                   AstNode *elseBranch) {
  AstNode *node = allocateAstNode(token, STMT_IF);
  node->expr = condition;
  node->branches.left = thenBranch;
  node->branches.right = elseBranch;
  return node;
}

AstNode *newWhileStmt(Token token, AstNode *condition, AstNode *thenBranch) {
  AstNode *node = allocateAstNode(token, STMT_WHILE);
  node->expr = condition;
  node->branches.left = thenBranch;
  return node;
}

AstNode *newForStmt(Token token, AstNode *initNode, AstNode *conditionNode,
                    AstNode *postNode, AstNode *bodyNode) {

  AstNode *node = allocateAstNode(token, STMT_FOR);
  node->preExpr = initNode;
  node->condExpr = conditionNode;
  node->postExpr = postNode;
  node->expr = bodyNode;
  return node;
}

AstNode *newModuleStmt(Token token) {
  // FIXME: do we need to manually make something "main"?
  AstNode *node = allocateAstNode(token, STMT_MODULE);
  node->stmts = linkedList_allocate();
  return node;
}

AstNode *newBlockStmt(Token token) {
  AstNode *node = allocateAstNode(token, STMT_BLOCK);
  node->stmts = linkedList_allocate();
  return node;
}

AstNode *newFunctionStmt(Token token, AstNode *body) {
  AstNode *node = allocateAstNode(token, STMT_FUNCTION);
  node->expr = body;
  return node;
}

AstNode *newReturnStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_RETURN);
  node->expr = expr;
  return node;
}

AstNode *newExprStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_EXPR);
  node->expr = expr;
  return node;
}

AstNode *newMethodStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_METHOD);
  node->expr = expr;
  return node;
}

AstNode *newClassStmt(Token token) {
  AstNode *node = allocateAstNode(token, STMT_CLASS);
  return node;
}

AstNode *newClassBodyStmt(Token token) {
  AstNode *node = allocateAstNode(token, STMT_CLASS_BODY);
  node->stmts = linkedList_allocate();
  return node;
}

AstNode *newSetPropertyStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(token, STMT_SET_PROPERTY);
  node->expr = expr;
  return node;
}

char *tokenTypeStr(TokenType op) {
  switch (op) {
  case TOKEN_BANG:
    return "!";
  case TOKEN_BANG_EQUAL:
    return "!=";
  case TOKEN_EQUAL_EQUAL:
    return "==";
  case TOKEN_GREATER:
    return ">";
  case TOKEN_GREATER_EQUAL:
    return ">";
  case TOKEN_LESS:
    return "<";
  case TOKEN_LESS_EQUAL:
    return "<=";
  case TOKEN_MINUS:
    return "-";
  case TOKEN_PERCENT:
    return "%";
  case TOKEN_PLUS:
    return "+";
  case TOKEN_SLASH:
    return "/";
  case TOKEN_STAR:
    return "*";
  default:
    return "?";
  }
}

static void printArgumentList(const Node *argument, int indentation) {
  if (argument == NULL) {
    printf("%*sArguments: ()\n", indentation + 2, "");
    return;
  }
  printf("%*sArguments:\n", indentation + 2, "");
  while (argument != NULL) {
    printAST(argument->data, indentation + 4);
    argument = argument->next;
  }
}

static void printParameterList(const Node *parameter, int indentation) {
  while (parameter != NULL) {
    Token *name = parameter->data;
    printf("%.*s", name->length, name->start);
    parameter = parameter->next;
    if (parameter != NULL) {
      printf(", ");
    }
  }
  printf("\n");
}

static void printStatementList(const Node *statement, int indentation) {
  while (statement != NULL) {
    printAST(statement->data, indentation + 2);
    statement = statement->next;
  }
}

void printAST(const AstNode *node, int indentation) {
  if (node == NULL) {
    printf("%*s<nil>\n", indentation, "");
    return;
  }

  switch (node->type) {
  case EXPR_AND: {
    printf("%*sExpr And:\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(node->branches.right, indentation + 4);
    break;
  }
  case EXPR_BINARY: {
    printf("%*sExpr Binary:\n", indentation, "");
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(node->op));
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(node->branches.right, indentation + 4);
    break;
  }
  case EXPR_CALL: {
    printf("%*sExpr Call:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printArgumentList(node->params->head, indentation);
    break;
  }
  case EXPR_FUNCTION: {
    printf("%*sParameters: ", indentation, "");
    printParameterList(node->params->head, indentation);
    printf("%*sBody:\n", indentation, "");
    printAST(node->expr, indentation + 2);
    break;
  }
  case EXPR_GET_PROPERTY: {
    printf("%*sExpr GetProperty:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    break;
  }
  case EXPR_INVOKE: {
    printf("%*sExpr Invoke:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sMethod: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printArgumentList(node->params->head, indentation);
    break;
  }
  case EXPR_LITERAL: {
    printf("%*sExpr Literal: ", indentation, "");
    printValue(stdout, node->literal);
    printf("\n");
    break;
  }
  case EXPR_NIL: {
    printf("%*sExpr: Nil\n", indentation, "");
    break;
  }
  case EXPR_OR: {
    printf("%*sExpr Or:\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(node->branches.right, indentation + 4);
    break;
  }
  case EXPR_UNARY: {
    printf("%*sExpr Unary\n", indentation, "");
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(node->op));
    printf("%*sRight:\n", indentation + 2, "");
    printAST(node->branches.right, indentation + 4);
    break;
  }
  case EXPR_SUPER: {
    printf("%*sExpr Super:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    break;
  }
  case EXPR_SUPER_INVOKE: {
    printf("%*sExpr SuperInvoke:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printArgumentList(node->params->head, indentation);
    break;
  }
  case EXPR_THIS: {
    printf("%*sExpr: This\n", indentation, "");
    break;
  }
  case EXPR_VARIABLE: {
    printf("%*sExpr Variable: %.*s\n", indentation, "", node->token.length,
           node->token.start);
    break;
  }
  case STMT_ASSIGN: {
    printf("%*sStmt Assign:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sValue:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_BLOCK: {
    printf("%*sStmt Block:\n", indentation, "");
    printStatementList(node->stmts->head, indentation);
    break;
  }
  case STMT_CLASS: {
    printf("%*sStmt Class:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    if (node->superclass.length > 0) {
      printf("%*sSuperclass: %.*s\n", indentation + 2, "",
             node->superclass.length, node->superclass.start);
    }
    printAST(node->expr, indentation + 2);
    break;
  }
  case STMT_CLASS_BODY: {
    printf("%*sStmt ClassBody:\n", indentation, "");
    printStatementList(node->stmts->head, indentation);
    break;
  }
  case STMT_DEFINE: {
    printf("%*sStmt Define:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sValue:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_DEFINE_CONST: {
    printf("%*sStmt DefineConst:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sValue:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_EXPR: {
    printf("%*sStmt Expr:\n", indentation, "");
    printAST(node->expr, indentation + 2);
    break;
  }
  case STMT_FOR: {
    printf("%*sStmt For:\n", indentation, "");
    printf("%*sPre:\n", indentation + 2, "");
    printAST(node->preExpr, indentation + 4);
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(node->condExpr, indentation + 4);
    printf("%*sPost:\n", indentation + 2, "");
    printAST(node->postExpr, indentation + 4);
    printf("%*sBody:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_FUNCTION: {
    printf("%*sStmt Function:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_IF: {
    printf("%*sStmt If:\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sElse:\n", indentation + 2, "");
    printAST(node->branches.right, indentation + 4);
    break;
  }
  case STMT_METHOD: {
    printf("%*sStmt Method:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_MODULE: {
    printf("%*sStmt Module:\n", indentation, "");
    printStatementList(node->stmts->head, indentation);
    break;
  }
  case STMT_PRINT: {
    printf("%*sStmt Print:\n", indentation, "");
    printAST(node->expr, indentation + 2);
    break;
  }
  case STMT_RETURN: {
    printf("%*sStmt Return:\n", indentation, "");
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_SET_PROPERTY: {
    printf("%*sStmt SetProperty:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    break;
  }
  case STMT_WHILE: {
    printf("%*sStmt While:\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(node->expr, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(node->branches.left, indentation + 4);
    break;
  }
  }
}
