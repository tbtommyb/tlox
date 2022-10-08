#include "ast.h"
#include "compiler.h"
#include "linked_list.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

#define ALLOCATE_NODE(type, token, nodeType)                                   \
  (type *)allocateAstNode(sizeof(type), token, nodeType)

static AstNode *allocateAstNode(size_t size, Token token, NodeType type) {
  AstNode *node = (AstNode *)reallocate(NULL, 0, size);
  node->type = type;
  node->token = token;
  node->scope = NULL;
  return node;
}

LiteralExprAstNode *newLiteralExpr(Token token, Value value) {
  LiteralExprAstNode *node =
      ALLOCATE_NODE(LiteralExprAstNode, token, EXPR_LITERAL);
  node->literal = value;
  return node;
}

NilExprAstNode *newNilExpr(Token token) {
  NilExprAstNode *node = ALLOCATE_NODE(NilExprAstNode, token, EXPR_NIL);
  return node;
}

BinaryExprAstNode *newBinaryExpr(Token token, AstNode *left, AstNode *right,
                                 TokenType op) {
  BinaryExprAstNode *node =
      ALLOCATE_NODE(BinaryExprAstNode, token, EXPR_BINARY);
  node->branches.left = left;
  node->branches.right = right;
  node->op = op;
  return node;
}

ModuleStmtAstNode *newModuleStmt(Token token) {
  ModuleStmtAstNode *node =
      ALLOCATE_NODE(ModuleStmtAstNode, token, STMT_MODULE);
  node->stmts = linkedList_allocate();
  return node;
}

PrintStmtAstNode *newPrintStmt(Token token, AstNode *expr) {
  PrintStmtAstNode *node = ALLOCATE_NODE(PrintStmtAstNode, token, STMT_PRINT);
  node->expr = expr;
  return node;
}

UnaryExprAstNode *newUnaryExpr(Token token, AstNode *right, TokenType op) {
  UnaryExprAstNode *node = ALLOCATE_NODE(UnaryExprAstNode, token, EXPR_UNARY);
  node->right = right;
  node->op = op;
  return node;
}

AndExprAstNode *newAndExpr(Token token) {
  return ALLOCATE_NODE(AndExprAstNode, token, EXPR_AND);
}

OrExprAstNode *newOrExpr(Token token) {
  return ALLOCATE_NODE(OrExprAstNode, token, EXPR_OR);
}

VariableExprAstNode *newVariableExpr(Token token) {
  VariableExprAstNode *node =
      ALLOCATE_NODE(VariableExprAstNode, token, EXPR_VARIABLE);
  return node;
}

CallExprAstNode *newCallExpr(Token token) {
  CallExprAstNode *node = ALLOCATE_NODE(CallExprAstNode, token, EXPR_CALL);
  node->params = linkedList_allocate();
  return node;
}

InvocationExprAstNode *newInvocationExpr(Token token) {
  InvocationExprAstNode *node =
      ALLOCATE_NODE(InvocationExprAstNode, token, EXPR_INVOKE);
  node->params = linkedList_allocate();
  return node;
}

ThisExprAstNode *newThisExpr(Token token) {
  ThisExprAstNode *node = ALLOCATE_NODE(ThisExprAstNode, token, EXPR_THIS);
  return node;
}

SuperInvocationExprAstNode *newSuperInvocationExpr(Token method) {
  SuperInvocationExprAstNode *node =
      ALLOCATE_NODE(SuperInvocationExprAstNode, method, EXPR_SUPER_INVOKE);
  node->params = linkedList_allocate();
  return node;
}

SuperExprAstNode *newSuperExpr(Token method) {
  SuperExprAstNode *node = ALLOCATE_NODE(SuperExprAstNode, method, EXPR_SUPER);
  return node;
}

GetPropertyExprAstNode *newGetPropertyExpr(Token token) {
  GetPropertyExprAstNode *node =
      ALLOCATE_NODE(GetPropertyExprAstNode, token, EXPR_GET_PROPERTY);
  return node;
}

AssignStmtAstNode *newAssignStmt(Token token, AstNode *expr) {
  AssignStmtAstNode *node =
      ALLOCATE_NODE(AssignStmtAstNode, token, STMT_ASSIGN);
  node->expr = expr;
  return node;
}

DefineStmtAstNode *newDefineStmt(Token token, bool isConst, AstNode *expr) {
  DefineStmtAstNode *node =
      ALLOCATE_NODE(DefineStmtAstNode, token, STMT_DEFINE);
  node->expr = expr;
  node->isConst = isConst;
  return node;
}

IfStmtAstNode *newIfStmt(Token token, AstNode *condition, AstNode *thenBranch,
                         AstNode *elseBranch) {
  IfStmtAstNode *node = ALLOCATE_NODE(IfStmtAstNode, token, STMT_IF);
  node->condition = condition;
  node->branches.then = thenBranch;
  node->branches.elseB = elseBranch;
  return node;
}

WhileStmtAstNode *newWhileStmt(Token token, AstNode *condition,
                               AstNode *thenBranch) {
  WhileStmtAstNode *node = ALLOCATE_NODE(WhileStmtAstNode, token, STMT_WHILE);
  node->condition = condition;
  node->branches.then = thenBranch;
  return node;
}

ForStmtAstNode *newForStmt(Token token, AstNode *preNode,
                           AstNode *conditionNode, AstNode *postNode,
                           AstNode *bodyNode) {
  ForStmtAstNode *node = ALLOCATE_NODE(ForStmtAstNode, token, STMT_FOR);
  node->branches.pre = preNode;
  node->branches.cond = conditionNode;
  node->branches.post = postNode;
  node->branches.body = bodyNode;
  return node;
}

BlockStmtAstNode *newBlockStmt(Token token) {
  BlockStmtAstNode *node = ALLOCATE_NODE(BlockStmtAstNode, token, STMT_BLOCK);
  node->stmts = linkedList_allocate();
  return node;
}

FunctionExprAstNode *newFunctionExpr(Token token, FunctionType functionType) {
  FunctionExprAstNode *node =
      ALLOCATE_NODE(FunctionExprAstNode, token, EXPR_FUNCTION);
  node->body = NULL;
  node->arity = 0;
  node->functionType = functionType;
  return node;
}

FunctionStmtAstNode *newFunctionStmt(Token token, FunctionExprAstNode *body) {
  FunctionStmtAstNode *node =
      ALLOCATE_NODE(FunctionStmtAstNode, token, STMT_FUNCTION);
  node->expr = body;
  return node;
}

ReturnStmtAstNode *newReturnStmt(Token token, AstNode *expr) {
  ReturnStmtAstNode *node =
      ALLOCATE_NODE(ReturnStmtAstNode, token, STMT_RETURN);
  node->expr = expr;
  return node;
}

ExprStmtAstNode *newExprStmt(Token token, AstNode *expr) {
  ExprStmtAstNode *node = ALLOCATE_NODE(ExprStmtAstNode, token, STMT_EXPR);
  node->expr = expr;
  return node;
}

MethodStmtAstNode *newMethodStmt(Token token, FunctionExprAstNode *expr) {
  MethodStmtAstNode *node =
      ALLOCATE_NODE(MethodStmtAstNode, token, STMT_METHOD);
  node->body = expr;
  return node;
}

ClassStmtAstNode *newClassStmt(Token token) {
  ClassStmtAstNode *node = ALLOCATE_NODE(ClassStmtAstNode, token, STMT_CLASS);
  node->superclass = optionalTokenInit();
  return node;
}

ClassBodyStmtAstNode *newClassBodyStmt(Token token) {
  ClassBodyStmtAstNode *node =
      ALLOCATE_NODE(ClassBodyStmtAstNode, token, STMT_CLASS_BODY);
  node->stmts = linkedList_allocate();
  return node;
}

SetPropertyStmtAstNode *newSetPropertyStmt(Token token, AstNode *expr) {
  SetPropertyStmtAstNode *node =
      ALLOCATE_NODE(SetPropertyStmtAstNode, token, STMT_SET_PROPERTY);
  node->target = NULL;
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

static void printParameterList(const Token params[], int arity,
                               int indentation) {
  for (int i = 0; i < arity; i++) {
    Token name = params[i];
    printf("%.*s", name.length, name.start);
    if (i + 1 != arity) {
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

  switch (AST_NODE_TYPE(node)) {
  case EXPR_AND: {
    AndExprAstNode *expr = AS_AND_EXPR(node);
    printf("%*sExpr And:\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(expr->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(expr->branches.right, indentation + 4);
    break;
  }
  case EXPR_OR: {
    OrExprAstNode *expr = AS_OR_EXPR(node);
    printf("%*sExpr Or:\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(expr->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(expr->branches.right, indentation + 4);
    break;
  }
  case EXPR_BINARY: {
    BinaryExprAstNode *expr = AS_BINARY_EXPR(node);
    printf("%*sExpr Binary:\n", indentation, "");
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(expr->op));
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(expr->branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(expr->branches.right, indentation + 4);
    break;
  }
  case EXPR_CALL: {
    CallExprAstNode *expr = AS_CALL_EXPR(node);
    printf("%*sExpr Call:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(expr->target, indentation + 4);
    printArgumentList(expr->params->head, indentation);
    break;
  }
  case EXPR_FUNCTION: {
    FunctionExprAstNode *expr = AS_FUNCTION_EXPR(node);
    printf("%*sParameters: ", indentation, "");
    printParameterList(expr->params, expr->arity, indentation);
    printf("%*sBody:\n", indentation, "");
    printAST(AS_AST_NODE(expr->body), indentation + 2);
    break;
  }
  case STMT_FUNCTION: {
    FunctionStmtAstNode *stmt = AS_FUNCTION_STMT(node);
    printf("%*sStmt Function:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(AS_AST_NODE(stmt->expr), indentation + 4);
    break;
  }
  case EXPR_GET_PROPERTY: {
    printf("%*sExpr GetProperty:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(AS_GET_PROPERTY_EXPR(node)->target, indentation + 4);
    break;
  }
  case EXPR_LITERAL: {
    LiteralExprAstNode *expr = AS_LITERAL_EXPR(node);
    printf("%*sExpr Literal: ", indentation, "");
    printValue(stdout, expr->literal);
    printf("\n");
    break;
  }
  case EXPR_NIL: {
    printf("%*sExpr: Nil\n", indentation, "");
    break;
  }
  case EXPR_UNARY: {
    UnaryExprAstNode *expr = AS_UNARY_EXPR(node);
    printf("%*sExpr Unary\n", indentation, "");
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(expr->op));
    printf("%*sRight:\n", indentation + 2, "");
    printAST(expr->right, indentation + 4);
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
    printArgumentList(AS_SUPER_INVOCATION_EXPR(node)->params->head,
                      indentation);
    break;
  }
  case EXPR_INVOKE: {
    InvocationExprAstNode *expr = AS_INVOCATION_EXPR(node);
    printf("%*sExpr Invoke:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(expr->target, indentation + 4);
    printf("%*sMethod: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printArgumentList(expr->params->head, indentation);
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
    printAST(AS_ASSIGN_STMT(node)->expr, indentation + 4);
    break;
  }
  case STMT_BLOCK: {
    printf("%*sStmt Block:\n", indentation, "");
    printStatementList(AS_BLOCK_STMT(node)->stmts->head, indentation);
    break;
  }
  case STMT_CLASS: {
    ClassStmtAstNode *stmt = AS_CLASS_STMT(node);
    printf("%*sStmt Class:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    if (OPTIONAL_HAS_VALUE(stmt->superclass)) {
      Token superclassName = OPTIONAL_VALUE(stmt->superclass);
      printf("%*sSuperclass: %.*s\n", indentation + 2, "",
             superclassName.length, superclassName.start);
    }
    printAST(AS_AST_NODE(stmt->body), indentation + 2);
    break;
  }
  case STMT_CLASS_BODY: {
    printf("%*sStmt ClassBody:\n", indentation, "");
    printStatementList(AS_CLASS_BODY_STMT(node)->stmts->head, indentation);
    break;
  }
  case STMT_DEFINE: {
    DefineStmtAstNode *stmt = AS_DEFINE_STMT(node);
    if (stmt->isConst) {
      printf("%*sStmt DefineConst:\n", indentation, "");
    } else {
      printf("%*sStmt Define:\n", indentation, "");
    }
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sValue:\n", indentation + 2, "");
    printAST(AS_DEFINE_STMT(node)->expr, indentation + 4);
    break;
  }
  case STMT_EXPR: {
    ExprStmtAstNode *stmt = AS_EXPR_STMT(node);
    printf("%*sStmt Expr:\n", indentation, "");
    printAST(stmt->expr, indentation + 2);
    break;
  }
  case STMT_FOR: {
    ForStmtAstNode *stmt = AS_FOR_STMT(node);
    printf("%*sStmt For:\n", indentation, "");
    printf("%*sPre:\n", indentation + 2, "");
    printAST(stmt->branches.pre, indentation + 4);
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(stmt->branches.cond, indentation + 4);
    printf("%*sPost:\n", indentation + 2, "");
    printAST(stmt->branches.post, indentation + 4);
    printf("%*sBody:\n", indentation + 2, "");
    printAST(stmt->branches.body, indentation + 4);
    break;
  }
  case STMT_IF: {
    IfStmtAstNode *stmt = AS_IF_STMT(node);
    printf("%*sStmt If:\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(stmt->condition, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(stmt->branches.then, indentation + 4);
    printf("%*sElse:\n", indentation + 2, "");
    printAST(stmt->branches.elseB, indentation + 4);
    break;
  }
  case STMT_METHOD: {
    MethodStmtAstNode *stmt = AS_METHOD_STMT(node);
    printf("%*sStmt Method:\n", indentation, "");
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(AS_AST_NODE(stmt->body), indentation + 4);
    break;
  }
  case STMT_MODULE: {
    ModuleStmtAstNode *stmt = AS_MODULE_STMT(node);
    printf("%*sStmt Module:\n", indentation, "");
    printStatementList(stmt->stmts->head, indentation);
    break;
  }
  case STMT_PRINT: {
    PrintStmtAstNode *stmt = AS_PRINT_STMT(node);
    printf("%*sStmt Print:\n", indentation, "");
    printAST(stmt->expr, indentation + 2);
    break;
  }
  case STMT_RETURN: {
    printf("%*sStmt Return:\n", indentation, "");
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(AS_RETURN_STMT(node)->expr, indentation + 4);
    break;
  }
  case STMT_SET_PROPERTY: {
    SetPropertyStmtAstNode *stmt = AS_SET_PROPERTY_STMT(node);
    printf("%*sStmt SetProperty:\n", indentation, "");
    printf("%*sTarget:\n", indentation + 2, "");
    printAST(stmt->target, indentation + 4);
    printf("%*sName: %.*s\n", indentation + 2, "", node->token.length,
           node->token.start);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(stmt->expr, indentation + 4);
    break;
  }
  case STMT_WHILE: {
    WhileStmtAstNode *stmt = AS_WHILE_STMT(node);
    printf("%*sStmt While:\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(stmt->condition, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(stmt->branches.then, indentation + 4);
    break;
  }
  }
}

OptionalToken optionalTokenInit() {
  return (OptionalToken){.present = false, .value = syntheticToken("")};
}

void optionalTokenSet(OptionalToken *ot, Token value) {
  ot->present = true;
  ot->value = value;
}
