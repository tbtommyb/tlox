#include "ast.h"
#include "compiler.h"
#include "linked_list.h"
#include "memory.h"
#include "scanner.h"
#include "value.h"

// TODO: integrate with garbage collector
static AstNode *allocateAstNode(NodeType type) {
  AstNode *node = (AstNode *)reallocate(NULL, 0, sizeof(AstNode));
  node->type = type;
  node->literal = EMPTY_VAL;
  node->op = TOKEN_UNKNOWN;
  node->expr = NULL;
  node->branches.left = NULL;
  node->branches.right = NULL;
  node->stmts = NULL;
  node->params = NULL;
  node->methods = NULL;
  node->arity = 0;
  node->scope = NULL;
  node->token = (Token){0};
  node->superclass = (Token){0};
  node->functionType = (FunctionType){0};

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

AstNode *newAndExpr() { return allocateAstNode(EXPR_AND); }

AstNode *newOrExpr() { return allocateAstNode(EXPR_OR); }

AstNode *newVariableExpr(Token token) {
  AstNode *node = allocateAstNode(EXPR_VARIABLE);
  node->token = token;
  return node;
}

AstNode *newFunctionExpr(FunctionType functionType) {
  AstNode *node = allocateAstNode(EXPR_FUNCTION);
  node->params = linkedList_allocate();
  node->functionType = functionType;
  return node;
}

AstNode *newCallExpr() {
  AstNode *node = allocateAstNode(EXPR_CALL);
  node->params = linkedList_allocate();
  return node;
}

AstNode *newInvocationExpr(Token name) {
  AstNode *node = allocateAstNode(EXPR_INVOKE);
  node->params = linkedList_allocate();
  node->token = name;
  return node;
}

AstNode *newGetPropertyExpr(Token name) {
  AstNode *node = allocateAstNode(EXPR_GET_PROPERTY);
  node->token = name;
  return node;
}

AstNode *newDefineStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_DEFINE);
  node->token = token;
  node->expr = expr;
  return node;
}

AstNode *newAssignStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_ASSIGN);
  node->token = token;
  node->expr = expr;
  return node;
}

AstNode *newConstDefineStmt(Token token, AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_DEFINE_CONST);
  node->token = token;
  node->expr = expr;
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

AstNode *newWhileStmt(AstNode *condition, AstNode *thenBranch) {
  AstNode *node = allocateAstNode(STMT_WHILE);
  node->expr = condition;
  node->branches.left = thenBranch;
  return node;
}

AstNode *newForStmt(AstNode *initNode, AstNode *conditionNode,
                    AstNode *postNode, AstNode *bodyNode) {

  AstNode *node = allocateAstNode(STMT_FOR);
  node->preExpr = initNode;
  node->condExpr = conditionNode;
  node->postExpr = postNode;
  node->expr = bodyNode;
  return node;
}

AstNode *newModuleStmt() {
  AstNode *node = allocateAstNode(STMT_MODULE);
  node->stmts = linkedList_allocate();
  Token name;
  name.start = "main";
  name.length = 4;
  node->token = name;
  return node;
}

AstNode *newBlockStmt() {
  AstNode *node = allocateAstNode(STMT_BLOCK);
  node->stmts = linkedList_allocate();
  return node;
}

AstNode *newFunctionStmt(Token name, AstNode *body) {
  AstNode *node = allocateAstNode(STMT_FUNCTION);
  node->token = name;
  node->expr = body;
  return node;
}

AstNode *newReturnStmt(AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_RETURN);
  node->expr = expr;
  return node;
}

AstNode *newExprStmt(AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_EXPR);
  node->expr = expr;
  return node;
}

AstNode *newMethodStmt(Token name, AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_METHOD);
  node->token = name;
  node->expr = expr;
  return node;
}

AstNode *newClassStmt(Token name) {
  AstNode *node = allocateAstNode(STMT_CLASS);
  node->token = name;
  return node;
}

AstNode *newClassBodyStmt() {
  AstNode *node = allocateAstNode(STMT_CLASS_BODY);
  node->methods = linkedList_allocate();
  return node;
}

AstNode *newSetPropertyStmt(Token name, AstNode *expr) {
  AstNode *node = allocateAstNode(STMT_SET_PROPERTY);
  node->token = name;
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

// FIXME: change to take pointers
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
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(node.op));
    printf("%*sRight:\n", indentation + 2, "");
    printAST(*node.branches.right, indentation + 4);
    break;
  }
  case EXPR_AND: {
    printf("%*sExpr And\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(*node.branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(*node.branches.right, indentation + 4);
    break;
  }
  case EXPR_OR: {
    printf("%*sExpr Or\n", indentation, "");
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(*node.branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(*node.branches.right, indentation + 4);
    break;
  }
  case EXPR_BINARY: {
    printf("%*sExpr Binary\n", indentation, "");
    printf("%*sOp: %s\n", indentation + 2, "", tokenTypeStr(node.op));
    printf("%*sLeft:\n", indentation + 2, "");
    printAST(*node.branches.left, indentation + 4);
    printf("%*sRight:\n", indentation + 2, "");
    printAST(*node.branches.right, indentation + 4);
    break;
  }
  case EXPR_VARIABLE: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sVariable %s\n", indentation, "", nameString->chars);
    break;
  }
  case EXPR_FUNCTION: {
    Node *param = (Node *)node.params->head;
    printf("%*sParams: ", indentation, "");
    while (param != NULL) {
      Token *name = param->data;
      ObjString *paramName = copyString(name->start, name->length);
      printf("%s", paramName->chars);
      param = param->next;
      if (param != NULL) {
        printf(", ");
      }
    }
    printf("\n");
    printf("%*sBody:\n", indentation, "");
    printAST(*node.expr, indentation + 2);
    break;
  }
  case EXPR_CALL: {
    printf("%*sExpr Call\n", indentation, "");
    if (node.branches.left != NULL) {
      printAST(*(AstNode *)node.branches.left, indentation + 2);
    }
    Node *param = (Node *)node.params->head;
    if (param == NULL) {
      printf("%*sArgs: ()\n", indentation + 2, "");
    } else {
      printf("%*sArgs:\n", indentation + 2, "");
      while (param != NULL) {
        printAST(*(AstNode *)param->data, indentation + 4);
        param = param->next;
      }
    }
    break;
  }
  case EXPR_INVOKE: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sExpr Invoke\n", indentation, "");
    if (node.branches.left != NULL) {
      printAST(*(AstNode *)node.branches.left, indentation + 2);
    }
    printf("%*sMethod: %s\n", indentation + 2, "", nameString->chars);
    Node *param = (Node *)node.params->head;
    if (param == NULL) {
      printf("%*sArgs: ()\n", indentation + 2, "");
    } else {
      printf("%*sArgs:\n", indentation + 2, "");
      while (param != NULL) {
        printAST(*(AstNode *)param->data, indentation + 4);
        param = param->next;
      }
    }
    break;
  }
  case STMT_DEFINE: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sStmt Define\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    if (node.expr != NULL) {
      printf("%*sValue:\n", indentation + 2, "");
      printAST(*node.expr, indentation + 4);
    }
    break;
  }
  case STMT_DEFINE_CONST: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sStmt Define const\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    if (node.expr != NULL) {
      printf("%*sValue:\n", indentation + 2, "");
      printAST(*node.expr, indentation + 4);
    }
    break;
  }
  case STMT_ASSIGN: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sStmt Assign\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    printf("%*sValue:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case STMT_IF: {
    printf("%*sStmt If\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(*node.branches.left, indentation + 4);
    if (node.branches.right) {
      printf("%*sElse:\n", indentation + 2, "");
      printAST(*node.branches.right, indentation + 4);
    }
    break;
  }
  case STMT_WHILE: {
    printf("%*sStmt While\n", indentation, "");
    printf("%*sCondition:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    printf("%*sThen:\n", indentation + 2, "");
    printAST(*node.branches.left, indentation + 4);
    break;
  }
  case STMT_FOR: {
    printf("%*sStmt For\n", indentation, "");
    if (node.preExpr) {
      printf("%*sInit:\n", indentation + 2, "");
      printAST(*node.preExpr, indentation + 4);
    }
    if (node.condExpr) {
      printf("%*sCondition:\n", indentation + 2, "");
      printAST(*node.condExpr, indentation + 4);
    }
    if (node.postExpr) {
      printf("%*sPost:\n", indentation + 2, "");
      printAST(*node.postExpr, indentation + 4);
    }
    printf("%*sBody:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case STMT_PRINT: {
    printf("%*sStmt Print\n", indentation, "");
    printAST(*node.expr, indentation + 2);
    break;
  }
  case STMT_RETURN: {
    printf("%*sStmt Return\n", indentation, "");
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case STMT_EXPR: {
    printf("%*sStmt Expr\n", indentation, "");
    if (node.expr != NULL) {
      printAST(*node.expr, indentation + 2);
    }
    break;
  }
  case STMT_MODULE: {
    printf("%*sStmt Module\n", indentation, "");
    Node *stmtNode = (Node *)node.stmts->head;
    while (stmtNode != NULL) {
      printAST(*(AstNode *)stmtNode->data, indentation + 2);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    printf("%*sStmt Block\n", indentation, "");
    Node *stmtNode = (Node *)node.stmts->head;
    while (stmtNode != NULL) {
      printAST(*(AstNode *)stmtNode->data, indentation + 2);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_FUNCTION: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sStmt Function\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case STMT_CLASS: {
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sStmt Class\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    if (node.superclass.length > 0) {
      ObjString *superclassNameString =
          copyString(node.superclass.start, node.superclass.length);
      printf("%*sSuperclass: %s\n", indentation + 2, "",
             superclassNameString->chars);
    }
    printAST(*node.expr, indentation + 2);
    break;
  }
  case STMT_CLASS_BODY: {
    Node *method = (Node *)node.methods->head;
    while (method != NULL) {
      printAST(*(AstNode *)method->data, indentation + 2);
      method = method->next;
    }
    break;
  }
  case STMT_METHOD: {
    printf("%*sStmt Method\n", indentation, "");
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case STMT_SET_PROPERTY: {
    printf("%*sStmt set property\n", indentation, "");
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  case EXPR_GET_PROPERTY: {
    printf("%*sExpr get property\n", indentation, "");
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
    break;
  }
  }
}
