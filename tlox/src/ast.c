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
  node->arity = 0;
  node->scope = NULL;

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

AstNode *newVariableExpr(Token token) {
  AstNode *node = allocateAstNode(EXPR_VARIABLE);
  node->token = token;
  return node;
}

AstNode *newFunctionExpr() {
  AstNode *node = allocateAstNode(EXPR_FUNCTION);
  node->params = linkedList_allocate();
  return node;
}

AstNode *newCallExpr(Token name) {
  AstNode *node = allocateAstNode(EXPR_CALL);
  node->token = name;
  node->params = linkedList_allocate();
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

char *tokenTypeStr(TokenType op) {
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

// change to take pointer?
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
    ObjString *nameString = copyString(node.token.start, node.token.length);
    printf("%*sExpr Call\n", indentation, "");
    printf("%*sName: %s\n", indentation + 2, "", nameString->chars);
    Node *param = (Node *)node.params->head;
    if (param == NULL) {
      break;
    }
    printf("%*sArgs:\n", indentation + 2, "");
    while (param != NULL) {
      printAST(*(AstNode *)param->data, indentation + 4);
      param = param->next;
    }
    printf("\n");
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
  case STMT_PRINT: {
    printf("%*sStmt Print\n", indentation, "");
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
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
    printf("%*sExpr:\n", indentation + 2, "");
    printAST(*node.expr, indentation + 4);
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
  }
}
