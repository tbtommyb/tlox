#include "parser.h"
#include "ast.h"
#include "memory.h"
#include "scanner.h"
#include <stdlib.h>

static void advance(Parser *parser) {
  parser->previousPrevious = parser->previous;
  parser->previous = parser->current;

  for (;;) {
    parser->current = scanToken();
    if (parser->current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser->compiler, parser->current.start);
  }
}

static void consume(Parser *parser, TokenType type, const char *message) {
  if (parser->current.type == type) {
    advance(parser);
    return;
  }

  errorAtCurrent(parser->compiler, message);
}

static bool check(Parser *parser, TokenType type) {
  return parser->current.type == type;
}

static bool match(Parser *parser, TokenType type) {
  if (!check(parser, type)) {
    return false;
  }
  advance(parser);
  return true;
}

static AstNode *expression(Parser *parser);
static AstNode *statement(Parser *parser);
static AstNode *declaration(Parser *parser);

static PrintStmtAstNode *printStatement(Parser *parser) {
  AstNode *expr = expression(parser);
  Token token = parser->previous;
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after value.");
  return newPrintStmt(token, expr);
}

static ReturnStmtAstNode *returnStatement(Parser *parser) {
  AstNode *returnExpr = NULL;
  Token returnToken = parser->previous;
  if (!check(parser, TOKEN_SEMICOLON)) {
    returnExpr = expression(parser);
  }
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after return value.");
  ReturnStmtAstNode *node = newReturnStmt(returnToken, returnExpr);
  return node;
}

static void synchronize(Parser *parser) {
  parser->compiler->panicMode = false;

  while (parser->current.type != TOKEN_EOF) {
    if (parser->previous.type == TOKEN_SEMICOLON) {
      return;
    }
    switch (parser->current.type) {
    case TOKEN_CLASS:
    case TOKEN_CONST:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;

    default:; // Do nothing.
    }

    advance(parser);
  }
}

static ParseRule *getRule(TokenType type);

static AstNode *parsePrecedence(Parser *parser, Precedence precedence);

static Token parseVariable(Parser *parser, const char *errorMessage) {
  consume(parser, TOKEN_IDENTIFIER, errorMessage);

  Token prev = parser->previous;

  return prev;
}

static AstNode *binary(Parser *parser, bool canAssign) {
  // Remember the operator.
  Token token = parser->previous;
  TokenType operatorType = token.type;

  // Compile the right operand.
  ParseRule *rule = getRule(operatorType);
  AstNode *right = parsePrecedence(parser, (Precedence)(rule->precedence + 1));

  // Emit the operator instruction.
  if (operatorType == TOKEN_PLUS || operatorType == TOKEN_MINUS ||
      operatorType == TOKEN_STAR || operatorType == TOKEN_SLASH ||
      operatorType == TOKEN_BANG_EQUAL || operatorType == TOKEN_EQUAL_EQUAL ||
      operatorType == TOKEN_GREATER || operatorType == TOKEN_GREATER_EQUAL ||
      operatorType == TOKEN_LESS || operatorType == TOKEN_LESS_EQUAL ||
      operatorType == TOKEN_PERCENT) {
    return AS_AST_NODE(newBinaryExpr(token, NULL, right, operatorType));
  }
  return NULL;
}

static AstNode *literal(Parser *parser, bool canAssign) {
  switch (parser->previous.type) {
  case TOKEN_FALSE:
    return AS_AST_NODE(newLiteralExpr(parser->previous, FALSE_VAL));
  case TOKEN_NIL:
    return AS_AST_NODE(newNilExpr(parser->previous));
  case TOKEN_TRUE:
    return AS_AST_NODE(newLiteralExpr(parser->previous, TRUE_VAL));
  default:
    return NULL; // Unreachable.
  }
}

static AstNode *expression(Parser *parser) {
  return parsePrecedence(parser, PREC_ASSIGNMENT);
}

static BlockStmtAstNode *block(Parser *parser) {
  BlockStmtAstNode *node = newBlockStmt(parser->current);
  while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
    linkedList_append(node->stmts, declaration(parser));
  }

  consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
  return node;
}

static DefineStmtAstNode *varDeclaration(Parser *parser, bool isConst) {
  Token name = parseVariable(parser, "Expect variable name.");

  DefineStmtAstNode *node = newDefineStmt(name, isConst, NULL);
  if (match(parser, TOKEN_EQUAL)) {
    node->expr = expression(parser);
  }
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  return node;
}

static ExprStmtAstNode *expressionStatement(Parser *parser) {
  ExprStmtAstNode *node = newExprStmt(parser->current, expression(parser));
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after expression.");
  return node;
}

static ForStmtAstNode *forStatement(Parser *parser) {
  Token token = parser->previous;

  consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

  AstNode *preNode = NULL;
  if (match(parser, TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(parser, TOKEN_VAR)) {
    preNode = AS_AST_NODE(varDeclaration(parser, false));
  } else {
    preNode = AS_AST_NODE(expressionStatement(parser));
  }

  AstNode *conditionNode = NULL;
  if (!match(parser, TOKEN_SEMICOLON)) {
    conditionNode = expression(parser);
    consume(parser, TOKEN_SEMICOLON, "Expect ';' after loop condition.");
  }

  AstNode *postNode = NULL;
  if (!match(parser, TOKEN_RIGHT_PAREN)) {
    postNode = expression(parser);
    consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
  }

  AstNode *bodyNode = statement(parser);

  return newForStmt(token, preNode, conditionNode, postNode, bodyNode);
}

static IfStmtAstNode *ifStatement(Parser *parser) {
  Token token = parser->previous;

  consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  AstNode *condition = expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  AstNode *thenBranch = statement(parser);
  AstNode *elseBranch = NULL;
  if (match(parser, TOKEN_ELSE)) {
    elseBranch = statement(parser);
  }

  return newIfStmt(token, condition, thenBranch, elseBranch);
}

static WhileStmtAstNode *whileStatement(Parser *parser) {
  Token token = parser->previous;

  consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  AstNode *condition = expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  AstNode *thenBranch = statement(parser);

  return newWhileStmt(token, condition, thenBranch);
}

static AstNode *variable(Parser *parser, bool canAssign) {
  Token token = parser->previous;

  if (canAssign && match(parser, TOKEN_EQUAL)) {
    return AS_AST_NODE(newAssignStmt(token, expression(parser)));
  }
  return AS_AST_NODE(newVariableExpr(token));
}

static FunctionExprAstNode *function(Parser *parser, FunctionType type) {
  FunctionExprAstNode *node = newFunctionExpr(parser->current, type);

  int arity = 0;
  consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(parser, TOKEN_RIGHT_PAREN)) {
    do {
      arity++;
      if (arity > 255) {
        errorAtCurrent(parser->compiler,
                       "Can't have more than 255 parameters.");
      }
      Token paramName = parseVariable(parser, "Expect parameter name.");
      node->params[arity - 1] = paramName;
    } while (match(parser, TOKEN_COMMA));
  }
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(parser, TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  node->body = block(parser);
  node->arity = arity;

  return node;
}

static FunctionStmtAstNode *funDeclaration(Parser *parser) {
  Token name = parseVariable(parser, "Expect function name.");
  FunctionExprAstNode *body = function(parser, TYPE_FUNCTION);
  return newFunctionStmt(name, body);
}

static MethodStmtAstNode *method(Parser *parser) {
  Token name = parseVariable(parser, "Expect method name.");

  FunctionType type = TYPE_METHOD;
  if (parser->previous.length == 4 &&
      memcmp(parser->previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }

  FunctionExprAstNode *body = function(parser, type);
  return newMethodStmt(name, body);
}

static ClassStmtAstNode *classDeclaration(Parser *parser) {
  Token name = parseVariable(parser, "Expect class name.");
  ClassStmtAstNode *node = newClassStmt(name);

  if (match(parser, TOKEN_LESS)) {
    Token superClassName = parseVariable(parser, "Expect superclass name.");
    optionalTokenSet(&node->superclass, superClassName);
  }

  consume(parser, TOKEN_LEFT_BRACE, "Expect '{' before class body.");

  ClassBodyStmtAstNode *classBodyNode = newClassBodyStmt(name);
  node->body = classBodyNode;
  while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
    linkedList_append(classBodyNode->stmts, method(parser));
  }

  consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' before class body.");

  return node;
}

static AstNode *declaration(Parser *parser) {
  if (match(parser, TOKEN_CONST)) {
    return AS_AST_NODE(varDeclaration(parser, true));
  } else if (match(parser, TOKEN_VAR)) {
    return AS_AST_NODE(varDeclaration(parser, false));
  } else if (match(parser, TOKEN_FUN)) {
    return AS_AST_NODE(funDeclaration(parser));
  } else if (match(parser, TOKEN_CLASS)) {
    return AS_AST_NODE(classDeclaration(parser));
  } else {
    return statement(parser);
  }
  if (parser->compiler->panicMode) {
    synchronize(parser);
  }
}

static AstNode *statement(Parser *parser) {
  if (match(parser, TOKEN_PRINT)) {
    return AS_AST_NODE(printStatement(parser));
  } else if (match(parser, TOKEN_IF)) {
    return AS_AST_NODE(ifStatement(parser));
  } else if (match(parser, TOKEN_FOR)) {
    return AS_AST_NODE(forStatement(parser));
  } else if (match(parser, TOKEN_LEFT_BRACE)) {
    return AS_AST_NODE(block(parser));
  } else if (match(parser, TOKEN_RETURN)) {
    return AS_AST_NODE(returnStatement(parser));
  } else if (match(parser, TOKEN_WHILE)) {
    return AS_AST_NODE(whileStatement(parser));
  } else {
    return AS_AST_NODE(expressionStatement(parser));
  }
  /* } else if (match(TOKEN_SWITCH)) { */
  /*   switchStatement(); */
  /* } else if (match(TOKEN_CONTINUE)) { */
  /*   continueStatement(); */
}

static AstNode *unary(Parser *parser, bool canAssign) {
  Token token = parser->previous;
  TokenType operatorType = token.type;

  // Compile the operand.
  AstNode *operand = parsePrecedence(parser, PREC_UNARY);

  if (operatorType == TOKEN_BANG || operatorType == TOKEN_MINUS) {
    return AS_AST_NODE(newUnaryExpr(token, operand, operatorType));
  }
  return NULL;
}

static AstNode *number(Parser *parser, bool canAssign) {
  double value = strtod(parser->previous.start, NULL);
  return AS_AST_NODE(newLiteralExpr(parser->previous, NUMBER_VAL(value)));
}

static AstNode *string(Parser *parser, bool canAssign) {
  Token token = parser->previous;
  return AS_AST_NODE(newLiteralExpr(
      token, OBJ_VAL(copyString(token.start + 1, token.length - 2))));
}

static AstNode *grouping(Parser *parser, bool canAssign) {
  AstNode *node = expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
  return node;
}

static void argumentList(Parser *parser, AstNode *parent) {
  uint8_t argCount = 0;
  if (!check(parser, TOKEN_RIGHT_PAREN)) {
    do {
      // FIXME: code smell
      switch (AST_NODE_TYPE(parent)) {
      case EXPR_CALL: {
        linkedList_append(((CallExprAstNode *)parent)->params,
                          expression(parser));
        break;
      }
      case EXPR_SUPER_INVOKE: {
        linkedList_append(((SuperInvocationExprAstNode *)parent)->params,
                          expression(parser));
        break;
      }
      case EXPR_INVOKE: {
        linkedList_append(((InvocationExprAstNode *)parent)->params,
                          expression(parser));
        break;
      }
      default: {
        error(parser->compiler,
              "Received unexpected AstNode type in argumentList");
        return;
      }
      }
      if (argCount == 255) {
        errorAt(parser->compiler, &parser->previous,
                "Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(parser, TOKEN_COMMA));
  }
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
}

static AstNode *call(Parser *parser, bool canAssign) {
  CallExprAstNode *expr = newCallExpr(parser->current);
  argumentList(parser, AS_AST_NODE(expr));

  return AS_AST_NODE(expr);
}

static AstNode *and_(Parser *parser, bool canAssign) {
  AndExprAstNode *expr = newAndExpr(parser->current);
  expr->branches.right = parsePrecedence(parser, PREC_AND);

  return AS_AST_NODE(expr);
}

static AstNode *or_(Parser *parser, bool canAssign) {
  OrExprAstNode *expr = newOrExpr(parser->current);
  expr->branches.right = parsePrecedence(parser, PREC_OR);

  return AS_AST_NODE(expr);
}

static AstNode *dot(Parser *parser, bool canAssign) {
  Token name = parseVariable(parser, "Expect property name after '.'.");

  if (canAssign && match(parser, TOKEN_EQUAL)) {
    return AS_AST_NODE(newSetPropertyStmt(name, expression(parser)));
  } else if (match(parser, TOKEN_LEFT_PAREN)) {
    InvocationExprAstNode *node = newInvocationExpr(name);
    argumentList(parser, AS_AST_NODE(node));
    return AS_AST_NODE(node);
  } else {
    return AS_AST_NODE(newGetPropertyExpr(name));
  }
}

static AstNode *this_(Parser *parser, bool canAssign) {
  return AS_AST_NODE(newThisExpr(parser->previous));
}

static AstNode *super_(Parser *parser, bool canAssign) {
  consume(parser, TOKEN_DOT, "Expect '.' after 'super'.");
  Token method = parseVariable(parser, "Expect superclass method name.");

  if (match(parser, TOKEN_LEFT_PAREN)) {
    SuperInvocationExprAstNode *node = newSuperInvocationExpr(method);
    argumentList(parser, AS_AST_NODE(node));
    return AS_AST_NODE(node);
  } else {
    return AS_AST_NODE(newSuperExpr(method));
  }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_LEFT_BRACKET] = {arrayLiteral, leftBracket, PREC_CALL}, */
    [TOKEN_RIGHT_BRACKET] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, dot, PREC_CALL},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_PERCENT] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, and_, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, or_, PREC_OR},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {super_, NULL, PREC_NONE},
    [TOKEN_THIS] = {this_, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_QUESTION] = {NULL, ternary, PREC_OR}, */
    /* [TOKEN_PLUS_PLUS] = {NULL, increment, PREC_CALL}, */
    /* [TOKEN_MINUS_MINUS] = {NULL, decrement, PREC_CALL}, */
};

static AstNode *parsePrecedence(Parser *parser, Precedence precedence) {
  advance(parser);
  ParseFn prefixRule = getRule(parser->previous.type)->prefix;
  if (prefixRule == NULL) {
    error(parser->compiler, "Expect expression.");
    return NULL;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  AstNode *left = prefixRule(parser, canAssign);

  while (precedence <= getRule(parser->current.type)->precedence) {
    advance(parser);
    ParseFn infixRule = getRule(parser->previous.type)->infix;
    if (infixRule == NULL) {
      return NULL;
    }
    AstNode *newNode = infixRule(parser, canAssign);
    // FIXME: code smell
    switch (AST_NODE_TYPE(newNode)) {
    case EXPR_AND: {
      ((AndExprAstNode *)newNode)->branches.left = left;
      break;
    }
    case EXPR_CALL: {
      ((CallExprAstNode *)newNode)->target = left;
      break;
    }
    case EXPR_SUPER_INVOKE: {
      ((SuperInvocationExprAstNode *)newNode)->target = left;
      break;
    }
    case EXPR_INVOKE: {
      ((InvocationExprAstNode *)newNode)->target = left;
      break;
    }
    case EXPR_GET_PROPERTY: {
      ((GetPropertyExprAstNode *)newNode)->target = left;
      break;
    }
    case STMT_SET_PROPERTY: {
      ((SetPropertyStmtAstNode *)newNode)->target = left;
      break;
    }
    case EXPR_OR: {
      ((OrExprAstNode *)newNode)->branches.left = left;
      break;
    }
    case EXPR_BINARY: {
      ((BinaryExprAstNode *)newNode)->branches.left = left;
      break;
    }
    default: {
      error(parser->compiler, "Received unexpected AstNode type");
    }
    }
    left = newNode;
  }

  if (left != NULL) {
    return left;
  }

  if (canAssign && match(parser, TOKEN_EQUAL)) {
    error(parser->compiler, "Invalid assignment target.");
  }

  return NULL;
}

static ParseRule *getRule(TokenType type) { return &rules[type]; }

AstNode *parse(Parser *parser) {
  advance(parser);

  ModuleStmtAstNode *ast = newModuleStmt(parser->previous);
  while (!match(parser, TOKEN_EOF)) {
    linkedList_append(ast->stmts, declaration(parser));
  }

  return AS_AST_NODE(ast);
}
