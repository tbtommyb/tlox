#include "parser.h"
#include "ast.h"
#include "memory.h"
#include "scanner.h"
#include <stdlib.h>

// just use syntheticToken?
static Token *allocateToken(Token token) {
  Token *heapToken = (Token *)reallocate(NULL, 0, sizeof(Token));
  heapToken->length = token.length;
  heapToken->line = token.line;
  heapToken->start = token.start;
  heapToken->type = token.type;
  return heapToken;
}
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

static AstNode *printStatement(Parser *parser) {
  AstNode *expr = expression(parser);
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after value.");
  return newPrintStmt(expr);
}

static AstNode *returnStatement(Parser *parser) {
  AstNode *returnExpr = NULL;
  Token returnToken = parser->previous;
  if (!match(parser, TOKEN_SEMICOLON)) {
    returnExpr = expression(parser);
  }
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after return value.");
  AstNode *node = newReturnStmt(returnExpr);
  node->token = returnToken;
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
  TokenType operatorType = parser->previous.type;

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
    return newBinaryExpr(NULL, right, operatorType);
  }
  return NULL;
}

static AstNode *literal(Parser *parser, bool canAssign) {
  switch (parser->previous.type) {
  case TOKEN_FALSE:
    return newLiteralExpr(FALSE_VAL);
  case TOKEN_NIL:
    return newLiteralExpr(NIL_VAL);
  case TOKEN_TRUE:
    return newLiteralExpr(TRUE_VAL);
  default:
    return NULL; // Unreachable.
  }
}

static AstNode *expression(Parser *parser) {
  return parsePrecedence(parser, PREC_ASSIGNMENT);
}

static AstNode *block(Parser *parser) {
  AstNode *node = newBlockStmt();
  while (!check(parser, TOKEN_RIGHT_BRACE) && !check(parser, TOKEN_EOF)) {
    linkedList_append(node->stmts, declaration(parser));
  }

  consume(parser, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
  return node;
}

static AstNode *varDeclaration(Parser *parser, bool isConst) {
  Token name = parseVariable(parser, "Expect variable name.");

  AstNode *node = NULL;
  if (isConst) {
    // TODO maybe combine into one when creating subtypes for AST nodes
    node = newConstDefineStmt(name, NULL);
  } else {
    node = newDefineStmt(name, NULL);
  }
  if (match(parser, TOKEN_EQUAL)) {
    node->expr = expression(parser);
  }
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  return node;
}

static AstNode *expressionStatement(Parser *parser) {
  AstNode *node = newExprStmt(expression(parser));
  consume(parser, TOKEN_SEMICOLON, "Expect ';' after expression.");
  return node;
}

static AstNode *ifStatement(Parser *parser) {
  consume(parser, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  AstNode *condition = expression(parser);
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  AstNode *thenBranch = statement(parser);
  AstNode *elseBranch = NULL;
  if (match(parser, TOKEN_ELSE)) {
    elseBranch = statement(parser);
  }

  return newIfStmt(condition, thenBranch, elseBranch);
}

static AstNode *variable(Parser *parser, bool canAssign) {
  Token token = parser->previous;
  if (canAssign && match(parser, TOKEN_EQUAL)) {
    return newAssignStmt(token, expression(parser));
  }
  return newVariableExpr(token);
}

static AstNode *function(Parser *parser, FunctionType type) {
  AstNode *node = newFunctionExpr();

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
      linkedList_append(node->params, allocateToken(paramName));
    } while (match(parser, TOKEN_COMMA));
  }
  consume(parser, TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(parser, TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  node->expr = block(parser);
  node->arity = arity;

  return node;
}

static AstNode *funDeclaration(Parser *parser) {
  Token name = parseVariable(parser, "Expect function name.");
  AstNode *body = function(parser, TYPE_FUNCTION);
  return newFunctionStmt(name, body);
}

static AstNode *declaration(Parser *parser) {
  if (match(parser, TOKEN_CONST)) {
    return varDeclaration(parser, true);
  } else if (match(parser, TOKEN_VAR)) {
    return varDeclaration(parser, false);
  } else if (match(parser, TOKEN_FUN)) {
    return funDeclaration(parser);
  } else {
    return statement(parser);
  }
  /* if (match(TOKEN_CLASS)) {
  /*   classDeclaration(); */
  /* } */
  if (parser->compiler->panicMode) {
    synchronize(parser);
  }
}

static AstNode *statement(Parser *parser) {
  if (match(parser, TOKEN_PRINT)) {
    return printStatement(parser);
  } else if (match(parser, TOKEN_IF)) {
    return ifStatement(parser);
  } else if (match(parser, TOKEN_LEFT_BRACE)) {
    AstNode *node = block(parser);
    return node;
  } else if (match(parser, TOKEN_RETURN)) {
    return returnStatement(parser);
  } else {
    return expressionStatement(parser);
  }
  /* } else if (match(TOKEN_FOR)) { */
  /*   forStatement(); */
  /* } else if (match(TOKEN_WHILE)) { */
  /*   whileStatement(); */
  /* } else if (match(TOKEN_SWITCH)) { */
  /*   switchStatement(); */
  /* } else if (match(TOKEN_CONTINUE)) { */
  /*   continueStatement(); */
}

static AstNode *unary(Parser *parser, bool canAssign) {
  TokenType operatorType = parser->previous.type;

  // Compile the operand.
  AstNode *operand = parsePrecedence(parser, PREC_UNARY);

  if (operatorType == TOKEN_BANG || operatorType == TOKEN_MINUS) {
    return newUnaryExpr(operand, operatorType);
  }
  return NULL;
}

static AstNode *number(Parser *parser, bool canAssign) {
  double value = strtod(parser->previous.start, NULL);
  return newLiteralExpr(NUMBER_VAL(value));
}

static AstNode *string(Parser *parser, bool canAssign) {
  return newLiteralExpr(OBJ_VAL(
      copyString(parser->previous.start + 1, parser->previous.length - 2)));
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
      linkedList_append(parent->params, expression(parser));
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
  AstNode *expr = newCallExpr(parser->previousPrevious);
  argumentList(parser, expr);

  return expr;
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_LEFT_BRACKET] = {arrayLiteral, leftBracket, PREC_CALL}, */
    /* [TOKEN_RIGHT_BRACKET] = {NULL, NULL, PREC_NONE}, */
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_DOT] = {NULL, dot, PREC_CALL}, */
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_PERCENT] = {NULL, binary, PREC_FACTOR},
    /* [TOKEN_BANG] = {unary, NULL, PREC_NONE}, */
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY}, */
    /* [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY}, */
    /* [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON}, */
    [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    /* [TOKEN_AND] = {NULL, and_, PREC_AND}, */
    /* [TOKEN_CLASS] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_ELSE] = {NULL, NULL, PREC_NONE}, */
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    /* [TOKEN_FOR] = {NULL, NULL, PREC_NONE}, */
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    /* [TOKEN_OR] = {NULL, or_, PREC_OR}, */
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_SUPER] = {super_, NULL, PREC_NONE}, */
    /* [TOKEN_THIS] = {this_, NULL, PREC_NONE}, */
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_WHILE] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_ERROR] = {NULL, NULL, PREC_NONE}, */
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
    newNode->branches.left = left;
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

  AstNode *ast = newModuleStmt();
  while (!match(parser, TOKEN_EOF)) {
    linkedList_append(ast->stmts, declaration(parser));
  }

  return ast;
}
