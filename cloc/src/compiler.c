#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cfg.h"
#include "chunk.h"
#include "codegen.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"
#include "table.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  Token previousPrevious;
  bool hadError;
  bool panicMode;
  FILE *ostream;
  FILE *errstream;
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_TERNARY,    // ?:
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . () []
  PREC_PRIMARY
} Precedence;

typedef AstNode *(*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isConst;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_SCRIPT,
  TYPE_METHOD,
  TYPE_INITIALIZER,
} FunctionType;

typedef struct Compiler {
  struct Compiler *enclosing;
  ObjFunction *function;
  FunctionType type;

  Upvalue upvalues[UINT8_COUNT];
  Local locals[UINT8_COUNT];
  int localCount;
  int scopeDepth;

  int loopOffset;
  int currentStackDepth;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler *enclosing;
  bool hasSuperclass;
} ClassCompiler;

Parser parser;
Compiler *current = NULL;
ClassCompiler *currentClass = NULL;
Table stringConstants;
Table globalConsts;

static Chunk *currentChunk() { return &current->function->chunk; }

static void errorAt(Token *token, const char *message) {
  if (parser.panicMode)
    return;
  parser.panicMode = true;
  fprintf(parser.errstream, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(parser.errstream, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(parser.errstream, " at '%.*s'", token->length, token->start);
  }

  fprintf(parser.errstream, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char *message) { errorAt(&parser.previous, message); }

static void errorAtCurrent(const char *message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previousPrevious = parser.previous;
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR)
      break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char *message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
  if (!check(type)) {
    return false;
  }
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX)
    error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}

static void emitReturn() {
  if (current->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;

  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  compiler->loopOffset = -1;
  current = compiler;

  if (type != TYPE_SCRIPT) {
    current->function->name =
        copyString(parser.previous.start, parser.previous.length);
  }

  Local *local = &current->locals[current->localCount++];
  local->depth = 0;
  local->isCaptured = false;

  if (type != TYPE_FUNCTION) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}

static ObjFunction *endCompiler() {
  emitReturn();

  ObjFunction *function = current->function;
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
                                         ? function->name->chars
                                         : "<script>");
  }
#endif

  current = current->enclosing;
  return function;
}

static void beginScope() { current->scopeDepth++; }
static void endScope() {
  current->scopeDepth--;
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth > current->scopeDepth) {
    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }
    current->localCount--;
  }
}

static AstNode *expression();
static AstNode *statement();
static AstNode *declaration();

static AstNode *printStatement() {
  AstNode *expr = expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  return newPrintStmt(expr);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }
  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
    }
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) {
      return;
    }
    switch (parser.current.type) {
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

    advance();
  }
}

static ParseRule *getRule(TokenType type);

static AstNode *parsePrecedence(Precedence precedence);

static int searchConstantsFor(Value value) {
  ValueArray constants = currentChunk()->constants;
  for (int i = 0; i < constants.count; i++) {
    Value constant = constants.values[i];
    if (valuesEqual(constant, value)) {
      return i;
    }
  }
  return -1;
}

static uint8_t identifierConstant(Token *name) {
  Value value = OBJ_VAL(copyString(name->start, name->length));

  int constantIndex = searchConstantsFor(value);
  if (constantIndex != -1) {
    return constantIndex;
  }
  return makeConstant(value);
}

static bool identifiersEqual(Token *a, Token *b) {
  if (a->length != b->length) {
    return false;
  }
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name, Local **foundLocal) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local *local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      *foundLocal = local;
      return i;
    }
  }

  return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;

  for (int i = 0; i < upvalueCount; i++) {
    Upvalue *upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler *compiler, Token *name, Local **localVar) {
  if (compiler->enclosing == NULL) {
    return -1;
  }

  int local = resolveLocal(compiler->enclosing, name, localVar);
  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(compiler->enclosing, name, localVar);
  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }

  return -1;
}

static void addLocal(Token name, bool isConst) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local *local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1;
  local->isCaptured = false;
  local->isConst = isConst;
}

static void declareVariable(bool isConst) {
  Token *name = &parser.previous;

  if (current->scopeDepth == 0) {
    return;
  }

  for (int i = current->localCount - 1; i >= 0; i--) {
    Local *local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  addLocal(*name, isConst);
}

static uint8_t parseVariable(bool isConst, const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  Token token = parser.previous;
  if (tableFindString(&globalConsts, token.start, token.length)) {
    error("Cannot redeclare a const variable");
    return 0;
  }

  declareVariable(isConst);

  if (current->scopeDepth > 0) {
    return 0;
  }
  // we are in global scope. Check for a global variable with the same name
  if (searchConstantsFor(OBJ_VAL(copyString(token.start, token.length))) !=
      -1) {
    error("Already a variable with this name in this scope");
    return 0;
  }

  if (isConst) {
    ObjString *varName = makeString(token.start, token.length);
    tableSet(&globalConsts, OBJ_VAL(varName), TRUE_VAL);
  }

  return identifierConstant(&token);
}

static void markInitialized() {
  if (current->scopeDepth == 0) {
    return;
  }
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);

  emitByte(OP_POP);
  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

static AstNode *binary(bool canAssign) {
  // Remember the operator.
  TokenType operatorType = parser.previous.type;

  // Compile the right operand.
  ParseRule *rule = getRule(operatorType);
  AstNode *right = parsePrecedence((Precedence)(rule->precedence + 1));

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

static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void leftBracket(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_BRACKET, "Expect ']' after expression.");

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitByte(OP_SET_COMPUTED_PROPERTY);
  } else {
    emitByte(OP_GET_COMPUTED_PROPERTY);
  }
}

static void literal(bool canAssign) {
  switch (parser.previous.type) {
  case TOKEN_FALSE:
    emitByte(OP_FALSE);
    break;
  case TOKEN_NIL:
    emitByte(OP_NIL);
    break;
  case TOKEN_TRUE:
    emitByte(OP_TRUE);
    break;
  default:
    return; // Unreachable.
  }
}

static AstNode *expression() { return parsePrecedence(PREC_ASSIGNMENT); }

static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = parseVariable(false, "Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  ObjFunction *function = endCompiler();
  if (function->upvalueCount > 0 ||
      (type == TYPE_INITIALIZER || type == TYPE_METHOD)) {
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
    for (int i = 0; i < function->upvalueCount; i++) {
      emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
      emitByte(compiler.upvalues[i].index);
    }
  } else {
    emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous);

  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }
  function(type);
  emitBytes(OP_METHOD, constant);
}

static void funDeclaration() {
  uint8_t global = parseVariable(true, "Expect function name.");
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void varDeclaration(bool isConst) {
  uint8_t global = parseVariable(isConst, "Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  defineVariable(global);
}

static AstNode *expressionStatement() {
  AstNode *node = expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  /* emitByte(OP_POP); */
  return node;
}

static void whileStatement() {
  int oldLoopOffset = current->loopOffset;
  int oldStackDepth = current->currentStackDepth;
  current->loopOffset = currentChunk()->count;
  current->currentStackDepth = current->scopeDepth;

  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(current->loopOffset);

  patchJump(exitJump);
  emitByte(OP_POP);
  current->loopOffset = oldLoopOffset;
  current->currentStackDepth = oldStackDepth;
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

  int oldLoopOffset = current->loopOffset;
  int oldStackDepth = current->currentStackDepth;

  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration(false);
  } else {
    expressionStatement();
  }

  current->loopOffset = currentChunk()->count;
  current->currentStackDepth = current->scopeDepth;

  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Condition.
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;

    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(current->loopOffset);
    current->loopOffset = incrementStart;
    patchJump(bodyJump);
  }

  statement();

  emitLoop(current->loopOffset);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Condition.
  }

  current->loopOffset = oldLoopOffset;
  current->currentStackDepth = oldStackDepth;

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  emitByte(OP_POP);

  if (match(TOKEN_ELSE)) {
    statement();
  }
  patchJump(elseJump);
}

static void switchStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' after parenthesis.");

  beginScope();

  int endJumps[UINT8_COUNT];
  int caseCount = 0;

  while (match(TOKEN_CASE) && !check(TOKEN_EOF)) {
    if (caseCount == UINT8_COUNT) {
      error("Too many case branches in switch statement.");
      return;
    }

    expression();
    consume(TOKEN_COLON, "Expect ':' after 'case'.");
    emitByte(OP_EQUAL_PEEK);

    // Jump to next case if false
    int nextJump = emitJump(OP_JUMP_IF_FALSE);

    // Pop result of comparison, leave switched-on value on stack
    emitByte(OP_POP);

    statement();

    // Pop switched-on value
    emitByte(OP_POP);

    int endJump = emitJump(OP_JUMP);
    endJumps[caseCount] = endJump;

    patchJump(nextJump);
    // Pop switched-on value
    emitByte(OP_POP);

    caseCount++;
  }

  if (match(TOKEN_DEFAULT)) {
    consume(TOKEN_COLON, "Expect ':' after 'default'.");
    statement();
    emitByte(OP_POP);
  }

  for (int i = 0; i < caseCount; i++) {
    patchJump(endJumps[i]);
  }

  endScope();
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after switch statement.");
}

static void continueStatement() {
  if (current->loopOffset == -1) {
    error("Cannot use 'continue' outside a loop");
    return;
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");

  for (int i = current->localCount - 1;
       i >= 0 && current->locals[i].depth > current->currentStackDepth; i--) {
    emitByte(OP_POP);
  }

  emitLoop(current->loopOffset);
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  Local *local = NULL;
  int arg = resolveLocal(current, &name, &local);
  bool isGlobalConstant = false;

  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, &name, &local)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    isGlobalConstant =
        tableFindString(&globalConsts, name.start, name.length) != NULL;
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    if ((local != NULL && local->isConst) || isGlobalConstant) {
      error("Cannot reassign constant variable.");
      return;
    }
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    emitBytes(getOp, (uint8_t)arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char *text) {
  Token token;
  token.start = text;
  token.length = (int)strlen(text);
  return token;
}

static void super_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'super' outside of a class.");
  } else if (!currentClass->hasSuperclass) {
    error("Can't use 'super' in a class with no superclass.");
  }

  consume(TOKEN_DOT, "Expect '.' after 'super'.");
  consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
  uint8_t name = identifierConstant(&parser.previous);

  namedVariable(syntheticToken("this"), false);
  if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
  } else {
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_GET_SUPER, name);
  }
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous);
  declareVariable(true);

  emitBytes(OP_CLASS, nameConstant);
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.enclosing = currentClass;
  classCompiler.hasSuperclass = false;
  currentClass = &classCompiler;

  if (match(TOKEN_LESS)) {
    consume(TOKEN_IDENTIFIER, "Expect superclass name.");
    variable(false);

    if (identifiersEqual(&className, &parser.previous)) {
      error("A class can't inherit from itself.");
    }

    beginScope();
    addLocal(syntheticToken("super"), true);
    defineVariable(0);

    namedVariable(className, false);
    emitByte(OP_INHERIT);
    classCompiler.hasSuperclass = true;
  }

  namedVariable(className, false);
  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  emitByte(OP_POP);

  if (classCompiler.hasSuperclass) {
    endScope();
  }

  currentClass = currentClass->enclosing;
}

static AstNode *declaration() {
  return statement();
  /* if (match(TOKEN_CLASS)) { */
  /*   classDeclaration(); */
  /* } else if (match(TOKEN_FUN)) { */
  /*   funDeclaration(); */
  /* } else if (match(TOKEN_CONST)) { */
  /*   varDeclaration(true); */
  /* } else if (match(TOKEN_VAR)) { */
  /*   varDeclaration(false); */
  /* } else { */
  /*   statement(); */
  /* } */
  if (parser.panicMode) {
    synchronize();
  }
}

static AstNode *statement() {
  if (match(TOKEN_PRINT)) {
    return printStatement();
  } else {
    return expressionStatement();
  }
  /* } else if (match(TOKEN_RETURN)) { */
  /*   returnStatement(); */
  /* } else if (match(TOKEN_FOR)) { */
  /*   forStatement(); */
  /* } else if (match(TOKEN_IF)) { */
  /*   ifStatement(); */
  /* } else if (match(TOKEN_WHILE)) { */
  /*   whileStatement(); */
  /* } else if (match(TOKEN_SWITCH)) { */
  /*   switchStatement(); */
  /* } else if (match(TOKEN_LEFT_BRACE)) { */
  /*   beginScope(); */
  /*   block(); */
  /*   endScope(); */
  /* } else if (match(TOKEN_CONTINUE)) { */
  /*   continueStatement(); */
  /* } else { */
  /*   expressionStatement(); */
  /* } */
}

static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static AstNode *number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  /* emitConstant(NUMBER_VAL(value)); */
  return newLiteralExpr(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool canAssign) {
  emitConstant(OBJ_VAL(
      copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void this_(bool canAssign) {
  variable(false);
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }
}

static void ternary(bool canAssign) {
  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);

  parsePrecedence(PREC_TERNARY);

  consume(TOKEN_COLON, "Expect ':' after ?.");

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_ASSIGNMENT);
  patchJump(elseJump);
}

static AstNode *unary(bool canAssign) {
  TokenType operatorType = parser.previous.type;

  // Compile the operand.
  AstNode *operand = parsePrecedence(PREC_UNARY);

  if (operatorType == TOKEN_BANG || operatorType == TOKEN_MINUS) {
    return newUnaryExpr(operand, operatorType);
  }
  return NULL;
}

static void postfixModification(bool canAssign, OpCode op) {
  uint8_t setOp;
  Local *local = NULL;
  Token *token = &parser.previousPrevious;
  int arg = resolveLocal(current, token, &local);
  bool isGlobalConstant = false;

  if (arg != -1) {
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, token, &local)) != -1) {
    setOp = OP_SET_UPVALUE;
  } else {
    isGlobalConstant =
        tableFindString(&globalConsts, token->start, token->length) != NULL;
    arg = identifierConstant(token);
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign) {
    if ((local != NULL && local->isConst) || isGlobalConstant) {
      error("Cannot reassign constant variable.");
      return;
    }
    emitConstant(NUMBER_VAL(1));
    emitByte(op);
    emitBytes(setOp, (uint8_t)arg);
  }
}

static void increment(bool canAssign) {
  postfixModification(canAssign, OP_ADD);
}

static void decrement(bool canAssign) {
  postfixModification(canAssign, OP_SUBTRACT);
}

static void arrayLiteral(bool canAssign) {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_BRACKET)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 array literal elements.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array items.");
  emitBytes(OP_ARRAY, argCount);
}

ParseRule rules[] = {
    /* [TOKEN_LEFT_PAREN] = {grouping, call, PREC_CALL}, */
    /* [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_LEFT_BRACKET] = {arrayLiteral, leftBracket, PREC_CALL}, */
    /* [TOKEN_RIGHT_BRACKET] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_COMMA] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_DOT] = {NULL, dot, PREC_CALL}, */
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_PERCENT] = {NULL, binary, PREC_FACTOR},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    /* [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_BANG_EQUAL] = {NULL, binary, PREC_EQUALITY}, */
    /* [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY}, */
    /* [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON}, */
    /* [TOKEN_IDENTIFIER] = {variable, NULL, PREC_NONE}, */
    /* [TOKEN_STRING] = {string, NULL, PREC_NONE}, */
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    /* [TOKEN_AND] = {NULL, and_, PREC_AND}, */
    /* [TOKEN_CLASS] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_ELSE] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_FALSE] = {literal, NULL, PREC_NONE}, */
    /* [TOKEN_FOR] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_FUN] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_IF] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_NIL] = {literal, NULL, PREC_NONE}, */
    /* [TOKEN_OR] = {NULL, or_, PREC_OR}, */
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    /* [TOKEN_RETURN] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_SUPER] = {super_, NULL, PREC_NONE}, */
    /* [TOKEN_THIS] = {this_, NULL, PREC_NONE}, */
    /* [TOKEN_TRUE] = {literal, NULL, PREC_NONE}, */
    /* [TOKEN_VAR] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_WHILE] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_ERROR] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_EOF] = {NULL, NULL, PREC_NONE}, */
    /* [TOKEN_QUESTION] = {NULL, ternary, PREC_OR}, */
    /* [TOKEN_PLUS_PLUS] = {NULL, increment, PREC_CALL}, */
    /* [TOKEN_MINUS_MINUS] = {NULL, decrement, PREC_CALL}, */
};

static AstNode *parsePrecedence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return NULL;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  AstNode *left = prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    AstNode *newNode = infixRule(canAssign);
    newNode->branches.left = left;
    left = newNode;
  }

  if (left != NULL) {
    return left;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }

  return NULL;
}

static ParseRule *getRule(TokenType type) { return &rules[type]; }

ObjFunction *compile(const char *source, FILE *ostream, FILE *errstream) {
  initScanner(source);

  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;
  parser.ostream = ostream;
  parser.errstream = errstream;

  initTable(&stringConstants);
  initTable(&globalConsts);

  advance();

  /* while (!match(TOKEN_EOF)) { */
  AstNode *ast = declaration();
  /* } */

  printAST(*ast, 0);
  CFG *cfg = newCFG(ast);
  printBasicBlock(cfg->start);

  Chunk *chunk = generateChunk(cfg->start);
  ObjFunction *function = newFunction();
  function->chunk = *chunk;

  disassembleChunk(chunk, "testing");

  /* ObjFunction *function = endCompiler(); */
  freeTable(&stringConstants);
  freeTable(&globalConsts);

  /* return NULL; */
  return parser.hadError ? NULL : function;
}

// TODO: add constants to this
void markCompilerRoots() {
  Compiler *compiler = current;
  while (compiler != NULL) {
    markObject((Obj *)compiler->function);
    compiler = compiler->enclosing;
  }
}
