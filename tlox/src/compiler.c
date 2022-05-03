#include <string.h>

#include "ast.h"
#include "cfg.h"
#include "chunk.h"
#include "codegen.h"
#include "common.h"
#include "compiler.h"
#include "linked_list.h"
#include "memory.h"
#include "parser.h"
#include "scanner.h"
#include "semantic.h"
#include "table.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

Table stringConstants;
Table globalConsts;
Table labels;
/* typedef struct ClassCompiler { */
/*   struct ClassCompiler *enclosing; */
/*   bool hasSuperclass; */
/* } ClassCompiler; */

/* ClassCompiler *currentClass = NULL; */

/* static void emitLoop(int loopStart) { */
/*   emitByte(OP_LOOP); */

/*   int offset = currentChunk()->count - loopStart + 2; */
/*   if (offset > UINT16_MAX) */
/*     error("Loop body too large."); */

/*   emitByte((offset >> 8) & 0xff); */
/*   emitByte(offset & 0xff); */
/* } */

/* static int emitJump(uint8_t instruction) { */
/*   emitByte(instruction); */
/*   emitByte(0xff); */
/*   emitByte(0xff); */
/*   return currentChunk()->count - 2; */
/* } */

/* static void emitReturn() { */
/*   if (current->type == TYPE_INITIALIZER) { */
/*     emitBytes(OP_GET_LOCAL, 0); */
/*   } else { */
/*     emitByte(OP_NIL); */
/*   } */
/*   emitByte(OP_RETURN); */
/* } */

/* static uint8_t makeConstant(Value value) { */
/*   int constant = addConstant(currentChunk(), value); */
/*   if (constant > UINT8_MAX) { */
/*     error("Too many constants in one chunk."); */
/*     return 0; */
/*   } */

/*   return (uint8_t)constant; */
/* } */

/* static void emitConstant(Value value) { */
/*   emitBytes(OP_CONSTANT, makeConstant(value)); */
/* } */

/* static void patchJump(int offset) { */
/*   // -2 to adjust for the bytecode for the jump offset itself. */
/*   int jump = currentChunk()->count - offset - 2; */

/*   if (jump > UINT16_MAX) { */
/*     error("Too much code to jump over."); */
/*   } */

/*   currentChunk()->code[offset] = (jump >> 8) & 0xff; */
/*   currentChunk()->code[offset + 1] = jump & 0xff; */
/* } */

void errorAt(Compiler *compiler, Token *token, const char *message) {
  // TODO: does panicMode better belong on parser?
  if (compiler->panicMode)
    return;
  compiler->panicMode = true;
  fprintf(compiler->errstream, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(compiler->errstream, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(compiler->errstream, " at '%.*s'", token->length, token->start);
  }

  fprintf(compiler->errstream, ": %s\n", message);
  compiler->hadError = true;
}

void error(Compiler *compiler, const char *message) {
  errorAt(compiler, &compiler->parser->previous, message);
}

void errorAtCurrent(Compiler *compiler, const char *message) {
  errorAt(compiler, &compiler->parser->current, message);
}

static void initScope(Scope *scope, Compiler *compiler, FunctionType type) {
  scope->enclosing = compiler->currentScope;

  scope->localCount = 0;
  scope->scopeDepth = 0;
  scope->loopOffset = -1;

  compiler->currentScope = scope;

  Local *local = &scope->locals[scope->localCount++];
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

/* static ObjFunction *endCompiler() { */
/*   emitReturn(); */

/*   ObjFunction *function = current->function; */
/* #ifdef DEBUG_PRINT_CODE */
/*   if (!parser.hadError) { */
/*     disassembleChunk(currentChunk(), function->name != NULL */
/*                                          ? function->name->chars */
/*                                          : "<script>"); */
/*   } */
/* #endif */

/*   current = current->enclosing; */
/*   return function; */
/* } */

/* static int resolveLocal(OldCompiler *compiler, Token *name, */
/*                         Local **foundLocal) { */
/*   for (int i = compiler->localCount - 1; i >= 0; i--) { */
/*     Local *local = &compiler->locals[i]; */
/*     if (identifiersEqual(name, &local->name)) { */
/*       if (local->depth == -1) { */
/*         error("Can't read local variable in its own initializer."); */
/*       } */
/*       *foundLocal = local; */
/*       return i; */
/*     } */
/*   } */

/*   return -1; */
/* } */

/* static int addUpvalue(OldCompiler *compiler, uint8_t index, bool isLocal) {
 */
/*   int upvalueCount = compiler->function->upvalueCount; */

/*   for (int i = 0; i < upvalueCount; i++) { */
/*     Upvalue *upvalue = &compiler->upvalues[i]; */
/*     if (upvalue->index == index && upvalue->isLocal == isLocal) { */
/*       return i; */
/*     } */
/*   } */

/*   if (upvalueCount == UINT8_COUNT) { */
/*     error("Too many closure variables in function."); */
/*     return 0; */
/*   } */

/*   compiler->upvalues[upvalueCount].isLocal = isLocal; */
/*   compiler->upvalues[upvalueCount].index = index; */
/*   return compiler->function->upvalueCount++; */
/* } */

/* static int resolveUpvalue(OldCompiler *compiler, Token *name, */
/*                           Local **localVar) { */
/*   if (compiler->enclosing == NULL) { */
/*     return -1; */
/*   } */

/*   int local = resolveLocal(compiler->enclosing, name, localVar); */
/*   if (local != -1) { */
/*     compiler->enclosing->locals[local].isCaptured = true; */
/*     return addUpvalue(compiler, (uint8_t)local, true); */
/*   } */

/*   int upvalue = resolveUpvalue(compiler->enclosing, name, localVar); */
/*   if (upvalue != -1) { */
/*     return addUpvalue(compiler, (uint8_t)upvalue, false); */
/*   } */

/*   return -1; */
/* } */

/* static void addLocal(Token name, bool isConst) { */
/*   if (current->localCount == UINT8_COUNT) { */
/*     error("Too many local variables in function."); */
/*     return; */
/*   } */

/*   Local *local = &current->locals[current->localCount++]; */
/*   local->name = name; */
/*   local->depth = -1; */
/*   local->isCaptured = false; */
/*   local->isConst = isConst; */
/* } */

// only does locals
/* static void declareVariable(bool isConst) { */
/*   Token *name = &parser.previous; */

/*   if (current->scopeDepth == 0) { */
/*     return; */
/*   } */

/*   for (int i = current->localCount - 1; i >= 0; i--) { */
/*     Local *local = &current->locals[i]; */
/*     if (local->depth != -1 && local->depth < current->scopeDepth) { */
/*       break; */
/*     } */

/*     if (identifiersEqual(name, &local->name)) { */
/*       error("Already a variable with this name in this scope."); */
/*     } */
/*   } */

/*   addLocal(*name, isConst); */
/* } */

/* static uint8_t parseVariable(bool isConst, const char *errorMessage) { */
/*   consume(TOKEN_IDENTIFIER, errorMessage); */

/*   Token token = parser.previous; */
/*   if (tableFindString(&globalConsts, token.start, token.length)) { */
/*     error("Cannot redeclare a const variable"); */
/*     return 0; */
/*   } */

/*   declareVariable(isConst); */

/*   if (current->scopeDepth > 0) { */
/*     return 0; */
/*   } */
/*   // we are in global scope. Check for a global variable with the same name
 */
/*   if (searchConstantsFor(OBJ_VAL(copyString(token.start, token.length))) !=
 */
/*       -1) { */
/*     error("Already a variable with this name in this scope"); */
/*     return 0; */
/*   } */

/*   if (isConst) { */
/*     ObjString *varName = makeString(token.start, token.length); */
/*     tableSet(&globalConsts, OBJ_VAL(varName), TRUE_VAL); */
/*   } */

/*   return identifierConstant(&token); */
/* } */

/* static void markInitialized() { */
/*   if (current->scopeDepth == 0) { */
/*     return; */
/*   } */
/*   current->locals[current->localCount - 1].depth = current->scopeDepth; */
/* } */

/* static void defineVariable(uint8_t global) { */
/*   if (current->scopeDepth > 0) { */
/*     markInitialized(); */
/*     return; */
/*   } */

/*   emitBytes(OP_DEFINE_GLOBAL, global); */
/* } */

/* static uint8_t argumentList() { */
/*   uint8_t argCount = 0; */
/*   if (!check(TOKEN_RIGHT_PAREN)) { */
/*     do { */
/*       expression(); */
/*       if (argCount == 255) { */
/*         error("Can't have more than 255 arguments."); */
/*       } */
/*       argCount++; */
/*     } while (match(TOKEN_COMMA)); */
/*   } */
/*   consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments."); */
/*   return argCount; */
/* } */

/* static void and_(bool canAssign) { */
/*   int endJump = emitJump(OP_JUMP_IF_FALSE); */

/*   emitByte(OP_POP); */
/*   parsePrecedence(PREC_AND); */

/*   patchJump(endJump); */
/* } */

/* static void call(bool canAssign) { */
/*   uint8_t argCount = argumentList(); */
/*   emitBytes(OP_CALL, argCount); */
/* } */

/* static void dot(bool canAssign) { */
/*   consume(TOKEN_IDENTIFIER, "Expect property name after '.'."); */
/*   uint8_t name = identifierConstant(&parser.previous); */

/*   if (canAssign && match(TOKEN_EQUAL)) { */
/*     expression(); */
/*     emitBytes(OP_SET_PROPERTY, name); */
/*   } else if (match(TOKEN_LEFT_PAREN)) { */
/*     uint8_t argCount = argumentList(); */
/*     emitBytes(OP_INVOKE, name); */
/*     emitByte(argCount); */
/*   } else { */
/*     emitBytes(OP_GET_PROPERTY, name); */
/*   } */
/* } */

/* static void leftBracket(bool canAssign) { */
/*   expression(); */
/*   consume(TOKEN_RIGHT_BRACKET, "Expect ']' after expression."); */

/*   if (canAssign && match(TOKEN_EQUAL)) { */
/*     expression(); */
/*     emitByte(OP_SET_COMPUTED_PROPERTY); */
/*   } else { */
/*     emitByte(OP_GET_COMPUTED_PROPERTY); */
/*   } */
/* } */

/* static void function(FunctionType type) { */
/*   OldCompiler compiler; */
/*   initCompiler(&compiler, type); */
/*   beginScope(); */

/*   consume(TOKEN_LEFT_PAREN, "Expect '(' after function name."); */
/*   if (!check(TOKEN_RIGHT_PAREN)) { */
/*     do { */
/*       current->function->arity++; */
/*       if (current->function->arity > 255) { */
/*         errorAtCurrent("Can't have more than 255 parameters."); */
/*       } */
/*       /\* uint8_t constant = parseVariable(false, "Expect parameter name.");
 * *\/ */
/*       /\* defineVariable(constant); *\/ */
/*     } while (match(TOKEN_COMMA)); */
/*   } */
/*   consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters."); */
/*   consume(TOKEN_LEFT_BRACE, "Expect '{' before function body."); */
/*   block(); */

/*   ObjFunction *function = endCompiler(); */
/*   if (function->upvalueCount > 0 || */
/*       (type == TYPE_INITIALIZER || type == TYPE_METHOD)) { */
/*     emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function))); */
/*     for (int i = 0; i < function->upvalueCount; i++) { */
/*       emitByte(compiler.upvalues[i].isLocal ? 1 : 0); */
/*       emitByte(compiler.upvalues[i].index); */
/*     } */
/*   } else { */
/*     emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function))); */
/*   } */
/* } */

/* static void method() { */
/*   consume(TOKEN_IDENTIFIER, "Expect method name."); */
/*   uint8_t constant = identifierConstant(&parser.previous); */

/*   FunctionType type = TYPE_METHOD; */
/*   if (parser.previous.length == 4 && */
/*       memcmp(parser.previous.start, "init", 4) == 0) { */
/*     type = TYPE_INITIALIZER; */
/*   } */
/*   function(type); */
/*   emitBytes(OP_METHOD, constant); */
/* } */

/* static void funDeclaration() { */
/*   /\* uint8_t global = parseVariable(true, "Expect function name."); *\/ */
/*   /\* markInitialized(); *\/ */
/*   /\* function(TYPE_FUNCTION); *\/ */
/*   /\* defineVariable(global); *\/ */
/* } */

/* static void whileStatement() { */
/*   int oldLoopOffset = current->loopOffset; */
/*   int oldStackDepth = current->currentStackDepth; */
/*   current->loopOffset = currentChunk()->count; */
/*   current->currentStackDepth = current->scopeDepth; */

/*   consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'."); */
/*   expression(); */
/*   consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition."); */

/*   int exitJump = emitJump(OP_JUMP_IF_FALSE); */
/*   emitByte(OP_POP); */
/*   statement(); */
/*   emitLoop(current->loopOffset); */

/*   patchJump(exitJump); */
/*   emitByte(OP_POP); */
/*   current->loopOffset = oldLoopOffset; */
/*   current->currentStackDepth = oldStackDepth; */
/* } */

/* static void forStatement() { */
/*   beginScope(); */
/*   consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'."); */

/*   int oldLoopOffset = current->loopOffset; */
/*   int oldStackDepth = current->currentStackDepth; */

/*   if (match(TOKEN_SEMICOLON)) { */
/*     // No initializer. */
/*   } else if (match(TOKEN_VAR)) { */
/*     varDeclaration(false); */
/*   } else { */
/*     expressionStatement(); */
/*   } */

/*   current->loopOffset = currentChunk()->count; */
/*   current->currentStackDepth = current->scopeDepth; */

/*   int exitJump = -1; */
/*   if (!match(TOKEN_SEMICOLON)) { */
/*     expression(); */
/*     consume(TOKEN_SEMICOLON, "Expect ';' after loop condition."); */

/*     // Jump out of the loop if the condition is false. */
/*     exitJump = emitJump(OP_JUMP_IF_FALSE); */
/*     emitByte(OP_POP); // Condition. */
/*   } */

/*   if (!match(TOKEN_RIGHT_PAREN)) { */
/*     int bodyJump = emitJump(OP_JUMP); */
/*     int incrementStart = currentChunk()->count; */

/*     expression(); */
/*     emitByte(OP_POP); */
/*     consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses."); */

/*     emitLoop(current->loopOffset); */
/*     current->loopOffset = incrementStart; */
/*     patchJump(bodyJump); */
/*   } */

/*   statement(); */

/*   emitLoop(current->loopOffset); */

/*   if (exitJump != -1) { */
/*     patchJump(exitJump); */
/*     emitByte(OP_POP); // Condition. */
/*   } */

/*   current->loopOffset = oldLoopOffset; */
/*   current->currentStackDepth = oldStackDepth; */

/*   endScope(); */
/* } */

/* static void switchStatement() { */
/*   consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'."); */
/*   expression(); */
/*   consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression."); */
/*   consume(TOKEN_LEFT_BRACE, "Expect '{' after parenthesis."); */

/*   beginScope(); */

/*   int endJumps[UINT8_COUNT]; */
/*   int caseCount = 0; */

/*   while (match(TOKEN_CASE) && !check(TOKEN_EOF)) { */
/*     if (caseCount == UINT8_COUNT) { */
/*       error("Too many case branches in switch statement."); */
/*       return; */
/*     } */

/*     expression(); */
/*     consume(TOKEN_COLON, "Expect ':' after 'case'."); */
/*     emitByte(OP_EQUAL_PEEK); */

/*     // Jump to next case if false */
/*     int nextJump = emitJump(OP_JUMP_IF_FALSE); */

/*     // Pop result of comparison, leave switched-on value on stack */
/*     emitByte(OP_POP); */

/*     statement(); */

/*     // Pop switched-on value */
/*     emitByte(OP_POP); */

/*     int endJump = emitJump(OP_JUMP); */
/*     endJumps[caseCount] = endJump; */

/*     patchJump(nextJump); */
/*     // Pop switched-on value */
/*     emitByte(OP_POP); */

/*     caseCount++; */
/*   } */

/*   if (match(TOKEN_DEFAULT)) { */
/*     consume(TOKEN_COLON, "Expect ':' after 'default'."); */
/*     statement(); */
/*     emitByte(OP_POP); */
/*   } */

/*   for (int i = 0; i < caseCount; i++) { */
/*     patchJump(endJumps[i]); */
/*   } */

/*   endScope(); */
/*   consume(TOKEN_RIGHT_BRACE, "Expect '}' after switch statement."); */
/* } */

/* static void continueStatement() { */
/*   if (current->loopOffset == -1) { */
/*     error("Cannot use 'continue' outside a loop"); */
/*     return; */
/*   } */
/*   consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'."); */

/*   for (int i = current->localCount - 1; */
/*        i >= 0 && current->locals[i].depth > current->currentStackDepth; i--)
 * { */
/*     emitByte(OP_POP); */
/*   } */

/*   emitLoop(current->loopOffset); */
/* } */

// TODO: how to represent variable ref in AST?
// This is all codegen?
/* static AstNode *namedVariable(Token name, bool canAssign) { */
/*   uint8_t getOp, setOp; */
/*   Local *local = NULL; */
/*   int arg = resolveLocal(current, &name, &local); */
/*   bool isGlobalConstant = false; */

/*   if (arg != -1) { */
/*     getOp = OP_GET_LOCAL; */
/*     setOp = OP_SET_LOCAL; */
/*   } else if ((arg = resolveUpvalue(current, &name, &local)) != -1) { */
/*     getOp = OP_GET_UPVALUE; */
/*     setOp = OP_SET_UPVALUE; */
/*   } else { */
/*     isGlobalConstant = */
/*         tableFindString(&globalConsts, name.start, name.length) != NULL; */
/*     arg = identifierConstant(&name); */
/*     getOp = OP_GET_GLOBAL; */
/*     setOp = OP_SET_GLOBAL; */
/*   } */

/*   if (canAssign && match(TOKEN_EQUAL)) { */
/*     if ((local != NULL && local->isConst) || isGlobalConstant) { */
/*       error("Cannot reassign constant variable."); */
/*       return NULL; */
/*     } */
/*     expression(); */
/*     emitBytes(setOp, (uint8_t)arg); */
/*   } else { */
/*     emitBytes(getOp, (uint8_t)arg); */
/*   } */
/* } */

/* static Token syntheticToken(const char *text) { */
/*   Token token; */
/*   token.start = text; */
/*   token.length = (int)strlen(text); */
/*   return token; */
/* } */

/* static void super_(bool canAssign) { */
/*   if (currentClass == NULL) { */
/*     error("Can't use 'super' outside of a class."); */
/*   } else if (!currentClass->hasSuperclass) { */
/*     error("Can't use 'super' in a class with no superclass."); */
/*   } */

/*   consume(TOKEN_DOT, "Expect '.' after 'super'."); */
/*   consume(TOKEN_IDENTIFIER, "Expect superclass method name."); */
/*   uint8_t name = identifierConstant(&parser.previous); */

/*   namedVariable(syntheticToken("this"), false); */
/*   if (match(TOKEN_LEFT_PAREN)) { */
/*     uint8_t argCount = argumentList(); */
/*     namedVariable(syntheticToken("super"), false); */
/*     emitBytes(OP_SUPER_INVOKE, name); */
/*     emitByte(argCount); */
/*   } else { */
/*     namedVariable(syntheticToken("super"), false); */
/*     emitBytes(OP_GET_SUPER, name); */
/*   } */
/* } */

/* static void classDeclaration() { */
/*   consume(TOKEN_IDENTIFIER, "Expect class name."); */
/*   Token className = parser.previous; */
/*   uint8_t nameConstant = identifierConstant(&parser.previous); */
/*   declareVariable(true); */

/*   emitBytes(OP_CLASS, nameConstant); */
/*   defineVariable(nameConstant); */

/*   ClassCompiler classCompiler; */
/*   classCompiler.enclosing = currentClass; */
/*   classCompiler.hasSuperclass = false; */
/*   currentClass = &classCompiler; */

/*   if (match(TOKEN_LESS)) { */
/*     consume(TOKEN_IDENTIFIER, "Expect superclass name."); */
/*     variable(false); */

/*     if (identifiersEqual(&className, &parser.previous)) { */
/*       error("A class can't inherit from itself."); */
/*     } */

/*     beginScope(); */
/*     addLocal(syntheticToken("super"), true); */
/*     defineVariable(0); */

/*     namedVariable(className, false); */
/*     emitByte(OP_INHERIT); */
/*     classCompiler.hasSuperclass = true; */
/*   } */

/*   namedVariable(className, false); */
/*   consume(TOKEN_LEFT_BRACE, "Expect '{' before class body."); */
/*   while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) { */
/*     method(); */
/*   } */
/*   consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body."); */
/*   emitByte(OP_POP); */

/*   if (classCompiler.hasSuperclass) { */
/*     endScope(); */
/*   } */

/*   currentClass = currentClass->enclosing; */
/* } */

/* static void grouping(bool canAssign) { */
/*   expression(); */
/*   consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression."); */
/* } */

/* static void or_(bool canAssign) { */
/*   int elseJump = emitJump(OP_JUMP_IF_FALSE); */
/*   int endJump = emitJump(OP_JUMP); */

/*   patchJump(elseJump); */
/*   emitByte(OP_POP); */

/*   parsePrecedence(PREC_OR); */
/*   patchJump(endJump); */
/* } */

/* static void string(bool canAssign) { */
/*   emitConstant(OBJ_VAL( */
/*       copyString(parser.previous.start + 1, parser.previous.length - 2))); */
/* } */

/* static void this_(bool canAssign) { */
/*   variable(false); */
/*   if (currentClass == NULL) { */
/*     error("Can't use 'this' outside of a class."); */
/*     return; */
/*   } */
/* } */

/* static void ternary(bool canAssign) { */
/*   int thenJump = emitJump(OP_JUMP_IF_FALSE); */
/*   emitByte(OP_POP); */

/*   parsePrecedence(PREC_TERNARY); */

/*   consume(TOKEN_COLON, "Expect ':' after ?."); */

/*   int elseJump = emitJump(OP_JUMP); */

/*   patchJump(thenJump); */
/*   emitByte(OP_POP); */

/*   parsePrecedence(PREC_ASSIGNMENT); */
/*   patchJump(elseJump); */
/* } */

/* static void postfixModification(bool canAssign, OpCode op) { */
/*   uint8_t setOp; */
/*   Local *local = NULL; */
/*   Token *token = &parser.previousPrevious; */
/*   int arg = resolveLocal(current, token, &local); */
/*   bool isGlobalConstant = false; */

/*   if (arg != -1) { */
/*     setOp = OP_SET_LOCAL; */
/*   } else if ((arg = resolveUpvalue(current, token, &local)) != -1) { */
/*     setOp = OP_SET_UPVALUE; */
/*   } else { */
/*     isGlobalConstant = */
/*         tableFindString(&globalConsts, token->start, token->length) != NULL;
 */
/*     arg = identifierConstant(token); */
/*     setOp = OP_SET_GLOBAL; */
/*   } */

/*   if (canAssign) { */
/*     if ((local != NULL && local->isConst) || isGlobalConstant) { */
/*       error("Cannot reassign constant variable."); */
/*       return; */
/*     } */
/*     emitConstant(NUMBER_VAL(1)); */
/*     emitByte(op); */
/*     emitBytes(setOp, (uint8_t)arg); */
/*   } */
/* } */

/* static void increment(bool canAssign) { */
/*   postfixModification(canAssign, OP_ADD); */
/* } */

/* static void decrement(bool canAssign) { */
/*   postfixModification(canAssign, OP_SUBTRACT); */
/* } */

/* static void arrayLiteral(bool canAssign) { */
/*   uint8_t argCount = 0; */
/*   if (!check(TOKEN_RIGHT_BRACKET)) { */
/*     do { */
/*       expression(); */
/*       if (argCount == 255) { */
/*         error("Can't have more than 255 array literal elements."); */
/*       } */
/*       argCount++; */
/*     } while (match(TOKEN_COMMA)); */
/*   } */
/*   consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array items."); */
/*   emitBytes(OP_ARRAY, argCount); */
/* } */

ObjFunction *compile(Compiler *compiler, const char *source) {
  // FIXME: move to Compiler
  initScanner(source);

  initTable(&stringConstants);
  initTable(&globalConsts);
  initTable(&labels);

  Scope scope;
  initScope(&scope, compiler, TYPE_SCRIPT);

  CompilerState state = {.stringConstants = &stringConstants,
                         .globalConsts = &globalConsts,
                         .labels = &labels};

  ObjString *functionName = NULL;
  if (compiler->type != TYPE_SCRIPT) {
    functionName = copyString(compiler->parser->previous.start,
                              compiler->parser->previous.length);
  }

  AstNode *ast = parse(compiler->parser);
#ifdef DEBUG_PRINT_CODE
  printAST(*ast, 0);
#endif
  if (compiler->hadError) {
    return NULL;
  }

  analyse(ast, compiler, &state);

  if (compiler->hadError) {
    return NULL;
  }

  CFG *cfg = newCFG(state, ast);
#ifdef DEBUG_PRINT_CODE
  printCFG(cfg);
#endif

  Chunk *chunk = generateChunk(cfg, &labels);
  ObjFunction *function = newFunction();
  function->chunk = *chunk;
  function->name = functionName;

#ifdef DEBUG_PRINT_CODE
  disassembleChunk(chunk,
                   functionName != NULL ? functionName->chars : "<script>");
#endif

  /* ObjFunction *function = endCompiler(); */
  freeTable(&stringConstants);
  freeTable(&globalConsts);
  freeTable(&labels);

  return compiler->hadError ? NULL : function;
}

Compiler initCompiler(FunctionType type, FILE *ostream, FILE *errstream) {
  Parser parser = initParser();
  Compiler compiler = {.hadError = false,
                       .panicMode = false,
                       .ostream = ostream,
                       .errstream = errstream,
                       .parser = &parser,
                       .currentScope = NULL,
                       .type = type};
  parser.compiler = &compiler; // FIXME: is this stack reference bad?
  return compiler;
}
