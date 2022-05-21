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
#include "scope.h"
#include "semantic.h"
#include "symbol_table.h"
#include "table.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

Table stringConstants;
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

/* static void and_(bool canAssign) { */
/*   int endJump = emitJump(OP_JUMP_IF_FALSE); */

/*   emitByte(OP_POP); */
/*   parsePrecedence(PREC_AND); */

/*   patchJump(endJump); */
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

/* static void or_(bool canAssign) { */
/*   int elseJump = emitJump(OP_JUMP_IF_FALSE); */
/*   int endJump = emitJump(OP_JUMP); */

/*   patchJump(elseJump); */
/*   emitByte(OP_POP); */

/*   parsePrecedence(PREC_OR); */
/*   patchJump(endJump); */
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
  initScanner(source);

  initTable(&stringConstants);
  // should this still be global?
  initTable(&labels);

  Scope *scope = scope_allocate(TYPE_SCRIPT);
  scope_init(scope, compiler);

  CompilerState state = {.stringConstants = &stringConstants,
                         .functions = linkedList_allocate(),
                         .labels = &labels};

  AstNode *ast = parse(compiler->parser);
#ifdef DEBUG_PRINT_CODE
  printAST(*ast, 0);
#endif
  if (compiler->hadError) {
    return NULL;
  }

  analyse(ast, compiler, TYPE_SCRIPT);

  if (compiler->hadError) {
    return NULL;
  }

  WorkUnit *mainWorkUnit = createWorkUnits(compiler, &state, ast);

  if (compiler->hadError) {
    return NULL;
  }

#ifdef DEBUG_PRINT_CODE
  printWorkUnits(mainWorkUnit);
#endif

  ObjFunction *mainFunction = compileWorkUnit(compiler, mainWorkUnit, &labels);

  if (compiler->hadError) {
    return NULL;
  }

#ifdef DEBUG_PRINT_CODE
  disassembleWorkUnits(mainWorkUnit);
#endif

  freeTable(&stringConstants);
  freeTable(&labels);

  return compiler->hadError ? NULL : mainFunction;
}

void initCompiler(Parser *parser, Compiler *compiler, FunctionType type,
                  FILE *ostream, FILE *errstream) {
  compiler->hadError = false;
  compiler->panicMode = false;
  compiler->ostream = ostream;
  compiler->errstream = errstream;
  compiler->parser = parser;
  compiler->currentScope = NULL;
  compiler->type = type;
  parser->compiler = compiler; // FIXME: is this stack reference bad?
  return;
}
