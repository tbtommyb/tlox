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
  printAST(ast, 0);
#endif
  if (compiler->hadError) {
    return NULL;
  }

  analyse(ast, compiler);

  if (compiler->hadError) {
    return NULL;
  }

  WorkUnit *mainWorkUnit = createMainWorkUnit(compiler, ast);

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
