#ifndef clox_parser_h
#define clox_parser_h

#include "ast.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include <stdbool.h>
#include <stdio.h>

typedef struct Parser {
  Token current;
  Token previous;
  Token previousPrevious;
  Compiler *compiler; // Non-owning reference for convenience
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

typedef AstNode *(*ParseFn)(Parser *parser, bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

AstNode *parse(Parser *parser);

#endif // clox_parser_h
