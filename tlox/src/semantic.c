#include "semantic.h"
#include "ast.h"
#include "compiler.h"
#include "symbol_table.h"

static Scope *beginScope(Compiler *compiler, FunctionType type) {
  Scope *scope = scope_allocate(type);
  scope_init(scope, compiler);
  compiler->currentScope = scope;
  return scope;
}

static void endScope(Compiler *compiler) {
  compiler->currentScope = compiler->currentScope->enclosing;
}

static bool isGlobalScope(Scope *scope) {
  return scope != NULL && scope->enclosing == NULL;
}

static Scope *getGlobalScope(Scope *scope) {
  Scope *curr = scope;
  while (curr->enclosing != NULL) {
    curr = curr->enclosing;
  }
  return curr;
}

// TODO: do a function-only pass first so that functions don't need to be
// declared before use
void analyse(AstNode *node, Compiler *compiler) {
  if (node == NULL) {
    return;
  }
  bool variableIsConst = false;
  switch (node->type) {
  case EXPR_BINARY: {
    analyse(node->branches.left, compiler);
    analyse(node->branches.right, compiler);
    break;
  }
  case EXPR_UNARY: {
    analyse(node->branches.right, compiler);
    break;
  }
  case EXPR_AND: {
    analyse(node->branches.left, compiler);
    analyse(node->branches.right, compiler);
    break;
  }
  case EXPR_OR: {
    analyse(node->branches.left, compiler);
    analyse(node->branches.right, compiler);
    break;
  }
  case EXPR_VARIABLE: {
    Symbol symbol = {0};
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &symbol)) {
      errorAt(compiler, &node->token, "Undefined variable.");
      break;
    }
    if (!symbol.isDefined) {
      errorAt(compiler, &node->token,
              "Cannot reference variable in its own initialiser.");
      break;
    }
    break;
  }
  case STMT_ASSIGN: {
    Symbol symbol = {0};
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &symbol)) {
      errorAt(compiler, &node->token, "Undefined variable.");
      break;
    }
    if (symbol.isConst) {
      errorAt(compiler, &node->token, "Cannot reassign a const variable.");
      break;
    }
    break;
  }
  case STMT_DEFINE_CONST: {
    variableIsConst = true;
    /* fallthrough */
  }
  case STMT_DEFINE: {
    Symbol existing = {0};
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, &existing)) {
      if (existing.isConst) {
        errorAt(compiler, &node->token, "Cannot redefine a const variable.");
        break;
      } else {
        errorAt(compiler, &node->token,
                "Already a variable with this name in this scope.");
        break;
      }
    }
    ScopeType scopeType =
        isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
    Symbol *symbol =
        newSymbol(node->token, scopeType, false, variableIsConst, false, 0);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    analyse(node->expr, compiler);

    symbol->isDefined = true;
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);
    break;
  }
  case STMT_MODULE: {
    node->scope = compiler->currentScope;
    Node *stmtNode = (Node *)node->stmts->head;

    while (stmtNode != NULL) {
      analyse(stmtNode->data, compiler);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    Node *blockNode = (Node *)node->stmts->head;

    node->scope = beginScope(compiler, compiler->currentScope->type);

    while (blockNode != NULL) {
      analyse(blockNode->data, compiler);
      blockNode = blockNode->next;
    }

    endScope(compiler);
    break;
  }
  case STMT_PRINT: {
    analyse(node->expr, compiler);
    break;
  }
  case STMT_EXPR: {
    analyse(node->expr, compiler);
    break;
  }
  case STMT_RETURN: {
    if (compiler->currentScope->type != TYPE_FUNCTION) {
      errorAt(compiler, &node->token, "Can't return from top-level code.");
    }
    /* if (parser->compiler->type == TYPE_INITIALIZER) { */
    /*   error(parser->compiler, "Can't return a value from an initializer.");
     */
    /* } */
    analyse(node->expr, compiler);
    break;
  }
  case STMT_IF: {
    analyse(node->expr, compiler);
    analyse(node->branches.left, compiler);
    analyse(node->branches.right, compiler);
    break;
  }
  case STMT_WHILE: {
    analyse(node->expr, compiler);
    analyse(node->branches.left, compiler);
    break;
  }
  case STMT_FOR: {
    node->scope = beginScope(compiler, compiler->currentScope->type);

    analyse(node->preExpr, compiler);
    analyse(node->condExpr, compiler);
    analyse(node->postExpr, compiler);
    analyse(node->expr, compiler);

    endScope(compiler);
    break;
  }
  case STMT_FUNCTION: {
    Symbol existing = {0};
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, &existing)) {
      if (existing.isConst) {
        errorAt(compiler, &node->token, "Cannot redefine a const variable.");
        break;
      } else {
        errorAt(compiler, &node->token,
                "Already a variable with this name in this scope.");
        break;
      }
    }
    ScopeType scopeType =
        isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
    // FIXME: need better symbol creation.
    Symbol *symbol =
        newSymbol(node->token, scopeType, false, true, true, node->arity);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    node->expr->token = node->token;
    analyse(node->expr, compiler);
    break;
  }
  case EXPR_FUNCTION: {
    node->scope = beginScope(compiler, node->functionType);

    Node *paramNode = (Node *)node->params->head;

    int arity = 0;
    while (paramNode != NULL) {
      Token *name = paramNode->data;
      Symbol *symbol =
          newSymbol(*name, SCOPE_FUNCTION_PARAM, false, true, true, 0);
      st_set(compiler->currentScope->st, name->start, name->length, symbol);
      paramNode = paramNode->next;
      arity++;
    }

    analyse(node->expr, compiler);

    endScope(compiler);

    if (node->functionType == TYPE_FUNCTION) {
      // Create empty symbol on heap
      Symbol *functionSymbol =
          newSymbol(node->token, SCOPE_GLOBAL, false, false, false, 0);
      bool found = st_get(compiler->currentScope->st, node->token.start,
                          node->token.length, functionSymbol);
      if (!found) {
        errorAt(compiler, &node->token, "Function definition is not in scope.");
        break;
      }
      functionSymbol->arity = arity;
      st_set(compiler->currentScope->st, node->token.start, node->token.length,
             functionSymbol);
    }
    break;
  }
  case EXPR_CALL: {
    analyse(node->branches.left, compiler);
    Node *paramNode = (Node *)node->params->head;

    // FIXME: do something with arity
    int callArity = 0;
    while (paramNode != NULL) {
      analyse(paramNode->data, compiler);
      paramNode = paramNode->next;
      callArity++;
    }

    node->scope = compiler->currentScope;
    break;
  }
  case EXPR_INVOKE: {
    analyse(node->branches.left, compiler);

    Node *paramNode = (Node *)node->params->head;

    // FIXME: do something with arity
    int callArity = 0;
    while (paramNode != NULL) {
      analyse(paramNode->data, compiler);
      paramNode = paramNode->next;
      callArity++;
    }

    node->scope = compiler->currentScope;
    break;
  }
  case STMT_CLASS: {
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, &(Symbol){0})) {
      errorAt(compiler, &node->token,
              "Already a variable with this name in this scope.");
    }

    ScopeType scopeType =
        isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
    // FIXME: need better symbol creation.
    Symbol *symbol = newSymbol(node->token, scopeType, false, true, true, 0);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    node->scope = beginScope(compiler, TYPE_CLASS);

    if (node->superclass.length > 0) {
      if (identifiersEqual(&node->token, &node->superclass)) {
        errorAt(compiler, &node->superclass,
                "A class can't inherit from itself.");
      }
      Token super = {.start = "super", .length = (int)strlen("super")};
      Symbol *symbol = newSymbol(super, SCOPE_LOCAL, false, false, true, 0);
      scope_set(compiler->currentScope, super.start, super.length, symbol);
    }

    analyse(node->expr, compiler);

    endScope(compiler);

    break;
  }
  case STMT_CLASS_BODY: {
    Node *methodNode = (Node *)node->methods->head;
    while (methodNode != NULL) {
      analyse(methodNode->data, compiler);
      methodNode = methodNode->next;
    }

    break;
  }
  case STMT_METHOD: {
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, &(Symbol){0})) {
      errorAt(compiler, &node->token,
              "Already a variable with this name in this scope.");
      break;
    }

    Symbol *symbol =
        newSymbol(node->token, SCOPE_GLOBAL, false, true, true, node->arity);
    // FIXME: temporary hack until a type system allows us to look up correct
    // scope
    Scope *global = getGlobalScope(compiler->currentScope);
    scope_set(global, node->token.start, node->token.length, symbol);

    node->expr->token = node->token;
    analyse(node->expr, compiler);
    break;
  }
  case STMT_SET_PROPERTY: {
    /* ScopeType scopeType = */
    /*     isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
     */
    /* // FIXME: need better symbol creation. */
    /* Symbol *symbol = newSymbol(node->token, scopeType, false, true, true, 0);
     */
    /* scope_set(compiler->currentScope, node->token.start, node->token.length,
     */
    /*           symbol); */
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &(Symbol){0})) {
      errorAt(compiler, &node->token, "Property not found.");
    }
    analyse(node->expr, compiler);
    break;
  }
  case EXPR_GET_PROPERTY: {
    /* ScopeType scopeType = */
    /*     isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
     */
    /* // FIXME: need better symbol creation. */
    /* Symbol *symbol = newSymbol(node->token, scopeType, false, true, true, 0);
     */
    /* scope_set(compiler->currentScope, node->token.start, node->token.length,
     */
    /*           symbol); */
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &(Symbol){0})) {
      errorAt(compiler, &node->token, "Property not found.");
    }
    break;
  }
  }
}
