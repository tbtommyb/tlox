#include "semantic.h"
#include "ast.h"
#include "compiler.h"
#include "symbol_table.h"
#include <stdbool.h>

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

static bool isInClassScope(Scope *scope) {
  Scope *curr = scope;
  while (curr->enclosing != NULL) {
    if (curr->type == TYPE_CLASS) {
      return true;
    }
    curr = curr->enclosing;
  }
  return false;
}

// TODO: do a function-only pass first so that functions don't need to be
// declared before use
void analyse(AstNode *node, Compiler *compiler) {
  if (node == NULL) {
    return;
  }
  switch (AST_NODE_TYPE(node)) {
  case EXPR_LITERAL:
  case EXPR_NIL:
    break;
  case EXPR_BINARY: {
    BinaryExprAstNode *expr = AS_BINARY_EXPR(node);
    analyse(expr->branches.left, compiler);
    analyse(expr->branches.right, compiler);
    break;
  }
  case EXPR_UNARY: {
    analyse(AS_UNARY_EXPR(node)->right, compiler);
    break;
  }
  case EXPR_AND: {
    analyse(AS_AND_EXPR(node)->branches.left, compiler);
    analyse(AS_AND_EXPR(node)->branches.right, compiler);
    break;
  }
  case EXPR_OR: {
    analyse(AS_OR_EXPR(node)->branches.left, compiler);
    analyse(AS_OR_EXPR(node)->branches.right, compiler);
    break;
  }
  case EXPR_VARIABLE: {
    Symbol *symbol = NULL;
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &symbol)) {
      errorAt(compiler, &node->token, "Undefined variable.");
      break;
    }
    if (!symbol->isDefined) {
      errorAt(compiler, &node->token,
              "Cannot reference variable in its own initialiser.");
      break;
    }
    break;
  }
  case STMT_ASSIGN: {
    Symbol *symbol = NULL;
    if (!scope_search(compiler->currentScope, node->token.start,
                      node->token.length, &symbol)) {
      errorAt(compiler, &node->token, "Undefined variable.");
      break;
    }
    if (symbol->isConst) {
      errorAt(compiler, &node->token, "Cannot reassign a const variable.");
      break;
    }
    analyse(AS_ASSIGN_STMT(node)->expr, compiler);
    break;
  }
  case STMT_DEFINE: {
    DefineStmtAstNode *stmt = AS_DEFINE_STMT(node);
    Symbol *existing = NULL;
    if (scope_current_search(compiler->currentScope, node->token.start,
                             node->token.length, &existing)) {
      if (existing->isConst) {
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
    Symbol symbol =
        newSymbol(node->token, scopeType, false, stmt->isConst, false, 0);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    analyse(stmt->expr, compiler);

    symbol.isDefined = true;
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);
    break;
  }
  case STMT_MODULE: {
    node->scope = compiler->currentScope;
    ModuleStmtAstNode *stmt = AS_MODULE_STMT(node);
    Node *stmtNode = (Node *)stmt->stmts->head;

    while (stmtNode != NULL) {
      analyse(stmtNode->data, compiler);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    BlockStmtAstNode *stmt = AS_BLOCK_STMT(node);
    Node *blockNode = (Node *)stmt->stmts->head;

    node->scope = beginScope(compiler, compiler->currentScope->type);

    while (blockNode != NULL) {
      analyse(blockNode->data, compiler);
      blockNode = blockNode->next;
    }

    endScope(compiler);
    break;
  }
  case STMT_PRINT: {
    analyse(AS_PRINT_STMT(node)->expr, compiler);
    break;
  }
  case STMT_EXPR: {
    ExprStmtAstNode *stmt = AS_EXPR_STMT(node);
    analyse(stmt->expr, compiler);
    break;
  }
  case STMT_RETURN: {
    if (compiler->currentScope->type != TYPE_FUNCTION &&
        compiler->currentScope->type != TYPE_METHOD &&
        compiler->currentScope->type != TYPE_INITIALIZER) {
      errorAt(compiler, &node->token, "Can't return from top-level code.");
    }
    /* if (parser->compiler->type == TYPE_INITIALIZER) { */
    /*   error(parser->compiler, "Can't return a value from an
initializer.");
     */
    /* } */
    analyse(AS_RETURN_STMT(node)->expr, compiler);
    break;
  }
  case STMT_IF: {
    IfStmtAstNode *stmt = AS_IF_STMT(node);
    analyse(stmt->condition, compiler);
    analyse(stmt->branches.then, compiler);
    analyse(stmt->branches.elseB, compiler);
    break;
  }
  case STMT_WHILE: {
    WhileStmtAstNode *stmt = AS_WHILE_STMT(node);
    analyse(stmt->condition, compiler);
    analyse(stmt->branches.then, compiler);
    break;
  }
  case STMT_FOR: {
    ForStmtAstNode *stmt = AS_FOR_STMT(node);
    node->scope = beginScope(compiler, compiler->currentScope->type);

    analyse(stmt->branches.pre, compiler);
    analyse(stmt->branches.cond, compiler);
    analyse(stmt->branches.post, compiler);
    analyse(stmt->branches.body, compiler);

    endScope(compiler);
    break;
  }
  case STMT_FUNCTION: {
    FunctionStmtAstNode *stmt = AS_FUNCTION_STMT(node);
    Symbol *existing = NULL;
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, &existing)) {
      if (existing->isConst) {
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
    int arity = stmt->expr->arity;
    // FIXME: need better symbol creation.
    Symbol symbol = newSymbol(node->token, scopeType, false, true, true, arity);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    // FIXME: hack
    AS_AST_NODE(stmt->expr)->token = node->token;
    analyse(AS_AST_NODE(stmt->expr), compiler);
    break;
  }
  case EXPR_FUNCTION: {
    FunctionExprAstNode *expr = AS_FUNCTION_EXPR(node);
    node->scope = beginScope(compiler, expr->functionType);

    int arity = 0;
    for (; arity < expr->arity;) {
      Token name = expr->params[arity];
      Symbol symbol =
          newSymbol(name, SCOPE_FUNCTION_PARAM, false, true, true, 0);
      st_set(compiler->currentScope->st, name.start, name.length, symbol);
      arity++;
    }

    analyse(AS_AST_NODE(expr->body), compiler);

    endScope(compiler);

    if (expr->functionType == TYPE_FUNCTION) {
      // Create empty symbol on heap
      Symbol *functionSymbol = NULL;
      bool found = st_get(compiler->currentScope->st, node->token.start,
                          node->token.length, &functionSymbol);
      if (!found) {
        errorAt(compiler, &node->token, "Function definition is not in scope.");
        break;
      }
      functionSymbol->arity = arity;
      /* st_set(compiler->currentScope->st, node->token.start,
       * node->token.length, */
      /*        functionSymbol); */
    }
    break;
  }
  case EXPR_CALL: {
    CallExprAstNode *expr = AS_CALL_EXPR(node);
    analyse(expr->target, compiler);
    Node *paramNode = (Node *)expr->params->head;

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
    InvocationExprAstNode *expr = AS_INVOCATION_EXPR(node);
    analyse(expr->target, compiler);

    Node *paramNode = (Node *)expr->params->head;

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
  case EXPR_THIS: {
    if (!isInClassScope(compiler->currentScope)) {
      errorAt(compiler, &node->token, "Can't use 'this' outside of a class.");
    }
    break;
  }
  case STMT_CLASS: {
    ClassStmtAstNode *stmt = AS_CLASS_STMT(node);
    if (scope_search(compiler->currentScope, node->token.start,
                     node->token.length, NULL)) {
      errorAt(compiler, &node->token,
              "Already a variable with this name in this scope.");
    }

    ScopeType scopeType =
        isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
    // FIXME: need better symbol creation.
    Symbol symbol = newSymbol(node->token, scopeType, false, false, true, 0);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    node->scope = beginScope(compiler, TYPE_CLASS);

    if (OPTIONAL_HAS_VALUE(stmt->superclass)) {
      Token superclass = OPTIONAL_VALUE(stmt->superclass);
      if (identifiersEqual(&node->token, &superclass)) {
        errorAt(compiler, &superclass, "A class can't inherit from itself.");
      }
      node->scope = beginScope(compiler, TYPE_CLASS);
      Token super = {.start = "super", .length = (int)strlen("super")};
      Symbol symbol = newSymbol(super, SCOPE_LOCAL, false, false, true, 0);
      scope_set(compiler->currentScope, super.start, super.length, symbol);
    }

    /* stmt->body->superclass = stmt->superclass; */
    analyse(AS_AST_NODE(stmt->body), compiler);

    if (OPTIONAL_HAS_VALUE(stmt->superclass)) {
      endScope(compiler);
    }
    endScope(compiler);

    break;
  }
  case STMT_CLASS_BODY: {
    ClassBodyStmtAstNode *stmt = AS_CLASS_BODY_STMT(node);
    Node *methodNode = (Node *)stmt->stmts->head;
    while (methodNode != NULL) {
      analyse(methodNode->data, compiler);
      methodNode = methodNode->next;
    }

    break;
  }
  case STMT_METHOD: {
    MethodStmtAstNode *stmt = AS_METHOD_STMT(node);
    if (scope_current_search(compiler->currentScope, node->token.start,
                             node->token.length, NULL)) {
      errorAt(compiler, &node->token,
              "Already a method with this name in this scope.");
      break;
    }

    int arity = stmt->body->arity;
    Symbol symbol =
        newSymbol(node->token, SCOPE_GLOBAL, false, true, true, arity);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    // FIXME: hack
    AS_AST_NODE(stmt->body)->token = node->token;
    analyse(AS_AST_NODE(stmt->body), compiler);
    break;
  }
  case STMT_SET_PROPERTY: {
    SetPropertyStmtAstNode *stmt = AS_SET_PROPERTY_STMT(node);
    ScopeType scopeType =
        isGlobalScope(compiler->currentScope) ? SCOPE_GLOBAL : SCOPE_LOCAL;
    // FIXME: what to do with arity here?
    Symbol symbol = newSymbol(node->token, scopeType, false, false, true, 0);
    scope_set(compiler->currentScope, node->token.start, node->token.length,
              symbol);

    analyse(stmt->target, compiler);
    analyse(stmt->expr, compiler);
    break;
  }
  case EXPR_GET_PROPERTY: {
    analyse(AS_GET_PROPERTY_EXPR(node)->target, compiler);
    break;
  }
  case EXPR_SUPER: {
    // TODO: check for superclass
    if (!isInClassScope(compiler->currentScope)) {
      errorAt(compiler, &node->token, "Can't use 'super' outside of a class.");
      break;
    }
    analyse(AS_SUPER_EXPR(node)->target, compiler);
    break;
  }
  case EXPR_SUPER_INVOKE: {
    InvocationExprAstNode *expr = AS_INVOCATION_EXPR(node);
    if (!isInClassScope(compiler->currentScope)) {
      errorAt(compiler, &node->token, "Can't use 'super' outside of a class.");
      break;
    }
    // TODO: need a way to know what class is being used in order to do semantic
    // checks
    analyse(expr->target, compiler);

    Node *paramNode = (Node *)expr->params->head;

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
  }
}
