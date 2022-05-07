#include "semantic.h"
#include "ast.h"
#include "compiler.h"
#include "symbol_table.h"

static SymbolTable *beginScope(Compiler *compiler) {
  SymbolTable *childSt = st_allocate();
  st_init(childSt, compiler->currentScope->st);
  compiler->currentScope->st = childSt;
  return childSt;
}

static void endScope(Compiler *compiler) {
  compiler->currentScope->st = compiler->currentScope->st->parent;
}

static bool isGlobalScope(Scope *scope) {
  return scope->st != NULL && scope->st->parent == NULL;
}

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
  case EXPR_VARIABLE: {
    Symbol symbol = {0};
    if (!st_search(compiler->currentScope->st, node->token.start,
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
    if (!st_search(compiler->currentScope->st, node->token.start,
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
    if (st_search(compiler->currentScope->st, node->token.start,
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
        newSymbol(node->token, scopeType, false, variableIsConst, false);
    st_set(compiler->currentScope->st, node->token.start, node->token.length,
           symbol);

    analyse(node->expr, compiler);

    symbol->isDefined = true;
    st_set(compiler->currentScope->st, node->token.start, node->token.length,
           symbol);
    break;
  }
  case STMT_MODULE: {
    Node *stmtNode = (Node *)node->stmts->head;

    while (stmtNode != NULL) {
      analyse(stmtNode->data, compiler);
      stmtNode = stmtNode->next;
    }
    break;
  }
  case STMT_BLOCK: {
    Node *blockNode = (Node *)node->stmts->head;

    node->st = beginScope(compiler);
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
  case STMT_IF: {
    analyse(node->expr, compiler);
    analyse(node->branches.left, compiler);
    analyse(node->branches.right, compiler);
    break;
  }
  }
}
