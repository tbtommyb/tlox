#include "semantic.h"
#include "ast.h"

void analyse(AstNode *node, Compiler *compiler, CompilerState *state) {
  if (node == NULL) {
    return;
  }
  switch (node->type) {
  case STMT_DEFINE: {
    ObjString *nameString = copyString(node->token.start, node->token.length);
    if (tableFindString(state->globalConsts, nameString->chars,
                        nameString->length)) {
      errorAt(compiler, &node->token, "Cannot redefine a const variable.");
      return;
    }
    break;
  }
  case STMT_DEFINE_CONST: {
    ObjString *nameString = copyString(node->token.start, node->token.length);
    bool isGlobalConstant =
        tableFindString(state->globalConsts, nameString->chars,
                        nameString->length) != NULL;
    if (isGlobalConstant) {
      errorAt(compiler, &node->token, "Cannot redefine a const variable.");
      return;
    }
    // FIXME use HashSet
    tableSet(state->globalConsts, OBJ_VAL(nameString), TRUE_VAL);
    break;
  }
  case STMT_MODULE: {
    Node *stmtNode = (Node *)node->stmts->head;

    while (stmtNode != NULL) {
      analyse(stmtNode->data, compiler, state);
      stmtNode = stmtNode->next;
    }
  }
  }
}
