#ifndef clox_scanner_h
#define clox_scanner_h

#include "token.h"
#include <stdbool.h>

bool identifiersEqual(Token *a, Token *b);
void initScanner(const char *source);
Token scanToken();

#endif
