#include "util.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

char *writeString(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int bytes = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);

  char *output = malloc(bytes + 1);
  va_start(ap, fmt);
  vsnprintf(output, bytes + 1, fmt, ap);
  va_end(ap);

  return output;
}
