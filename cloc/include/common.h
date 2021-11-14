#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEBUG_TRACE_EXECUTION
#undef DEBUG_TRACE_EXECUTION

#define DEBUG_PRINT_CODE
#define DEBUG_LOG_GC
#define DEBUG_STRESS_GC
#define UINT8_COUNT (UINT8_MAX + 1)
#undef DEBUG_LOG_GC
#undef DEBUG_STRESS_GC
#define NAN_BOXING

#endif
