#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_ADD,
  OP_CALL,
  OP_CLOSURE,
  OP_CONSTANT,
  OP_CONSTANT_LONG,
  OP_DEFINE_GLOBAL,
  OP_DIVIDE,
  OP_EQUAL,
  OP_FALSE,
  OP_GREATER,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LESS,
  OP_LOOP,
  OP_MULTIPLY,
  OP_NEGATE,
  OP_NIL,
  OP_NOT,
  OP_POP,
  OP_GET_LOCAL,
  OP_GET_GLOBAL,
  OP_PRINT,
  OP_RETURN,
  OP_CLASS,
  OP_SET_LOCAL,
  OP_SET_GLOBAL,
  OP_SUBTRACT,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_CLOSE_UPVALUE,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,
  OP_METHOD,
  OP_INVOKE,
  OP_TRUE
} OpCode;

typedef struct {
  int line;
  int offset;
} LineStart;

typedef struct {
  int count;
  int capacity;
  uint8_t *code;
  ValueArray constants;
  int lineCount;
  int lineCapacity;
  LineStart *lines;
} Chunk;

void initChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, uint8_t byte, int line);
void freeChunk(Chunk *chunk);

int addConstant(Chunk *chunk, Value value);
void writeConstant(Chunk *chunk, Value value, int line);
int getLine(Chunk *chunk, int instruction);

#endif
