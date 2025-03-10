// Module to create a "chunk" (i.e. sequence) of bytecode instructions.

#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"

// Each bytecode instruction has a one-byte
// operation code which controls its behaviour.
typedef enum {
    // Return from the current function.
    OP_RETURN,
} OpCode;

// Dynamic array storing a chunk of bytecode instructions.
typedef struct {
    // Standard variables needed to implement a dynamic array.
    int count;
    int capacity;

    // Store the instructions.
    uint8_t* code;
} Chunk;

void initChunk(Chunk* chunk);

// Free chunk memory once no longer needed.
void freeChunk(Chunk* chunk);

// Append an instruction to the chunk.
void writeChunk(Chunk* chunk, uint8_t byte);

#endif