// Module to create a "chunk" (i.e. sequence) of bytecode instructions.
// Our compiler will transform user code into a series of chunks,
// which can then be efficiently deserialised and executed by the VM.


#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

// Each bytecode instruction has a one-byte
// operation code which controls its behaviour.
typedef enum {
    // Load a constant from the chunk value pool.
    // Use an operand to specify which constant.
    OP_CONSTANT,

    // Perform the operation -(x) on a number x.
    OP_NEGATE,

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

    // Store the line number of each
    // instruction, for error reporting.
    int* lines;

    // Store chunk constants in a value pool.
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);

// Append an instruction to the chunk, storing
// its line number for error reporting.
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// Free chunk memory once no longer needed.
void freeChunk(Chunk* chunk);

// Convenience function to add a new
// constant to the chunk's value pool.
int addConstant(Chunk*, Value value);

#endif