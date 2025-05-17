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
    // Load a constant from the chunk constant pool.
    // Use an operand to specify which constant.
    OP_CONSTANT,

    // Define dedicated one-byte instructions for these common literals.
    OP_NIL,
    OP_TRUE,
    OP_FALSE,

    // Pop the top value of the stack and forget it.
    OP_POP,

    // Load a local variable.
    OP_GET_LOCAL,

    // Load a global variable.
    OP_GET_GLOBAL,

    // Define a global variable. The instruction's
    // operand is the index of the variable's name
    // in the chunk's constant table.
    OP_DEFINE_GLOBAL,

    // Assign a local variable.
    OP_SET_LOCAL,

    // Assign a global variable.
    OP_SET_GLOBAL,

    // Operators that return booleans.
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,

    // Standard binary arithmetic operators.
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,

    // Perform the logical not operation !b on a boolean b.
    OP_NOT,

    // Perform the operation -(x) on a number x.
    OP_NEGATE,

    // Evaluate and print an expression.
    OP_PRINT,

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

    // Store chunk constants in a constant pool.
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);

// Append an instruction to the chunk, storing
// its line number for error reporting.
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// Free chunk memory once no longer needed.
void freeChunk(Chunk* chunk);

// Convenience function to add a new
// constant to the chunk's constant pool.
int addConstant(Chunk*, Value value);

#endif