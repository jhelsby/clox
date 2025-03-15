// The clox virtual machine.
// Executes chunks of bytecode instructions.

#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"

// Stores all state needed to run a chunk.
typedef struct {
    Chunk* chunk;

    // Instruction pointer.
    uint8_t* ip;
} VM;

// Used to set the exit code of a process
// for error reporting.
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(Chunk* chunk);

#endif