// The clox virtual machine.
// Executes chunks of bytecode instructions.

#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"

// Stores all state needed to run a chunk.
// Will be expanded later.
typedef struct {
    Chunk* chunk;
} VM;

void initVM();
void freeVM();

#endif