// The clox virtual machine.
// Executes chunks of bytecode instructions.

#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

// Stores all state needed to run a chunk.
typedef struct {
    Chunk* chunk;

    // Instruction pointer.
    uint8_t* ip;

    // Provides a stack to store temporary variables.
    Value stack[STACK_MAX];
    // Stack pointer. Points to where the next value
    // will go. An empty stack has stackTop = 0.
    Value* stackTop;
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
InterpretResult interpret(const char* source);

// Manipulate the value stack.
void push(Value value);
Value pop();

#endif