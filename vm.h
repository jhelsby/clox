// The clox virtual machine.
// Executes chunks of bytecode instructions.

#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
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

    // Store global variables in (name, value) form, in a hash table.
    Table globals;

    // For string interning. We never store duplicate
    // strings - if two strings are equal, deduplicate so
    // we can refer to both using the same memory address.
    Table strings;

    // Head of our linked list of allocated Objs. For GC.
    Obj* objects;
} VM;

// Used to set the exit code of a process
// for error reporting.
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

// Allow object module to access the VM so it can store
// its list of allocated objects in the VM, for GC.
extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);

// Manipulate the value stack.
void push(Value value);
Value pop();

#endif