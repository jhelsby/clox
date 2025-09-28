// The clox virtual machine.
// Executes chunks of bytecode instructions.

#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// Store a function and its local variables.
// A frame on the call stack.
typedef struct {
  ObjFunction* function;
  // Instruction pointer for the current function.
  uint8_t* ip;
  // Points to the first slot in the VM's value stack which this function can use.
  Value* slots;
} CallFrame;

// Stores all state needed to run a chunk.
typedef struct {
    // Provides a CallStack to store function calls.
    CallFrame frames[FRAMES_MAX];
    int frameCount;

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