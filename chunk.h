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

    // Assign or retrieve a variable from an enclosing function's local scope.
    OP_GET_UPVALUE,
    OP_SET_UPVALUE,

    // Getters and setters for fields and methods in classes.
    OP_GET_PROPERTY,
    OP_SET_PROPERTY,

    // Get the method corresponding to a subclass accessing its superclass via super.method.
    OP_GET_SUPER,

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

    // Created after we've executed an if-statement's "then" branch,
    // to jump over the "else" branch.
    OP_JUMP,

    // If an if-condition is false, jump over its "then" branch.
    OP_JUMP_IF_FALSE,

    // Jump back to a given byte offset. For for- and while- loops.
    OP_LOOP,

    // Indicate a function call.
    OP_CALL,

    // Optimise calling a method by using a single INVOKE instruction
    // instead of first creating an ObjBoundMethod and then calling it.
    OP_INVOKE,

    // Optimise calling a super method if called directly, similar to INVOKE.
    OP_SUPER_INVOKE,

    // Indicate that we should load a closure.
    OP_CLOSURE,

    // Indicate that we should move an upvalue from the stack to the heap.
    // This happens if the local variable goes out of scope but has
    // been closed over by a function which is still in scope.
    OP_CLOSE_UPVALUE,

    // Return from the current function.
    OP_RETURN,

    // Create a class.
    OP_CLASS,

    // Bind a superclass to a subclass for inheritance.
    // The superclass should be second on the stack, and the subclass at the top.
    OP_INHERIT,

    // Bind a method to the most recently created class.
    OP_METHOD
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