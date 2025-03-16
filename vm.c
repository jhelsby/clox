#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "vm.h"

// Use a global variable (!?) to access the VM.
// We only need one VM here, and a global variable
// simplifies the implementation. Probably not a
// good idea for larger systems!
VM vm;

static void resetStack() {
    // Reset by setting the stack top to the beginning
    // of the stack. No need to erase old values -
    // we can just overwrite them.
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {
}

// Push onto the value stack.
void push(Value value) {
    // Add the value to the top of the stack,
    // which stackTop points to.
    *vm.stackTop = value;

    vm.stackTop++;
}

// Pop from the value stack.
Value pop() {
    vm.stackTop--;

    return *vm.stackTop;
}


// Decode and execute each instruction in the VM.
static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

    for (;;) {

// If the VM's diagnostic logging is active,
// print the instruction with its chunk offset.
#ifdef DEBUG_TRACE_EXECUTION
        disassembleInstruction(vm.chunk,
            // Convert the instruction pointer into
            // a relative offset from the beginning
            // of the chunk.
            (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();

                // Used for testing purposes so we know
                // OP_CONSTANT is being interpreted.
                printValue(constant);
                printf("\n");

                break;
            }
            case OP_RETURN: {
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;

    // Instruction pointer. Points to the
    // instruction _about_ to be executed.
    vm.ip = vm.chunk->code;

    return run();
}