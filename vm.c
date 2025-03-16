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

// This macro simplifies binary operations,
// especially for type checking.
//
// The do-while block ensures the statements are the
// same scope when macro is expanded. For details, see:
// https://craftinginterpreters.com/a-virtual-machine.html#binary-operators
#define BINARY_OP(op) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(a op b); \
    } while (false)

    for (;;) {

// If active, handle VM's diagnostic logging.
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");

        // Print stack trace - every stack value in order.
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");

        // Print the instruction being executed,
        // with its chunk offset.
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

                // Add the constant to the value stack.
                push(constant);

                break;
            }

            // Binary arithmetic operators.
            case OP_ADD:        BINARY_OP(+); break;
            case OP_SUBTRACT:   BINARY_OP(-); break;
            case OP_MULTIPLY:   BINARY_OP(*); break;
            case OP_DIVIDE:     BINARY_OP(/); break;

            // Negate the top of the stack.
            case OP_NEGATE: push(-pop()); break;

            case OP_RETURN: {

                // Temporary, for testing.
                // Returning a function will pop the stack
                // and print the first value before exiting.
                printValue(pop());
                printf("\n");

                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;

    // Instruction pointer. Points to the
    // instruction _about_ to be executed.
    vm.ip = vm.chunk->code;

    return run();
}