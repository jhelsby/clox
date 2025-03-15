#include <stdio.h>

#include "common.h"
#include "vm.h"

// Use a global variable (!?) to access the VM.
// We only need one VM here, and a global variable
// simplifies the implementation. Probably not a
// good idea for larger systems!
VM vm;

void initVM() {
}

void freeVM() {
}

// Decode and execute each instruction in the VM.
static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

    for (;;) {
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