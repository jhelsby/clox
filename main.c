#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
    // The VM is used throughout the interpreter's lifetime.
    initVM();

    // Temporary testing code.
    // Currently testing the binary arithmetic operations.
    // Specifically, the expression -((1.2 + 3.4) / 5.6).
    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1.2);

    // Pass in arbitrary line number (123) for testing.
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);

    constant = addConstant(&chunk, 3.4);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);

    writeChunk(&chunk, OP_ADD, 123);

    constant = addConstant(&chunk, 5.6);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);

    writeChunk(&chunk, OP_DIVIDE, 123);

    writeChunk(&chunk, OP_NEGATE, 123);

    writeChunk(&chunk, OP_RETURN, 123);

    // Disassemble the test chunk and
    // print its contents.
    disassembleChunk(&chunk, "test chunk");

    // Tell the VM to interpret the current bytecode chunk.
    interpret(&chunk);

    freeVM();
    freeChunk(&chunk);

    return 0;
}