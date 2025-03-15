#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {

    // Temporary testing code.
    // Currently testing line information.
    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1.2);

    // Pass in arbitrary line number (123) for testing.
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);
    writeChunk(&chunk, OP_RETURN, 123);

    // Disassemble the test chunk and
    // print its contents.
    disassembleChunk(&chunk, "test chunk");

    freeChunk(&chunk);

    return 0;
}