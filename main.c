#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {

    // Temporary testing code.
    // Currently testing OP_VALUE and a
    // manually loaded constant.
    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT);
    writeChunk(&chunk, constant);
    writeChunk(&chunk, OP_RETURN);

    // Disassemble the test chunk and
    // print its contents. We expect it to contain
    // a single OP_RETURN.
    disassembleChunk(&chunk, "test chunk");

    freeChunk(&chunk);

    return 0;
}