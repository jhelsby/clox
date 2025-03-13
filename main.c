#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {

    // Temporary testing code.
    // Manually create a chunk, append one 
    // OP_RETURN instruction to it, 
    // print its contents, and free it.
    Chunk chunk;
    initChunk(&chunk);
    writeChunk(&chunk, OP_RETURN);

    // Disassemble the test chunk and 
    // print its contents. We expect it to contain 
    // a single OP_RETURN.
    disassembleChunk(&chunk, "test chunk");

    freeChunk(&chunk);

    return 0;
}