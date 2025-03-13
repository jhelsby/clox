#include "common.h"
#include "chunk.h"

int main(int argc, const char* argv[]) {

    // Temporary testing code.
    // Manually create a chunk, append one 
    // OP_RETURN instruction to it, and free it.
    Chunk chunk;
    initChunk(&chunk);
    writeChunk(&chunk, OP_RETURN);
    freeChunk(&chunk);

    return 0;
}