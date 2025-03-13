#include <stdio.h>

#include "debug.h"

// Given a chunk and a byte offset, print the
// offset and the instruction stored there.
void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== &s ==\n", name);

    // Iterate through the chunk, one instruction
    // at a time. Instead of incrementing offset
    // in the loop, we let disassembleInstruction 
    // do it for us. This is because instructions 
    // can have different sizes.
    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction(chunk, offset);
    }
}

// Helper function for disassembleInstruction.
// Used to print a one-byte instruction.
static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);

    // A simple instruction has length 1,
    // so ends at offset + 1.
    return offset + 1;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}