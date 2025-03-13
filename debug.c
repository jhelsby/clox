#include <stdio.h>

#include "debug.h"
#include "value.h"

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

static int constantInstruction(const char* name, Chunk* chunk, int offset) {

    // Retrieve the constant's value pool index.
    uint8_t constant = chunk->code[offset + 1];

    printf("%-16s %4d '", name, constant);

    // Print the actual value of the constant.
    printValue(chunk->constants.values[constant]);

    printf("'\n");

    // The OP_CONSTANT instruction is two bytes -
    // one for the opcode and one for the operand.
    return offset + 2;
}

int disassembleInstruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}