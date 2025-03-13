#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

// Print each instruction in the given chunk.
void disassembleChunk(Chunk* chunk, const char* name);

// Given a chunk and a byte offset, print the
// offset and the instruction stored there.
int disassembleInstruction(Chunk* chunk, int offset);

#endif