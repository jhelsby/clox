#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    initValueArray(&chunk->constants);
}

// Append a byte to our chunk.
// Standard append implementation for a dynamic array.
void writeChunk(Chunk* chunk, uint8_t byte) {
    // Check if array is full.
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;

        // Set the capacity to some suitably larger number.
        chunk->capacity = GROW_CAPACITY(oldCapacity);

        // Create or grow the array to the new
        // capacity, allocating memory as needed.
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    // Append the new instruction to the chunk.
    chunk->code[chunk->count] = byte;

    // Increment count to include the new instruction.
    chunk->count++;
}

void freeChunk(Chunk* chunk) {
    // Deallocate chunk memory.
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);

    // Free the constants stored in our chunk.
    freeValueArray(&chunk->constants);

    // Zero out the fields, so the chunk
    // is in well-defined empty state.
    initChunk(chunk);
}

// Add a new constant to the chunk's value pool.
// Returns the index the constant was stored at.
int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}