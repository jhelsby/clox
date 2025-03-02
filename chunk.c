#include <stdlib.h>

#include "chunk.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
}

// Standard append implementation for a dynamic array.
void writeChunk(Chunk* chunk, uint8_t byte) {
    // CHeck if array is full.
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;

        // Set the capacity to some suitably larger number.
        chunk->capacity = GROW_CAPACITY(oldCapacity);

        // Create or grow the array to the new 
        // capacity, allocating memory as needed.
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }
}