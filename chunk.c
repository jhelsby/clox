#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

// Append a byte to our chunk, storing its line number
// for error reporting.
//
// Standard append implementation for a dynamic array.
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    // Check if array is full.
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;

        // Set the capacity to some suitably larger number.
        chunk->capacity = GROW_CAPACITY(oldCapacity);

        // Create or grow the arrays to the new
        // capacity, allocating memory as needed.
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
    }

    // Append the new instruction to the chunk.
    chunk->code[chunk->count] = byte;

    // Store the line number of the instruction.
    chunk->lines[chunk->count] = line;

    // Increment count to include the new instruction.
    chunk->count++;
}

void freeChunk(Chunk* chunk) {
    // Deallocate chunk memory.
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);

    // Free the constants stored in our chunk.
    freeValueArray(&chunk->constants);

    // Zero out the fields, so the chunk
    // is in well-defined empty state.
    initChunk(chunk);
}

// Add a new constant to the chunk's constant pool.
// Returns the index the constant was stored at.
int addConstant(Chunk* chunk, Value value) {
    // writeValueArray may call reallocate(), which will trigger a GC.
    // We must ensure the constant is stored on the Lox stack first
    // to prevent it from being cleaned up by the GC before we write it
    // to the constant pool. See:
    // https://craftinginterpreters.com/garbage-collection.html#adding-to-the-constant-table
    push(value);

    writeValueArray(&chunk->constants, value);

    // Once the value has been written, we no longer need on the stack -
    // the GC will find it in the constant pool.
    pop();

    return chunk->constants.count - 1;
}