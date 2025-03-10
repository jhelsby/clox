#include <stdlib.h>

#include "memory.h"

// Handles all dynamic memory management in clox.
// Uses a void* pointer so we can reallocate raw memory.
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    // realloc() allocates new memory if none is
    // allocated, or resizes memory if the allocated
    // size is changing.
    //
    // When growing memory, it allocates a new block,
    // copies over the old bytes, frees the old block,
    // and returns a pointer to the new block - 
    // just as required when growing our dynamic array.
    void* result = realloc(pointer, newSize);

    // Handle "running out of memory" error case.
    if (result == NULL) exit(1);

    return result;
}