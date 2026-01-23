// A collection of low-level memory operations
// used throughout the clox interpreter.

#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"

// Allocate an array with the given element type and count.
#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))

// Free an array. We use reallocate to do this instead of free()
// because we'll add a running count of allocated bytes
// via reallocate later.
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

// Set dynamic array capacity. If current capacity
// is zero, set it to eight. Otherwise, double it.
#define GROW_CAPACITY(capacity) \
    ((capacity < 8 ? 8 : (capacity) * 2))

// Allocates dynamic array memory based on
// its type, contents, and desired new size.
#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

// Frees dynamic array memory based on its type and contents.
#define FREE_ARRAY(type, pointer, oldCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), 0)

// Single function used for all dynamic memory management
// in clox -  allocation, freeing, and reallocation.
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

// Call Lox's tracing garbage collector to clean up unreachable objects.
void collectGarbage();

// Free all allocated objects.
void freeObjects();

#endif