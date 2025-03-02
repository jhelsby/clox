// A collection of low-level memory operations
// used throughout the clox interpreter.

#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"

// Set dynamic array capacity. If current capacity
// is zero, set it to eight. Otherwise, double it.
#define GROW_CAPACITY(capacity) \
    ((capacity < 8 ? 8 : (capacity) * 2))

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif