#include <stdio.h>

#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

// Append a value to our array.
// Standard append implementation for a dynamic array.
void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;

        // Set the capacity to some suitably larger number.
        array->capacity = GROW_CAPACITY(oldCapacity);

        // Create or grow the array to the new
        // capacity, allocating memory as needed.
        GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    // Append the new value to the array.
    array->values[array->count] = value;

    // Increment count to include the new value.
    array->count++;
}

void freeValueArray(ValueArray* array) {
    // Deallocate array memory.
    FREE_ARRAY(Value, array->values, array->capacity);

    // Zero out the fields, so the array
    // is in well-defined empty state.
    initValueArray(array);
}