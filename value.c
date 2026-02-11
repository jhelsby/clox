#include <stdio.h>
#include <string.h>

#include "object.h"
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
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
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

// Print the value of the given constant.
void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        printf("nil");
    } else if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        printObject(value);
    }
#else
    switch (value.type) {
        case VAL_BOOL:
          printf(AS_BOOL(value) ? "true" : "false");
          break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
    }
#endif
}

// Check two values are equal by comparing and unwrapping their types.
bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    // Special case for numbers when NaN boxing, because the
    // IEEE 754 spec mandates that NaN != NaN.
    // This is more inefficient though, and could be omitted
    // for performance if we don't care if NaN != NaN.
    if (IS_NUMBER(a) && IS_NUMBER(b)) {
        return AS_NUMBER(a) == AS_NUMBER(b);
    }
    // If NaN-boxing, two non-number Values are equal iff their bit representations are equal.
    return a == b;
#else
    if (a.type != b.type) return false;
    switch (a.type) {
        case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:    return true;
        case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);

        // Due to string interning implemented in
        // table.c, two strings are the same if
        // their memory addresses are the same.
        // No need to check each character here.
        case VAL_OBJ:    return AS_OBJ(a) == AS_OBJ(b);

        default:         return false; // Unreachable.
    }
#endif
}