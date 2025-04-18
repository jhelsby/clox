// To store values in a chunk in clox, we use a constant pool (or "constant table").
// Each chunk stores values in a "constants" array separate
// to its instructions. To reference a value, we point to
// the appropriate offset in this array.
//
// We could store small values directly in the codestream,
// after an opcode - known as intermediate instructions -
// but omit this here to minimise complexity.

#ifndef clox_value_h
#define clox_value_h

#include "common.h"

// Initially, the only constants our implementation
// will support is double-precision floats.
typedef double Value;

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

void initValueArray(ValueArray* array);

// Append an instruction to the array.
void writeValueArray(ValueArray* array, Value value);

// Free array memory once no longer needed.
void freeValueArray(ValueArray* array);

// Print the value of the given constant.
void printValue(Value value);

#endif