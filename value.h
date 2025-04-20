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

// Store all value types supported by the VM.
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

// Store a value representation as a tagged union.
// A variable can hold different types at different points in time.
// A tagged union allows us to handle this in a fairly memory-efficient way.
// Note this representation currently uses 16 bytes:
// 4 for type, 8 for the value, and 4 for padding.
// We'll improve on this later.
typedef struct {
    ValueType type;

    // Store the value of the variable here.
    union {
      bool boolean;
      double number;
    } as; // Calling it "as" looks nice when we
          // extract values from Value - see below.
} Value;

// Typecheck our Values to ensure type safety.
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)

// Convert a Value type into a C value.
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)

// Convert C values into the suitable Value type.
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);

// Append an instruction to the array.
void writeValueArray(ValueArray* array, Value value);

// Free array memory once no longer needed.
void freeValueArray(ValueArray* array);

// Print the value of the given constant.
void printValue(Value value);

#endif