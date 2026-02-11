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

#include <string.h>

// Stores heap-allocated value representations.
// Defined in object.h, but declared here to avoid
// cyclic dependencies between values and objects.
typedef struct Obj Obj;
// All object types.
typedef struct ObjString ObjString;

// If NaN boxing, store all details about Lox's Value type in uint-64 NaN mantissa bits - quiet NaN representations:
// https://craftinginterpreters.com/optimization.html#what-is-and-is-not-a-number
#ifdef NAN_BOXING

// Used in our NaNs to indicate an object pointer.
#define SIGN_BIT ((uint64_t)0x8000000000000000)

// Equivalent to 0111...111000....0 where the ones are the quiet NaN bits.
#define QNAN     ((uint64_t)0x7ffc000000000000)

// Use the two lowest bits of our unused mantissa space as a
// "type tag" to determine which of these values we're looking at.
#define TAG_NIL   1 // 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;

// Tricky logic but needed because something like
// ((v) == TRUE_VAL || (v) == FALSE_VAL) would cause
// v's side effects to be executed twice, if it has any.
// Three possibilities from (value) | 1:
// 1. Converts a FALSE_VAL to TRUE_VAL.
// 2. Does nothing to a TRUE_VAL.
// 3. Is some non-Boolean value, so == TRUE_VAL returns False.
#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)

#define IS_NIL(value)       ((value) == NIL_VAL)

// Every Lox Value that is not a number will use a quiet NaN representation.
// If all its NaN bits are set, plus the quiet NaN bit, it's not a number.
// Otherwise, it is a number.
// (Almost certainly, unless the CPU architecture does something unexpected.)
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)

// An object must be a NaN with the sign bit set.
#define IS_OBJ(value)       (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))



#define AS_BOOL(value)      ((value) == TRUE_VAL)

// Type punning: convert a C double to a uint64_t clox Value and vice versa.

#define AS_NUMBER(value)    valueToNum(value)

// Clear all quiet NaN and sign bits, leaving only the pointer bits.
#define AS_OBJ(value)       ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

static inline double valueToNum(Value value) {
  double num;
  // Copying looks like an inefficient way to convert a uint64_t to a double,
  // but this is a supported idiom for type punning, so most compilers
  // optimize the memcpy() away.
  memcpy(&num, &value, sizeof(Value));
  return num;
}

// A Lox boolean is one with either the true or false values.
#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)

// False/true/nil values have all the NaN bits set, plus the false/true/nil bits.
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))

#define NUMBER_VAL(num) numToValue(num)

// If the sign bit is set on a NaN, then the remaining low bits store the pointer to a Lox Object.
// Note:
// - Technically a pointer is 64 bits and could hence overlap with
// some of those NaN and sign bits. In practice, Nystrom says
// everything above the 48th bit in a pointer is always zero.
// - All the casting is to satisfy the pickiest C compilers.
//
// Because of this, this optimisation is dubious - it works in practice but not according to the spec.
#define OBJ_VAL(obj)    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline Value numToValue(double num) {
  Value value;
  // Copying looks like an inefficient way to convert a double to a uint64_t,
  // but this is a supported idiom for type punning, so most compilers
  // optimize the memcpy() away.
  memcpy(&value, &num, sizeof(double));
  return value;
}

#else

// Store all value types supported by the VM.
typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    // Store heap-allocated types.
    VAL_OBJ
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
        // For heap-allocated types, the
        // payload is a pointer to heap memory.
        Obj* obj;
    } as; // Calling it "as" looks nice when we
          // extract values from Value - see below.
} Value;

// Typecheck our Values to ensure type safety.
#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

// Convert a Value type into a C value.
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define AS_OBJ(value)     ((value).as.obj)

// Convert C values into the suitable Value type.
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#endif // If not NaN-boxing, store Lox Value details in simple C structs.

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