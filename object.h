// Stores heap-allocated values in a single Obj type.
// Uses "struct inheritance" - a common technique whose
// term is coined by Nystrom - to ensure Obj values with
// different type tags (e.g. strings, functions, etc.) and
// distinct memory requirements can be handled correctly.
// For more information, see:
// https://craftinginterpreters.com/strings.html#struct-inheritance

#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

// Extract an object type tag from a value.
#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

// Check if an object is of a certain object type.
// This allows for safe casting between Obj and the ObjType.
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

// Retrieve a value as an ObjString.
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))

// Retrieve a value (assumed to be a string) as a character array.
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

// Different Obj types have distinct memory requirements,
// so must be treated differently.
typedef enum {
    OBJ_STRING,
    // More ObjTypes will be added later.
} ObjType;

// A "base class" for objects, containing the state shared
// across all object types. This is necessary for our
// garbage collection implementation.
struct Obj {
    ObjType type;
};

// Standard implementation. A string object contains
// a heap-allocated array of characters.
struct ObjString {
    // C specifies that struct fields are arranged in
    // memory in the order they are declared. By having
    // an Obj be the first field, the memory of an Obj aligns
    // with the Obj bytes for ObjString.
    // This allows us to cast pointers back and forth
    // between them.
    Obj obj;
    // Length isn't strictly necessary, but is convenient.
    int length;
    char* chars;
};

// Claim ownership of the input C-string (reusing its memory)
// and convert it to an ObjString.
ObjString* takeString(char* chars, int length);

// Copy the given characters into a heap-allocated,
// string object. Null-terminate the string and return it.
ObjString* copyString(const char* chars, int length);

// Prints a string representation of an object.
void printObject(Value value);

// Used for the OBJ_TYPE macro. We set this as a separate function
// because its body uses value twice. If value were
// an expression, the macro would evaluate that expression twice.
// Not only is this inefficient, but the expression could
// have side-effects. For example,
// IS_STRING(POP()) would pop two values off the stack.
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif