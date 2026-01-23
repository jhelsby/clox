// Stores heap-allocated values in a single Obj type.
// Each Obj is a node in a linked list of all allocated Objs,
// for simple garbage collection.
//
// This implementation uses "struct inheritance" - a common
// technique whose term is coined by Nystrom - so Obj values
// can share common metadata fields but implement
// distinct memory requirements for different type tags
// (e.g. strings, functions, etc.).
// For more information, see:
// https://craftinginterpreters.com/strings.html#struct-inheritance

#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

// Extract an object type tag from a value.
#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

// Check if an object is of a certain object type.
// This allows for safe casting between Obj and the ObjType.
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

// Retrieve a value as an ObjFunction.
#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))

// Retrieve a value as an ObjNative.
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)

// Retrieve a value as an ObjString.
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))

// Retrieve a value (assumed to be a string) as a character array.
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

// Different Obj types have distinct memory requirements,
// so must be treated differently.
typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
} ObjType;

// A "base class" for objects, containing the state shared
// across all object types. This is necessary for our
// garbage collection implementation.
struct Obj {
    ObjType type;

    // Whether the given object has been marked as
    // reachable, and thus ineligible for GC cleanup.
    bool isMarked;

    // For our GC to track all allocated objects,
    // each Obj is a node in a linked list of Objs.
    struct Obj* next;
};

// Contains all data and metadata needed to
// describe an entire function and its bytecode.
typedef struct {
  Obj obj;
  int arity;
  // Track the number of upvalues - when a variable lies in the local scope of an enclosing function.
  int upvalueCount;
  // A dedicated chunk of bytecode containing instructions to execute the function.
  Chunk chunk;
  ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

// Used to create native functions - Lox functions which
// call native C code. These allow us to access OS operations,
// e.g. reading user input and accessing the file system.
typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

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

    // Store the string characters.
    char* chars;

    // Cache the hash of the string on initialisation.
    uint32_t hash;
};

// Create an upvalue - when a variable lies in the local scope of an enclosing function.
typedef struct ObjUpvalue {
  Obj obj;
  // A reference to the variable associated with the upvalue.
  // This allows us to assign to the variable itself and not a copy.
  Value* location;
  Value closed;
  // We use a linked list to join our upvalues so we can efficiently sort
  // them by stack slot order. For further details see:
  // https://craftinginterpreters.com/closures.html#tracking-open-upvalues
  struct ObjUpvalue* next;
} ObjUpvalue;

// Used to create closures, consisting of a function and any captured local variables.
typedef struct {
  Obj obj;
  ObjFunction* function;

  // Use a double pointer to upvalues since:
  // 1. different closures have different numbers of upvalues, so we need a dynamic array of upvalues.
  // 2. upvalues are dynamically allocated too.
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;

// Create a Lox function.
ObjFunction* newFunction();

// Create a closure, consisting of a function and any captured local variables.
ObjClosure* newClosure(ObjFunction* function);

// Create a Lox native function - a Lox function
// which can call native C code, enabling e.g. syscalls.
ObjNative* newNative(NativeFn function);

// Claim ownership of the input C-string (reusing its memory)
// and convert it to an ObjString.
ObjString* takeString(char* chars, int length);

// Copy the given characters into a heap-allocated,
// string object. Null-terminate the string and return it.
ObjString* copyString(const char* chars, int length);

// Create an upvalue. Input is the address of the slot where the closed-over variable is stored.
ObjUpvalue* newUpvalue(Value* slot);

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