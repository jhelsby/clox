
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

// Allocate an object of the given size on the heap.
#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    // Each time we allocate an Obj, we insert it at the
    // head of our VM's linked list of allocated objects.
    object->next = vm.objects;
    vm.objects = object;
    return object;
    }

// Create a new ObjString on the heap, and initialise it.
static ObjString* allocateString(char* chars, int length) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    return string;
}

// Claim ownership of the input C-string (reusing its memory)
// and convert it to an ObjString.
ObjString* takeString(char* chars, int length) {
    return allocateString(chars, length);
}

// Copy the given characters into a heap-allocated,
// string object. Null-terminate the string and return it.
ObjString* copyString(const char* chars, int length) {
  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
  return allocateString(heapChars, length);
}

// Prints a string representation of an object.
void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            // Print the characters in the string.
            printf("%s", AS_CSTRING(value));
            break;
        // Will add more cases.
    }
}
