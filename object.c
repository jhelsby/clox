
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
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
static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    // Whenever we create a new unique string,
    // add it to our string table for deduplication.
    tableSet(&vm.strings, string, NIL_VAL);

    return string;
}

// Implementation of the hash function FNV-1a.
static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

// Claim ownership of the input C-string (reusing its memory)
// and convert it to an ObjString.
ObjString* takeString(char* chars, int length) {
    // We hash the string on init.
    uint32_t hash = hashString(chars, length);

    // If we've allocated this string before, free it
    // (since the old owner doesn't need it anymore),
    // then give it to the new owner.
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

// Copy the given characters into a heap-allocated,
// string object. Null-terminate the string and return it.
ObjString* copyString(const char* chars, int length) {
    // We hash the string on init.
    uint32_t hash = hashString(chars, length);

    // If we've allocated memory for this
    // string before, return its memory address.
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned != NULL) return interned;

    // Otherwise, allocate a new string.

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
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
