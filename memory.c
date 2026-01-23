#include <stdlib.h>

#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

// Handles all dynamic memory management in clox.
// Uses a void* pointer so we can reallocate raw memory.
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  // If the "stress test" debug flag is set, run Lox's GC whenever we allocate new memory.
  // Terrible for performance but useful for debugging the GC.
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
      collectGarbage();
#endif
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    // realloc() allocates new memory if none is
    // allocated, or resizes memory if the allocated
    // size is changing.
    //
    // When growing memory, it allocates a new block,
    // copies over the old bytes, frees the old block,
    // and returns a pointer to the new block -
    // just as required when growing our dynamic array.
    void* result = realloc(pointer, newSize);

    // Handle "running out of memory" error case.
    if (result == NULL) exit(1);

    return result;
}

// Free an allocated object, and all memory associated with it.
static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
      case OBJ_CLOSURE: {
        // Free the closure and all its upvalues, but not the associated function.
        // There may be multiple closures that all reference the same function,
        // and we can only free the function once all of those closures are gone.
        // The GC will handle freeing the function.
        ObjClosure* closure = (ObjClosure*)object;
        FREE_ARRAY(ObjUpvalue*, closure->upvalues,
                  closure->upvalueCount);
        FREE(ObjClosure, object);
        break;
      }
      case OBJ_FUNCTION: {
        // Free the ObjFunction and any other memory it owns.
        ObjFunction* function = (ObjFunction*)object;
        // Functions own their chunk.
        freeChunk(&function->chunk);
        FREE(ObjFunction, object);
        break;
      }
      case OBJ_NATIVE: {
        FREE(ObjNative, object);
        break;
      }
      case OBJ_STRING: {
        // Free the string's Obj and its character array.
        ObjString* string = (ObjString*)object;
        FREE_ARRAY(char, string->chars, string->length + 1);
        FREE(ObjString, object);
        break;
      }
      case OBJ_UPVALUE:
        // Free an upvalue, but not the variable it references.
        // This is because multiple closures can close over the same variable.
        FREE(ObjUpvalue, object);
        break;
    }
}

// Call Lox's tracing garbage collector to clean up unreachable objects.
void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
#endif

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
#endif
}

// Free all allocated objects, by traversing
// the VM's linked list of objects.
void freeObjects() {
    Obj* object = vm.objects;
    while (object != NULL) {
      Obj* next = object->next;
      freeObject(object);
      object = next;
    }
  }
