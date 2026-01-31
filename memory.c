#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

// The threshold is a multiple of the heap size.
// The more live memory,
// the less often we trigger the GC, to avoid scanning live
// objects unnecessarily.
#define GC_HEAP_GROW_FACTOR 2

// Handles all dynamic memory management in clox.
// Uses a void* pointer so we can reallocate raw memory.
void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  // Track live memory.
  vm.bytesAllocated += newSize - oldSize;

  // If the "stress test" debug flag is set, run Lox's GC whenever we allocate new memory.
  // Terrible for performance but useful for debugging the GC.
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
      collectGarbage();
#endif
    }

    // Trigger the GC once live memory exceeds a threshold.
    if (vm.bytesAllocated > vm.nextGC) {
      collectGarbage();
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

// Mark a Lox object as reachable for the GC.
// Use the tricolor abstraction. All objects have a colour:
// - White: not reached or processed.
// - Gray: reachable, but we haven't checked the objects
//         it references to see if _they_ are reachable.
// - Black: reachable and we've marked the objects it references.
//
// Everything starts white. Once we've done a full traversal,
// everything will be either white or black. We clean up white
// objects.
//
// Our algorithm satisfies the tricolor invariant:
// no black node ever points to a white node.
// This prevents reachable objects from being freed.
void markObject(Obj* object) {
  if (object == NULL) return;

  // Prevent infinite loops caused by cycles of object references.
  if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  object->isMarked = true;

  // Store all gray objects to a worklist stack for easy access.
  // We will process each object in turn.
  if (vm.grayCapacity < vm.grayCount + 1) {
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    // Use C's realloc instead of reallocate to prevent
    // this GC operation from being managed by the GC,
    // which could cause some recursive bugs.
    vm.grayStack = (Obj**)realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
  }

  vm.grayStack[vm.grayCount++] = object;

  // Since we're using C's realloc we need to handle the case
  // where allocating the gray stack fails. For the sake of
  // simplicity here we just abort, since the gray stack
  // is normally small, so such failures should be rare.
  // However, something more elegant would be better.
  if (vm.grayStack == NULL) exit(1);
}

// Mark a Lox value as reachable for the GC.
void markValue(Value value) {
  if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

// Mark all the constants in the function's constant table as reachable.
static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}

// Traverse an object and mark the objects it references as reachable.
// In the tricolor abstraction, this is equivalent to marking an object as black.
static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  switch (object->type) {
    case OBJ_BOUND_METHOD: {
      // Ensure a handle to a method keeps the receiver in memory,
      // so that 'this' can find the object when you invoke the handle later.
      ObjBoundMethod* bound = (ObjBoundMethod*)object;
      markValue(bound->receiver);
      markObject((Obj*)bound->method);
      break;
    }
    case OBJ_CLASS: {
      ObjClass* klass = (ObjClass*)object;
      markObject((Obj*)klass->name);
      markTable(&klass->methods);
      break;
    }
    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      // Each closure references the function it wraps.
      markObject((Obj*)closure->function);
      // It also references an array of pointers to the upvalues it captures.
      for (int i = 0; i < closure->upvalueCount; i++) {
        markObject((Obj*)closure->upvalues[i]);
      }
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      // Each function has a reference to an ObjString containing the function's name.
      markObject((Obj*)function->name);
      // Mark all the constants in the function's constant table as reachable.
      markArray(&function->chunk.constants);
      break;
    }
    case OBJ_INSTANCE: {
      ObjInstance* instance = (ObjInstance*)object;
      markObject((Obj*)instance->klass);
      markTable(&instance->fields);
      break;
    }
    // Closed upvalues contain a reference to the closed-over value.
    case OBJ_UPVALUE:
      markValue(((ObjUpvalue*)object)->closed);
      break;
    // Strings and native function objects contain no outgoing
    // references, so there's nothing to traverse.
    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
  }
}

// Free an allocated object, and all memory associated with it.
static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
      case OBJ_BOUND_METHOD:
        FREE(ObjBoundMethod, object);
        break;
      case OBJ_CLASS: {
        ObjClass* klass = (ObjClass*)object;
        freeTable(&klass->methods);
        FREE(ObjClass, object);
        break;
      }
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
      case OBJ_INSTANCE: {
        ObjInstance* instance = (ObjInstance*)object;
        freeTable(&instance->fields);
        FREE(ObjInstance, object);
        break;
      }
    }
}

// Mark all objects that are immediately reachable via the stack -
// variables or temporaries.
static void markRoots() {
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }

  // Mark objects contained in the VM's call frames.
  // These store pointers to closures being called,
  // and we must mark any constants and upvalues
  // in these closures as reachable.
  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }

  // Mark the open upvalue list as reachable.
  for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
    markObject((Obj*)upvalue);
  }

  // Mark global variables, which live in a hash table owned by the VM.
  markTable(&vm.globals);

  // Mark any values directly accessed by the compiler during compilation as reachable.
  markCompilerRoots();

  // Our 'init' method string is special, so should always be a root.
  markObject((Obj*)vm.initString);
}

// Traverse all gray objects to find all reachable objects.
static void traceReferences() {
  while (vm.grayCount > 0) {
    // Pop the stack.
    Obj* object = vm.grayStack[--vm.grayCount];

    // Trace all of object's references and mark it as black.
    blackenObject(object);
  }
}

// Check every object in the heap by walking the linked list.
// Remove any unmarked (i.e. unreachable) objects from the list and free their memory.
// Reset any marked objects to unmarked for the next GC cycle.
static void sweep() {
  Obj* previous = NULL;
  Obj* object = vm.objects;
  while (object != NULL) {
    if (object->isMarked) {
      // Reset any marked objects to unmarked for the next GC cycle.
      object->isMarked = false;

      previous = object;
      object = object->next;
    } else {
      Obj* unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm.objects = object;
      }

      freeObject(unreached);
    }
  }
}

// Run a tracing garbage collection algorithm to clean up unreachable objects.
void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
  size_t before = vm.bytesAllocated;
#endif

  // Mark all objects that are immediately reachable via the stack.
  markRoots();

  // Traverse the roots to find all reachable objects.
  traceReferences();

  // Clean up the string pool, which needs special treatment because
  // the string hash table always points to all the strings, so they are
  // technically all reachable even if they aren't being used.
  tableRemoveWhite(&vm.strings);

  // Clean up all unreachable objects.
  sweep();

  // Sweeping reduces live memory, so recalculate our GC trigger threshold.
  vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
  printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
         before - vm.bytesAllocated, before, vm.bytesAllocated,
         vm.nextGC);
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

    free(vm.grayStack);
  }
