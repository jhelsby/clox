#ifndef clox_compiler_h
#define clox_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction* compile(const char* source);

// Mark any values directly accessed by the compiler
// during compilation as reachable, for the GC.
void markCompilerRoots();

#endif