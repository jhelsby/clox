// A collection of types and constants
// used throughout the clox interpreter.

#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Flag to dump a compiled chunk.
// #define DEBUG_PRINT_CODE

// Flag to add diagnostic logging to the VM.
// #define DEBUG_TRACE_EXECUTION

// The number of things we can encode in a byte.
#define UINT8_COUNT (UINT8_MAX + 1)

#endif