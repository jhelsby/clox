// A collection of types and constants
// used throughout the clox interpreter.

#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Flag to use NaN boxing - a technique which reduce the amount of memory
// used by Lox's Value type, by storing details in uint-64 NaN mantissa bits:
// https://craftinginterpreters.com/optimization.html#what-is-and-is-not-a-number
#define NAN_BOXING

// Flag to dump a compiled chunk.
// #define DEBUG_PRINT_CODE

// Flag to add diagnostic logging to the VM.
// #define DEBUG_TRACE_EXECUTION

// "Stress test" mode for Lox's garbage collector.
// When defined, the GC runs as often as possible.
// Terrible for performance but useful for debugging the GC.
// #define DEBUG_STRESS_GC

// Flag to print logs every time the GC does something.
// #define DEBUG_LOG_GC

// The number of things we can encode in a byte.
#define UINT8_COUNT (UINT8_MAX + 1)

#endif