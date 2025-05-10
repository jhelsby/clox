#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

// Use a global variable (!?) to access the VM.
// We only need one VM here, and a global variable
// simplifies the implementation. Probably not a
// good idea for larger systems!
VM vm;

static void resetStack() {
    // Reset by setting the stack top to the beginning
    // of the stack. No need to erase old values -
    // we can just overwrite them.
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
    // Initially, no allocated objects.
    vm.objects = NULL;

    // No stored strings on VM init.
    initTable(&vm.strings);
}

// Call runtime errors with informative error reporting,
// including the line number of the error.
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

void freeVM() {
    freeTable(&vm.strings);

    // Free all allocated objects when a program terminates.
    freeObjects();
}

// Push onto the value stack.
void push(Value value) {
    // Add the value to the top of the stack,
    // which stackTop points to.
    *vm.stackTop = value;

    vm.stackTop++;
}

// Pop from the value stack.
Value pop() {
    vm.stackTop--;

    return *vm.stackTop;
}

// Retrieve the top of the stack.
static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

// In Lox, nil and false are falsey and everything else is truthy.
static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

// Concatenate two strings, assuming the top two frames
// on the stack are ObjStrings.
static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    // Reuse the chars C-string array we heap allocated
    // to output the ObjString result. This avoids having
    // to make another copy and free chars.
    ObjString* result = takeString(chars, length);

    push(OBJ_VAL(result));
}

// Decode and execute each instruction in the VM.
static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)

#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// This macro simplifies binary operations,
// especially for type checking.
//
// The do-while block ensures the statements are the
// same scope when macro is expanded. For details, see:
// https://craftinginterpreters.com/a-virtual-machine.html#binary-operators
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)

    for (;;) {

// If active, handle VM's diagnostic logging.
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");

        // Print stack trace - every stack value in order.
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");

        // Print the instruction being executed,
        // with its chunk offset.
        disassembleInstruction(vm.chunk,
            // Convert the instruction pointer into
            // a relative offset from the beginning
            // of the chunk.
            (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();

                // Add the constant to the value stack.
                push(constant);

                break;
            }

            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;

            // Equal can be evaluated on any pair of objects.
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
              }

            // Binary operators.
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            // Concatenate two strings, or add two numbers.
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError(
                      "Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_NOT:
            push(BOOL_VAL(isFalsey(pop())));
            break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;

            // If the top of the stack is a number, negate it.
            case OP_NEGATE:
            if (!IS_NUMBER(peek(0))) {
              runtimeError("Operand must be a number.");
              return INTERPRET_RUNTIME_ERROR;
            }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;

            case OP_RETURN: {

                // Temporary, for testing.
                // Returning a function will pop the stack
                // and print the first value before exiting.
                printValue(pop());
                printf("\n");

                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    // If we can't compile the source code,
    // discard the chunk and report an error.
    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    // Otherwise, compile() compiles source and
    // fills the chunk with the resulting bytecode.

    // Load the chunk into the VM.
    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    // Execute the chunk.
    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}