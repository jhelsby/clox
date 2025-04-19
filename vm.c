#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
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

            // Binary arithmetic operators.
            case OP_ADD:      BINARY_OP(NUMBER_VAL, +); break;
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