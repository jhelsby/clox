#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

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

// Add native function returning current time.
// This can be used as a template for adding further native functions.
static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
    // Reset by setting the stack top to the beginning
    // of the stack. No need to erase old values -
    // we can just overwrite them.
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
}

// Call runtime errors with informative error reporting,
// including the line number of the error.
static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // Print the stack trace, from top to bottom
    // (most recently called function to top-level code).
    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ",
                function->chunk.lines[instruction]);
        if (function->name == NULL) {
        fprintf(stderr, "script\n");
        } else {
        fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

// Helper function to simplify defining native functions.
static void defineNative(const char* name, NativeFn function) {
  // Temporarily store the name and function on the stack to prevent them from
  // being garbage collected before we store the function in a global variable.
  // This is needed since copyString() and newNative() allocate memory dynamically.
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));

  // Store the native function as a global variable.
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);

  // Remove the name and function from the stack.
  pop();
  pop();
}

void initVM() {
    resetStack();
    // Initially, no allocated objects.
    vm.objects = NULL;

    // Initialise hash tables for chunk global variables and strings.
    initTable(&vm.globals);
    initTable(&vm.strings);

    // Add native function returning current time.
    // This can be used as a template for adding further native functions.
    defineNative("clock", clockNative);
}

void freeVM() {
    freeTable(&vm.globals);
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

// Call a function with the specified number of arguments.
// Add its call frame to the stack.
static bool call(ObjFunction* function, int argCount) {
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->function = function;
  frame->ip = function->chunk.code;

  // Offset by 1 because parameters are 1-indexed.
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

// Try to call a class or function, wrapped in a Value, with the given number of arguments.
// If it's not a class or function, throw an error.
static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_FUNCTION:
        return call(AS_FUNCTION(callee), argCount);
      case OBJ_NATIVE: {
        // If we're calling a native function, just invoke the corresponding
        // C function immediately and push the result onto the stack.
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
      default:
        break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
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

    // Reuse the chars C-strxing array we heap allocated
    // to output the ObjString result. This avoids having
    // to make another copy and free chars.
    ObjString* result = takeString(chars, length);

    push(OBJ_VAL(result));
}

// Decode and execute each instruction in the VM.
static InterpretResult run() {
  // Get the current topmost frame.
  // We use this local variable both for readability,
  // and to encourage the compiler to store this pointer
  // in a register, potentially speeding up ip access.
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() \
    (frame->ip += 2, \
    (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() \
    (frame->function->chunk.constants.values[READ_BYTE()])

// Read a string operand. Note this is a one-byte value
// which gets the given string from the chunk's constant table.
#define READ_STRING() AS_STRING(READ_CONSTANT())

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
        disassembleInstruction(&frame->function->chunk,
            // Convert the instruction pointer into
            // a relative offset from the beginning
            // of the chunk.
            (int)(frame->ip - frame->function->chunk.code));
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

            // Pop the top value of the stack and forget it.
            case OP_POP: pop(); break;

            // Load a local variable from the stack slot
            // where the local lives (given in the operand).
            // Push it onto the stack for later instrucitons.
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }

            // Take a local variable's assigned value from
            // the top of the stack, and store it in the stack
            // slot corresponding to the local variable (given
            // in the operand). Note that we don't pop the stack.
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }

            // Load a global variable. The instruction's operand is the
            // index of the variable's name in the chunk's constant table.
            case OP_GET_GLOBAL: {
                // Read the operand string.
                ObjString* name = READ_STRING();
                Value value;

                // If the variable name isn't in the table, it was never
                // defined. This is a runtime error.
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                // Otherwise, we've just stored the variable's
                // value in value. Add it to the stack.
                push(value);
                break;
            }

            // Define a global variable. The instruction's operand
            // is the index of the variable's name in the chunk's constant
            // table. We get the variable's name from the constant
            // table, and store its (name, value) pair in the hash table.
            case OP_DEFINE_GLOBAL: {
                // Read the operand string.
                ObjString* name = READ_STRING();

                // Store (name, value) in the table.
                tableSet(&vm.globals, name, peek(0));

                // Now it's stored, remove it from the stack.
                pop();
                break;
            }

            // Assign a global variable.
            case OP_SET_GLOBAL: {
                // Read the operand string.
                ObjString* name = READ_STRING();

                // Store (name, value) in the table.
                // This returns true if a new entry was added.
                if (tableSet(&vm.globals, name, peek(0))) {

                    // If the string hasn't been defined as a global
                    // variable, assigning to it is a runtime error.

                    // Delete the variable - we just assigned it.
                    tableDelete(&vm.globals, name);

                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

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

            // Pop and print the value on the top of the stack.
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            // Created after we've executed an if-statement's "then" branch,
            // to jump over the "else" branch.
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            // If an if-condition is false, jump over its "then" block.
            case OP_JUMP_IF_FALSE: {
                // Read the 16-bit jump offset operand.
                uint16_t offset = READ_SHORT();
                // If the if-condition is false, apply the jump offset.
                // Otherwise, process the "then" block.
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            // Jump back to a given byte offset. For for- and while- loops.
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            // Execute a function call. The stack window contains the function
            // then the arguments it's been called with, in order.
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                // callValue() adds a new frame to the call stack,
                // for the called function. Retrieve this frame.
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_RETURN: {
                // Get the result of the value returned by the function and discard its CallFrame.
                Value result = pop();
                vm.frameCount--;

                // If we've just discarded the only remaining CallFrame,
                // we've finished executing the top-level code completely.
                // Pop the main script function from the stack and exit the interpreter.
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                // Otherwise, discard all the function's slots and push the
                // returned value back to the top of the stack.
                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    // Compile the function or top-level script
    // (which is implicitly stored inside a function).
    ObjFunction* function = compile(source);

    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    // Add the function to the value stack.
    push(OBJ_VAL(function));

    // Add the call frame to the call stack.
    call(function, 0);

    return run();
}