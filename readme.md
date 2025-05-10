# clox

This repository contains my implementation of the _clox_ interpreter described in the book [_Crafting Interpreters_](https://craftinginterpreters.com/) (2021), by Robert Nystrom. It is written in C and comprises of:

1. a bytecode compiler.
2. a virtual machine to execute the bytecode instructions, with garbage collection.

_clox_ interprets the programming language _Lox_, designed for the purposes of the book. Lox is a simple, dynamically typed, object-oriented language. Its built-in data types are booleans, numbers, strings, and nil. My interpreter is a work-in-progress, but I aim to complete the book's implementation as soon as possible. I am adding my own comprehensive comments throughout the codebase, for personal educational purposes.

My notes on _jlox_, the book's simpler and less efficient Java-based Lox interpreter, can be found [here](https://github.com/jhelsby/jlox-notes). They cover many of the high-level concepts used in _clox_.

## Setup and Run

To compile and run the WIP interpreter with GCC, run either of the following commands in the root repository directory:

```bash
gcc *.c && ./a.out # Launches the REPL.
```
or
```bash
gcc *.c && ./a.out someLoxFile.lox # Runs someLoxFile.lox.
```

## Implementation Notes

Here are some miscellaneous technical points about the _clox_ implementation I wanted to keep track of. However, please note that most of my implementation notes are in the form of code comments.

### Basic Structure

I will expand on this as my implementation develops.

* Our Lox code is converted into a series of [_chunks_](./chunk.h) - compact sequences of bytecode instructions by the compiler.

* The virtual machine deserialises these chunks and executes them.

#### Compiler

* Parses using a Pratt parser. Implemented in [`compiler.c`](./compiler.c).

#### VM

* Temporary variables are stored on a simple stack, implemented in [`vm.c`](./vm.c).

* All heap-allocated values are of type Obj, implemented in the [`object.c`](./object.c) module. We store all allocated Objs in a linked list in the VM, for simple garbage collection.

### Miscellaneous points

* Each type of dynamic array we use in _clox_ requires its own nearly-identical struct, init, write, and free functions. This isn't very elegant, but we don't need many of them. We could avoid repetition by simulating generics using preprocessor macros, but this would likely prove even more complicated.

* `OP_CONSTANT` uses only a single byte for its operand, limiting a chunk to 256 different constants. This could be addressed by making it use more bytes (which would make every constant instruction take up more space), or by adding a new `OP_CONSTANT_LONG` instruction for this special case.

* Our VM has a fixed stack size. We could grow this dynamically for more flexibility.

* Our hash table in [table.c](./table.c) only accepts string key values. We don't need to hash other types to implement clox.

* Our VM uses string interning - we don't store duplicate strings in memory. Instead, if you try to create a string which has already been allocated in memory, clox will return the allocated string's memory address. This is a trade-off which adds overhead to string creation in exchange for fast string equality testing. To see if two strings are equal, we can just compare their memory addresses - O(1) - instead of comparing each character between both strings - O(n).