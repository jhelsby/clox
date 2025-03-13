# clox

This repository contains my implementation of the _clox_ interpreter described in the book [_Crafting Interpreters_](https://craftinginterpreters.com/) (2021), by Robert Nystrom. _clox_ interprets the programming language _Lox_, designed for the purposes of the book. Lox is a simple, dynamically typed, object-oriented language. Its built-in data types are booleans, numbers, strings, and nil.

My interpreter is a work-in-progress, but I aim to complete the book's implementation as soon as possible. I also intend to add my own comprehensive comments throughout the codebase, for personal educational purposes. My notes on _jlox_, the book's simpler and less efficient Java-based Lox interpreter, can be found [here](https://github.com/jhelsby/jlox-notes). They cover many of the high-level concepts used in _clox_.

### Setup

To compile the WIP interpreter with GCC, run the following in the root repository directory:

```bash
gcc *.c         # To call the output binary "a.out".
gcc -o clox *.c # To call the output binary "clox".
```

You can then run the binary with `./a.out` or `./clox` (depending what you just called it!).
