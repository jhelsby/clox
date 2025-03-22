#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

void compile(const char* source) {
  initScanner(source);

  // Temporary testing code to scan through
  // Lox code. Ask the scanner for tokens
  // until an EOF character. Print these tokens.
  int line = -1;
  for (;;) {
    Token token = scanToken();
    // If the token is on a new line of code,
    // print the line number.
    if (token.line != line) {
      printf("%4d ", token.line);
      line = token.line;
    } else {
      printf("   | ");
    }

    // Print the raw index of the token type, and the token itself.
    printf("%2d '%.*s'\n", token.type, token.length, token.start);

    if (token.type == TOKEN_EOF) break;
  }
}