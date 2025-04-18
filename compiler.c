// Single-pass compiler. Parses source code,
// producing parts of an AST as needed,
// and outputting bytecode as it goes.

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct {
  Token current;
  Token previous;
  bool hadError;

  // Flag to avoid error cascades.
  // Tracks whether errors have occurred,
  // so we only report the first error.
  bool panicMode;
} Parser;

Parser parser;
Parser parser;

// Store the chunk currently being compiled.
// Gets set at the beginning of compile().
Chunk* compilingChunk;

static Chunk* currentChunk() {
  return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
  // Only report the first error we find.
  // Avoids reporting error cascades.
  if (parser.panicMode) return;
  parser.panicMode = true;

  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    // Show the lexeme (if human-readable).
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

// Report an error at the current token.
static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

// Advance through the token stream,
// reporting errors as they are found.
// Only pass valid tokens to the
// rest of the parser.
static void advance() {
  // Store the old token, so we can retrieve
  // its lexeme when matching a token.
  parser.previous = parser.current;

  // Only break the loop when
  // non-error tokens are found.
  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) break;

    // Report any errors.
    errorAtCurrent(parser.current.start);
  }
}

// Advance through the token stream if
// the token matches an expected type.
// Otherwise, report an error.
// Allows us to identify syntax errors.
static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }

  errorAtCurrent(message);
}

// Append a byte to the chunk.
static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

// Append two bytes to a chunk, for when we write
// an opcode followed by a one-byte operand.
static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

// Append a return instruction to the chunk.
static void emitReturn() {
  emitByte(OP_RETURN);
}

// Add a value to the chunk's constant table. Return
// an error if there's insufficient space in the chunk.
static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  // We use a single byte for the index operand,
  // meaning only <= 256 constants per chunk.
  // We could adapt this to use two or more bytes.
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
  }

  return (uint8_t)constant;
}

// Append a constant to the chunk.
static void emitConstant(Value value) {
  emitBytes(OP_CONSTANT, makeConstant(value));
}

// Completes a compiled chunk by adding
// a return instruction at the end.
static void endCompiler() {
  emitReturn();
}

// Parse and compile parenthesised grouping expressions.
static void grouping() {
  // Assume the initial '(' has already been consumed.
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// Compile a number literal.
static void number() {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(value);
}

// Compile a unary operator and its operand.
static void unary() {
  // Assume the unary operator token has already been parsed.
  TokenType operatorType = parser.previous.type;

  // Compile the operand.
  expression();

  // Emit the operator instruction.
  // Note that we add this after the operand,
  // because we want to evaluate the operand first,
  // pop it from the stack, apply the
  // unary operator, then push the result.
  switch (operatorType) {
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; // Unreachable.
  }
}

static void expression() {
  // Will compile Lox expressions.
}

void compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");
  endCompiler();

  // Indicate whether the compilation was successful.
  return !parser.hadError;
}