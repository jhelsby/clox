// Single-pass compiler. Parses source code using
// Pratt parser, producing parts of an AST as needed,
// and outputting bytecode as it goes.

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;

  // Flag to avoid error cascades.
  // Tracks whether errors have occurred,
  // so we only report the first error.
  bool panicMode;
} Parser;

typedef enum {
  // Precedence levels from lowest to highest.
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

// Simple typedef for a function type with no arguments and
// returns nothing. Hides C's function pointer type syntax.
typedef void (*ParseFn)();

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

Parser parser;

// Store the chunk currently being compiled.
// Gets set at the beginning of compile().
Chunk* compilingChunk;

static Chunk* currentChunk() {
  return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
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

// Check if the current token has the given type.
static bool check(TokenType type) {
  return parser.current.type == type;
}

// If the current token has the given type,
// consume the token and return true.
// Otherwise, return false.
static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
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

#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

// Declare these here so we can:
// - reference them in our rules table below, then
//   define them AFTER the table.
// - use them recursively.

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

// Add the given token's lexeme to the chunk's
// constant table as a string, and return the
// index of the constant in the table. We use
// the index as it is easier to include
// in the bytecode stream on variable lookup.
static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start,
                                         name->length)));
}

// Parse a variable and store it in the chunk's constant
// table. Return the index of the constant in the table.
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  return identifierConstant(&parser.previous);
}

// Output's the "define variable" bytecode instruction
// and stores the initial value, using the constant table.
static void defineVariable(uint8_t global) {
  emitBytes(OP_DEFINE_GLOBAL, global);
}

// Compile a binary expression, with an infix operator.
static void binary() {
  // Assume the left-hand operand expression has already been
  // compiled, and the subsequent infix operator consumed.
  TokenType operatorType = parser.previous.type;

  // Binary operators are left-associative, so their
  // right-hand operand precedence is one level higher
  // than its own.
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));

  switch (operatorType) {
    // For comparison operators, we use two instructions for
    // !=, <=, and >= - mostly for didactic purposes:
    // it illustrates that bytecode can deviate from user source code,
    // provided the user-visible behaviour is the same.
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;

    // Arithmetic operations are one instruction.
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    default: return; // Unreachable.
  }
}

// Compile Boolean and nil literals to bytecode.
static void literal() {
  switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
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
  emitConstant(NUMBER_VAL(value));
}

// Assuming a string token has hit, copy the lexeme providing
// the string's characters into a new string object.
static void string() {
  // The +1 and -2 parts are trimming the string's quotation marks.
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                  parser.previous.length - 2)));
}

// Compile a unary operator and its operand.
static void unary() {
  // Assume the unary operator token has already been parsed.
  TokenType operatorType = parser.previous.type;

  // Compile the operand. Note parsing at unary
  // precedence allows nested unary expressions.
  parsePrecedence(PREC_UNARY);

  // Emit the operator instruction.
  // Note that we add this after the operand,
  // because we want to evaluate the operand first,
  // pop it from the stack, apply the
  // unary operator, then push the result.
  switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; // Unreachable.
  }
}

// All our parsing rules. The RHS is a ParseRule
// enum {prefix, infix, precedence}. A prefix/infix of
// NULL means there is no expression using that token
// as a prefix/infix operator.
// NOTE: this is a WIP and will be filled out as I
// implement more of clox.
ParseRule rules[] = {
  // Note that tokens are listed in the same order
  // as the TokenType enum, for easy lookup.
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

// Starting from the current token, parse any
// expression >= the given precedence level.
static void parsePrecedence(Precedence precedence) {
  advance();

  // The first token in an expression is always part of a prefix expression.
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }
  prefixRule();

  // Check if the next token is an infix operator. If it is,
  // and precedence is low enough, keep parsing through until
  // we get to the end of the expression.
  // Otherwise, we've reached the end of this expression.
  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule();
  }
}

// Simple getter retrieving the parse rule for
// the given token type.
static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

// Compiles the next Lox expression in the chunk.
static void expression() {
  // Parsing the lowest precedence level
  // will parse all expressions.
  parsePrecedence(PREC_ASSIGNMENT);
}

// Parse a variable declaration.
// For now, we only handle global variables.
static void varDeclaration() {
  // Extract the variable name and add it to the chunk's
  // constant table as a string. global will be the
  // index of the constant in the constant table.
  uint8_t global = parseVariable("Expect variable name.");

  // If we have var a = someExpr, parse someExpr.
  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    // Treat var a as var a = nil
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON,
          "Expect ';' after variable declaration.");

  // Store the variable's value.
  defineVariable(global);
}

// Parse an expression statement - an expression
// followed by a semicolon. It evalautes the
// expression and discards the result.
static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

// Evaluate and print an expression.
static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

// Hitting a compile error puts the compiler into panic mode.
// We want to exit this when we reach a synchronisation point,
// so we can continue parsing the rest of the source file
// and identify any further errors. Lox uses statement
// boundaries as its synchronisation points.
static void synchronize() {
  parser.panicMode = false;

  // Skip tokens until we hit a statement boundary.
  while (parser.current.type != TOKEN_EOF) {
    // A semicolon indicates the end of a statement.
    if (parser.previous.type == TOKEN_SEMICOLON) return;

    // These tokens indicate the beginning of a statement.
    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;

      default:
        ; // Do nothing.
    }

    advance();
  }
}

// Will eventually parse all declarations:
// classDecl, funDecl, varDecl, and statement.
static void declaration() {
  if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  // If the last statement gave a compile error, we
  // will now be in panic mode. Exit this once
  // we've reached a synchronisation point.
  if (parser.panicMode) synchronize();
}

// Will eventually parse all statements:
// exprStmt, forStmt, ifStmt, printStmt,
// returnStmt, whileStmt, and block.
static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else {
    expressionStatement();
  }
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  // Keep compiling declarations until
  // we hit the end of the source file.
  while (!match(TOKEN_EOF)) {
    declaration();
  }

  endCompiler();

  // Indicate whether the compilation was successful.
  return !parser.hadError;
}