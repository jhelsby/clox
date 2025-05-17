// Single-pass compiler. Parses source code using
// Pratt parser, producing parts of an AST as needed,
// and outputting bytecode as it goes.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

// Simple typedef. Hides C's function pointer type syntax.
// It needs to accept the canAssign function for variable
// assignment, even though no other ParseFns use it. For details, see:
// https://craftinginterpreters.com/global-variables.html#assignment
typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

// Store the name of a local variable,
// so we can find it when resolving
// identifiers with a matching lexeme.
typedef struct {
  Token name;
  // Record the scope depth of the local
  // variable's block, for faster resolution.
  int depth;
} Local;

// Store local variables, ordered in the
// array in the order of their declarations.
typedef struct {
  Local locals[UINT8_COUNT];
  // How many locals are in scope.
  int localCount;
  // How many blocks surround the section
  // of code we're currently compiling. This
  // allows us to easily discard all locals in a
  // given block. Scope depth zero is global scope.
  int scopeDepth;
} Compiler;

Parser parser;

// Global variable pointing to the compiler.
// This allows us to access the compiler from
// the frontend, for local variable resolution.
// A safer way to do this would be to create
// the compiler when we initialise the VM and
// pass it as an argument to all frontend functions.
Compiler* current = NULL;

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

// Initialise the compiler, for local variable resolution.
static void initCompiler(Compiler* compiler) {
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  current = compiler;
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

// Create a new scope by incrementing the
// compiler's current local variable depth.
static void beginScope() {
  current->scopeDepth++;
}

// End a scope by:
// - decrementing the compiler's current local variable depth.
// - popping them from the stack.
// - discarding them from our local variable list.
static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth >
            current->scopeDepth) {
    emitByte(OP_POP);
    current->localCount--;
  }
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

// Compare the identifier of two variables.
static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

// Resolve a local variable - given an identifier, find
// its corresponding local variable (if it exists).
// If multiple local variables with that name exist,
// we want the one declared in the deepest scope.
// If the local variable doesn't exist, return -1.
static int resolveLocal(Compiler* compiler, Token* name) {
  // Loop backwards over all local variables in our list,
  // to get the most deeply scoped local variable
  // whose name matches the given identifier.
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    // Retrieve the local variable we stored at index i.
    Local* local = &compiler->locals[i];

    // Get the first matching variable we find - this will
    // be the most deeply scoped.
    if (identifiersEqual(name, &local->name)) {
      // Forbid edge cases like:
      // {
      //    var a = "outer";
      //    {
      //       var a = a;
      //    }
      // }
      if (local->depth == -1) error("Can't read local variable in its own initializer.");

      return i;
    }
  }

  // Return -1 if there is no local variable with the given name.
  return -1;
}

// Add a new local variable to our list of locals.
// Store its existence and name.
static void addLocal(Token name) {
  // We store the index of our local variables with a
  // single-byte operand, which limits how many we can store.
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;

  // Set scope depth to -1 to indicate the variable is uninitialised.
  // Set it correctly once the variable's initializer has been compiled.
  local->depth = -1;
}

// For local variables, record the existence of the variable
// at compile-time. For global variables, do nothing.
static void declareVariable() {
  // Don't declare global variables - they are handled at runtime.
  if (current->scopeDepth == 0) return;

  // Extract the local variable's name.
  Token* name = &parser.previous;

  // Check all the other variables in the current scope,
  // to see if we've already declared one with this name.
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    // Check if we've exited the scope. A depth of -1
    // is used to indicate uninitialized local variables.
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  // Add it to our list of locals.
  addLocal(*name);
}

// Parse a variable, then:
// - For global variables, store it in the chunk's global constant
//   table. Return the index of the constant in the table.
// - For a local variable, declare it and exit.
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  // Declares a local variable; does nothing for globals.
  declareVariable();

  // Don't store local variables in the global constant table.
  if (current->scopeDepth > 0) return 0;

  // Store global variables in the constant table.
  return identifierConstant(&parser.previous);
}

// Mark the local variable currently being created
// as initialized, and set its depth.
static void markInitialized() {
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

// For global variables, outputs the "define variable" bytecode
// instruction and stores the initial value, using the constant
// table. For local variables, marks them as initialized.
static void defineVariable(uint8_t global) {

  if (current->scopeDepth > 0) {
    // Indicate the variable has been initialized,
    // and set its scope depth.
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

// Compile a binary expression, with an infix operator.
// canAssign is unused - required for assignment in variable().
static void binary(bool canAssign) {
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
// canAssign is unused - required for assignment in variable().
static void literal(bool canAssign) {
    switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
}

// Parse and compile parenthesised grouping expressions.
// canAssign is unused - required for assignment in variable().
static void grouping(bool canAssign) {
  // Assume the initial '(' has already been consumed.
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// Compile a number literal.
// canAssign is unused - required for assignment in variable().
static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

// Assuming a string token has hit, copy the lexeme providing
// the string's characters into a new string object.
// canAssign is unused - required for assignment in variable().
static void string(bool canAssign) {
  // The +1 and -2 parts are trimming the string's quotation marks.
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                  parser.previous.length - 2)));
}

// Compile a named variable. If it has already
// been assigned, retrieve its associated value.
// If we are reassigning it, and assignment is
// permitted, compile the assigned value and
// emit an assignment instruction.
static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;

  // Check if the variable is local. If it is, resolve it.
  int arg = resolveLocal(current, &name);

  // resolveLocal() returns -1 if the variable was global.
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    // Assignment.
    expression();
    emitBytes(setOp, (uint8_t)arg);
  } else {
    // Retrieval.
    emitBytes(getOp, (uint8_t)arg);
  }
}


// Compile a variable. Flag when it
// is permitted to assign it a value.
static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

// Compile a unary operator and its operand.
// canAssign is unused - required for assignment in variable().
static void unary(bool canAssign) {
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
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
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

  // Only allow assignment when parsing an assignment
  // expression or top-level expression.
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  // Check if the next token is an infix operator. If it is,
  // and precedence is low enough, keep parsing through until
  // we get to the end of the expression.
  // Otherwise, we've reached the end of this expression.
  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(canAssign);
  }

  // If we were trying to assign a variable
  // but weren't able to consume the = token,
  // there's been an error. Report it.
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
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

// Parse a block statement.
static void block() {
  // Parse declarations and statements until the end of the block ('}').
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
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
  // '{' initialises a new block, with its own scope.
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);

  Compiler compiler;
  initCompiler(&compiler);

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