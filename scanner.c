#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  const char* start;
  const char* current;
  int line;
} Scanner;

Scanner scanner;

// We store all our token characters in the source string.
// To retrieve a given token, we point to its start and
// give its length. This simplifies memory management.
void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
}

static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}

static bool isAtEnd() {
  return *scanner.current == '\0';
}

// Consume and return the current
// source string character.
static char advance() {
  scanner.current++;
  return scanner.current[-1];
}

// Return the current source
// string character.
static char peek() {
  return *scanner.current;
}

// Return the next source
// string character.
static char peekNext() {
  if (isAtEnd()) return '\0';
  return scanner.current[1];
}

// Conditionally consume a character.
// Consume if it matches the input character.
// Used to parse multi-character tokens.
static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scanner.current != expected) return false;
  scanner.current++;
  return true;
}

// Capture the current token using the
// scanner's start and current pointers.
static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;
  return token;
}

// Returns an error token using the
// scanner's current line number.
static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;
  return token;
}

// Advance the scanner to the next
// meaningful (non-whitespace) character.
static void skipWhitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;
      case '\n':
        scanner.line++;
        advance();
        break;
      // Handle comments, which Lox treats like whitespace.
      case '/':
        if (peekNext() == '/') {
          // A comment goes until the end of the line.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          return;
        }
        break;
      default:
        return;
    }
  }
}

// Helper function for identifier().
// Once we have scanned a keyword, match it
// to its type (e.g. if, var, class, etc.).
static TokenType identifierType() {

  // I'll expand this function soon.
  return TOKEN_IDENTIFIER;
}

// Extract an identifier token from the source string.
static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek())) advance();
  return makeToken(identifierType());
}

// Extract a number token from the source string.
static Token number() {
  while (isDigit(peek())) advance();

  // Look for a fractional part.
  if (peek() == '.' && isDigit(peekNext())) {
    // Consume the ".".
    advance();

    while (isDigit(peek())) advance();
  }

  // scanner.start is the start of the number,
  // and scanner.current is the end. We just
  // store the lexeme, converting it to a
  // runtime double value later and elsewhere.
  return makeToken(TOKEN_NUMBER);
}

// Extract a string token from the source string.
static Token string() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') scanner.line++;
    advance();
  }

  if (isAtEnd()) return errorToken("Unterminated string.");

  // The closing quote.
  advance();

  // scanner.start is the start of the string,
  // and scanner.current is the end. We just
  // store the lexeme, converting it to a
  // runtime string value later and elsewhere.
  return makeToken(TOKEN_STRING);
}

// Scan a single token from the scanner's
// current position in the source string.
Token scanToken() {
  skipWhitespace();
  scanner.start = scanner.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  char c = advance();

  if (isAlpha(c)) return identifier();
  if (isDigit(c)) return number();

  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);

    // Handle multicharacter tokens.
    case '!':
      return makeToken(
          match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=':
      return makeToken(
          match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '<':
      return makeToken(
          match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '>':
      return makeToken(
          match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
    case '"': return string();
  }

  return errorToken("Unexpected character.");
}