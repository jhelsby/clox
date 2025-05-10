// Standard hash table implementation using
// hash function FNV-1a and linear probing.

#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

// Hash table entries consist of a Lox string
// object as the key, and a Lox value as the value.
typedef struct {
    ObjString* key;
    Value value;
} Entry;

// Store the entries plus standard hash table metadata.
typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);

// If the given key is in the given table, make the value
// output parameter point to its value, and return true.
// Otherwise, return false.
bool tableGet(Table* table, ObjString* key, Value* value);

// Add a given key-value pair to the given table.
// Overwrite any existing pairs. Return true
// if a new entry was added and false otherwise.
bool tableSet(Table* table, ObjString* key, Value value);

// If the given key is in the given table, delete the
// key-value pair and return true. Otherwise, return false.
bool tableDelete(Table* table, ObjString* key);

// Copy all entries from one table to another.
// Used for method inheritance later.
void tableAddAll(Table* from, Table* to);

// Retrieve a string from our deduplication table.
// For string interning.
ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash);

#endif