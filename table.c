// Standard hash table implementation using
// hash function FNV-1a and linear probing.

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

// Load factor. Grow the table's array when
// it is >=75% full. Chosen fairly arbitrarily -
// could be optimised using usage benchmarks.
#define TABLE_MAX_LOAD 0.75

// Standard initialisation.
void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

// We can reuse our dynamic array FREE_ARRAY macro
// since the hash table entries are stored in an array.
void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

// Given a key, return its entry using linear probing.
// If there's no value associated with the key,
// we still return the entry - we'll use
// this to insert new key-value pairs.
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == key || entry->key == NULL) {
            return entry;
        }
        // If we don't find the key right away,
        // there must have been a hash collision.
        // Continue the probe sequence.
        index = (index + 1) % capacity;
    }
}

// Adjust the size of our table. On init, we
// allocate each entry of our table with null values.
// For subsequent adjustments, we must allocate new
// memory, but also re-hash all existing entries.
// This is because our hash function depends on array
// size, which has just changed.
static void adjustCapacity(Table* table, int capacity) {
    // Allocate our new array.
    Entry* entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // Rehash any existing entries.
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
    }

    // Free the old array.
    FREE_ARRAY(Entry, table->entries, table->capacity);

    // Point our table to the new array and capacity.
    table->entries = entries;
    table->capacity = capacity;
}

// Add a given key-value pair to the given table.
// Overwrite any existing pairs. Return true
// if a new entry was added and false otherwise.
bool tableSet(Table* table, ObjString* key, Value value) {
    // If the table's maximum size has been reached,
    // allocate memory for the required entry.
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;
    if (isNewKey) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// Copy all entries from one table to another.
// Used for method inheritance later.
void tableAddAll(Table* from, Table* to) {
    // Iterate through all "from" table entries.
    for (int i = 0; i < from->capacity; i++) {
        // Extract entry data.
        Entry* entry = &from->entries[i];
        if (entry->key != NULL) {
            // Copy the entry to the new table.
            tableSet(to, entry->key, entry->value);
        }
    }
  }
