// Standard hash table implementation using
// hash function FNV-1a and linear probing.
// Only accepts keys that are strings, since
// we don't need other types for clox.
// Deletion uses tombstones. For more details, see:
// https://craftinginterpreters.com/hash-tables.html#deleting-entries

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
// we still return an entry - we'll use
// this to insert new key-value pairs:
// - We return the first tombstone slots we encounter (old
//   deleted entries - see tableDelete()), if present.
// - Otherwise, we just return the key's entry.
// For more details, see:
// https://craftinginterpreters.com/hash-tables.html#deleting-entries
static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;

    // Handle deleted entries - see tableDelete().
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entry->value)) {
              // Empty entry. If we've stored a tombstone,
              // return it. Otherwise, return the entry.
              return tombstone != NULL ? tombstone : entry;
            } else {
              // We found a tombstone. Store the first
              // we find.
              if (tombstone == NULL) tombstone = entry;
            }
          } else if (entry->key == key) {
            // We found the key.
            return entry;
          }
        // If we don't find the key right away,
        // there must have been a hash collision.
        // Continue the probe sequence.
        index = (index + 1) % capacity;
    }
}

// If the given key is in the given table, make the value
// output parameter point to its value, and return true.
// Otherwise, return false.
bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
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

    // Rehash any existing entries, ignoring tombstones.

    // Reset count - we only want to include non-tombstone entries.
    table->count = 0;

    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) continue;
        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;

        // Count each element we find, to avoid counting tombstones.
        table->count++;
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

    // Only increment the count if we're adding the value
    // to an empty bucket. Don't increment if we
    // add to tombstones - this would artificially inflate
    // the load factor.
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

// If the given key is in the given table, delete the
// key-value pair and return true. Otherwise, return false.
// We replace the deleted value with a special "tombstone"
// entry to avoid breaking linear probing.
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    // Find the entry.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;

    // Place a tombstone in the entry -
    // a NULL key and a true value.
    entry->key = NULL;
    entry->value = BOOL_VAL(true);

    return true;
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

// Check if we have stored the input string
// in our string interning table.
ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash) {
  // If the table is empty, we haven't seen the string before.
  if (table->count == 0) return NULL;

  // Search through the hash table for the string.

  uint32_t index = hash % table->capacity;
  for (;;) {
    Entry* entry = &table->entries[index];
    // If the hash isn't in the table,
    // we haven't seen the string before.
    if (entry->key == NULL) {
      // Stop if we find an empty non-tombstone entry.
      if (IS_NIL(entry->value)) return NULL;

      // In case of a hash collision,
      // if the strings are different lengths,
      // or the hashes are different,
      // the strings aren't the same - no need
      // to compare all characters of the string.
      // If all characters of the strings
      // are the same, we have seen this string before.
    } else if (entry->key->length == length &&
        entry->key->hash == hash &&
        memcmp(entry->key->chars, chars, length) == 0) {
      // We found it.
      return entry->key;
    }

    index = (index + 1) % table->capacity;
  }
}
