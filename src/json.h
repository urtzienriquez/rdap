// Minimal JSON values: parse, build, serialize. Enough for the Debug Adapter Protocol.
#pragma once

#include <stdbool.h>
#include <stddef.h>

typedef enum { J_NULL, J_BOOL, J_NUM, J_STR, J_ARR, J_OBJ } JType;

typedef struct Json Json;
struct Json {
  JType type;
  bool b;
  double n;
  char *s;      // J_STR
  Json *child;  // J_ARR / J_OBJ: first element
  Json *last;   // J_ARR / J_OBJ: last element (for appending)
  Json *next;   // sibling in the parent's list
  char *key;    // set when the value is an object member
};

Json *json_parse(const char *text, size_t len); // NULL on malformed input
char *json_dump(const Json *v);                 // malloc'd, compact
void json_free(Json *v);

// building (each returns the new value; `set`/`push` take ownership of `v`)
Json *json_null(void);
Json *json_bool(bool b);
Json *json_num(double n);
Json *json_str(const char *s);
Json *json_strn(const char *s, size_t n);
Json *json_arr(void);
Json *json_obj(void);
Json *json_push(Json *arr, Json *v);
Json *json_set(Json *obj, const char *key, Json *v);

// reading (NULL-safe: a NULL object gives the default)
Json *json_get(const Json *obj, const char *key);
const char *json_get_str(const Json *obj, const char *key, const char *def);
double json_get_num(const Json *obj, const char *key, double def);
bool json_get_bool(const Json *obj, const char *key, bool def);
size_t json_len(const Json *arr);
