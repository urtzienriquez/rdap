#include "json.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Json *jnew(JType t) {
  Json *v = calloc(1, sizeof *v);
  v->type = t;
  return v;
}

Json *json_null(void) { return jnew(J_NULL); }
Json *json_bool(bool b) {
  Json *v = jnew(J_BOOL);
  v->b = b;
  return v;
}
Json *json_num(double n) {
  Json *v = jnew(J_NUM);
  v->n = n;
  return v;
}
Json *json_strn(const char *s, size_t n) {
  Json *v = jnew(J_STR);
  v->s = malloc(n + 1);
  memcpy(v->s, s, n);
  v->s[n] = 0;
  return v;
}
Json *json_str(const char *s) { return s ? json_strn(s, strlen(s)) : json_null(); }
Json *json_arr(void) { return jnew(J_ARR); }
Json *json_obj(void) { return jnew(J_OBJ); }

static void append(Json *parent, Json *v) {
  if (parent->last)
    parent->last->next = v;
  else
    parent->child = v;
  parent->last = v;
}

Json *json_push(Json *arr, Json *v) {
  append(arr, v);
  return v;
}

Json *json_set(Json *obj, const char *key, Json *v) {
  v->key = strdup(key);
  append(obj, v);
  return v;
}

void json_free(Json *v) {
  while (v) {
    Json *next = v->next;
    json_free(v->child);
    free(v->s);
    free(v->key);
    free(v);
    v = next;
  }
}

Json *json_get(const Json *obj, const char *key) {
  if (!obj || obj->type != J_OBJ) return NULL;
  for (Json *c = obj->child; c; c = c->next)
    if (strcmp(c->key, key) == 0) return c;
  return NULL;
}

const char *json_get_str(const Json *obj, const char *key, const char *def) {
  Json *v = json_get(obj, key);
  return v && v->type == J_STR ? v->s : def;
}

double json_get_num(const Json *obj, const char *key, double def) {
  Json *v = json_get(obj, key);
  return v && v->type == J_NUM ? v->n : def;
}

bool json_get_bool(const Json *obj, const char *key, bool def) {
  Json *v = json_get(obj, key);
  return v && v->type == J_BOOL ? v->b : def;
}

size_t json_len(const Json *arr) {
  size_t n = 0;
  if (arr && (arr->type == J_ARR || arr->type == J_OBJ))
    for (Json *c = arr->child; c; c = c->next) n++;
  return n;
}

// ---------------------------------------------------------------------------------------------
// parsing

typedef struct {
  const char *p, *end;
  int depth;
} Parser;

static void skip_ws(Parser *ps) {
  while (ps->p < ps->end && (*ps->p == ' ' || *ps->p == '\t' || *ps->p == '\n' || *ps->p == '\r'))
    ps->p++;
}

static bool lit(Parser *ps, const char *word) {
  size_t n = strlen(word);
  if ((size_t)(ps->end - ps->p) < n || memcmp(ps->p, word, n) != 0) return false;
  ps->p += n;
  return true;
}

static int hex4(const char *p) {
  int v = 0;
  for (int i = 0; i < 4; i++) {
    char c = p[i];
    v <<= 4;
    if (c >= '0' && c <= '9') v |= c - '0';
    else if (c >= 'a' && c <= 'f') v |= c - 'a' + 10;
    else if (c >= 'A' && c <= 'F') v |= c - 'A' + 10;
    else return -1;
  }
  return v;
}

static size_t utf8_put(char *out, unsigned cp) {
  if (cp < 0x80) {
    out[0] = (char)cp;
    return 1;
  }
  if (cp < 0x800) {
    out[0] = (char)(0xC0 | (cp >> 6));
    out[1] = (char)(0x80 | (cp & 0x3F));
    return 2;
  }
  if (cp < 0x10000) {
    out[0] = (char)(0xE0 | (cp >> 12));
    out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (char)(0x80 | (cp & 0x3F));
    return 3;
  }
  out[0] = (char)(0xF0 | (cp >> 18));
  out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
  out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
  out[3] = (char)(0x80 | (cp & 0x3F));
  return 4;
}

static char *parse_string_raw(Parser *ps) {
  if (ps->p >= ps->end || *ps->p != '"') return NULL;
  ps->p++;
  // the decoded string is never longer than the encoded one
  const char *start = ps->p;
  char *out = malloc((size_t)(ps->end - start) + 1), *o = out;
  while (ps->p < ps->end && *ps->p != '"') {
    char c = *ps->p++;
    if (c != '\\') {
      *o++ = c;
      continue;
    }
    if (ps->p >= ps->end) goto bad;
    c = *ps->p++;
    switch (c) {
    case '"': *o++ = '"'; break;
    case '\\': *o++ = '\\'; break;
    case '/': *o++ = '/'; break;
    case 'b': *o++ = '\b'; break;
    case 'f': *o++ = '\f'; break;
    case 'n': *o++ = '\n'; break;
    case 'r': *o++ = '\r'; break;
    case 't': *o++ = '\t'; break;
    case 'u': {
      if (ps->end - ps->p < 4) goto bad;
      int cp = hex4(ps->p);
      if (cp < 0) goto bad;
      ps->p += 4;
      if (cp >= 0xD800 && cp <= 0xDBFF && ps->end - ps->p >= 6 && ps->p[0] == '\\' && ps->p[1] == 'u') {
        int lo = hex4(ps->p + 2);
        if (lo >= 0xDC00 && lo <= 0xDFFF) {
          cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
          ps->p += 6;
        }
      }
      o += utf8_put(o, (unsigned)cp);
      break;
    }
    default: goto bad;
    }
  }
  if (ps->p >= ps->end) goto bad;
  ps->p++; // closing quote
  *o = 0;
  return out;
bad:
  free(out);
  return NULL;
}

static Json *parse_value(Parser *ps);

static Json *parse_container(Parser *ps, bool is_obj) {
  if (++ps->depth > 200) return NULL;
  ps->p++; // [ or {
  Json *v = is_obj ? json_obj() : json_arr();
  skip_ws(ps);
  char close = is_obj ? '}' : ']';
  if (ps->p < ps->end && *ps->p == close) {
    ps->p++;
    ps->depth--;
    return v;
  }
  for (;;) {
    skip_ws(ps);
    char *key = NULL;
    if (is_obj) {
      key = parse_string_raw(ps);
      if (!key) goto bad;
      skip_ws(ps);
      if (ps->p >= ps->end || *ps->p != ':') {
        free(key);
        goto bad;
      }
      ps->p++;
    }
    Json *item = parse_value(ps);
    if (!item) {
      free(key);
      goto bad;
    }
    item->key = key;
    append(v, item);
    skip_ws(ps);
    if (ps->p < ps->end && *ps->p == ',') {
      ps->p++;
      continue;
    }
    if (ps->p < ps->end && *ps->p == close) {
      ps->p++;
      ps->depth--;
      return v;
    }
    goto bad;
  }
bad:
  json_free(v);
  return NULL;
}

static Json *parse_value(Parser *ps) {
  skip_ws(ps);
  if (ps->p >= ps->end) return NULL;
  char c = *ps->p;
  if (c == '{') return parse_container(ps, true);
  if (c == '[') return parse_container(ps, false);
  if (c == '"') {
    char *s = parse_string_raw(ps);
    if (!s) return NULL;
    Json *v = jnew(J_STR);
    v->s = s;
    return v;
  }
  if (lit(ps, "true")) return json_bool(true);
  if (lit(ps, "false")) return json_bool(false);
  if (lit(ps, "null")) return json_null();
  if (c == '-' || (c >= '0' && c <= '9')) {
    char buf[64];
    size_t n = 0;
    while (ps->p < ps->end && n < sizeof buf - 1 && strchr("+-0123456789.eE", *ps->p)) buf[n++] = *ps->p++;
    buf[n] = 0;
    char *endp;
    double d = strtod(buf, &endp);
    if (endp == buf) return NULL;
    return json_num(d);
  }
  return NULL;
}

Json *json_parse(const char *text, size_t len) {
  Parser ps = {text, text + len, 0};
  Json *v = parse_value(&ps);
  if (!v) return NULL;
  skip_ws(&ps);
  if (ps.p != ps.end) {
    json_free(v);
    return NULL;
  }
  return v;
}

// ---------------------------------------------------------------------------------------------
// serializing

typedef struct {
  char *buf;
  size_t len, cap;
} Buf;

static void put(Buf *b, const char *s, size_t n) {
  if (b->len + n + 1 > b->cap) {
    while (b->len + n + 1 > b->cap) b->cap = b->cap ? b->cap * 2 : 256;
    b->buf = realloc(b->buf, b->cap);
  }
  memcpy(b->buf + b->len, s, n);
  b->len += n;
  b->buf[b->len] = 0;
}
static void puts_(Buf *b, const char *s) { put(b, s, strlen(s)); }

static void put_string(Buf *b, const char *s) {
  put(b, "\"", 1);
  for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
    switch (*p) {
    case '"': puts_(b, "\\\""); break;
    case '\\': puts_(b, "\\\\"); break;
    case '\n': puts_(b, "\\n"); break;
    case '\r': puts_(b, "\\r"); break;
    case '\t': puts_(b, "\\t"); break;
    case '\b': puts_(b, "\\b"); break;
    case '\f': puts_(b, "\\f"); break;
    default:
      if (*p < 0x20) {
        char esc[8];
        snprintf(esc, sizeof esc, "\\u%04x", *p);
        puts_(b, esc);
      } else {
        put(b, (const char *)p, 1);
      }
    }
  }
  put(b, "\"", 1);
}

static void dump(Buf *b, const Json *v) {
  switch (v->type) {
  case J_NULL: puts_(b, "null"); break;
  case J_BOOL: puts_(b, v->b ? "true" : "false"); break;
  case J_NUM: {
    char num[64];
    if (!isfinite(v->n)) snprintf(num, sizeof num, "null");
    else if (v->n == (double)(long long)v->n && fabs(v->n) < 1e15) snprintf(num, sizeof num, "%lld", (long long)v->n);
    else snprintf(num, sizeof num, "%.17g", v->n);
    puts_(b, num);
    break;
  }
  case J_STR: put_string(b, v->s); break;
  case J_ARR:
  case J_OBJ: {
    put(b, v->type == J_ARR ? "[" : "{", 1);
    for (Json *c = v->child; c; c = c->next) {
      if (c != v->child) put(b, ",", 1);
      if (v->type == J_OBJ) {
        put_string(b, c->key);
        put(b, ":", 1);
      }
      dump(b, c);
    }
    put(b, v->type == J_ARR ? "]" : "}", 1);
    break;
  }
  }
}

char *json_dump(const Json *v) {
  Buf b = {0};
  dump(&b, v);
  return b.buf;
}
