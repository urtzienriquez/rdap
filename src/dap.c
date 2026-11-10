#include "dap.h"

#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static int in_fd = -1, out_fd = -1;
static FILE *log_file;
static pthread_mutex_t send_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t log_mu = PTHREAD_MUTEX_INITIALIZER;
static int out_seq = 1;

typedef struct Node {
  Json *msg;
  struct Node *next;
} Node;
static Node *q_head, *q_tail;
static pthread_mutex_t q_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t q_cv = PTHREAD_COND_INITIALIZER;
static atomic_int q_len;

static DapReaderHook reader_hook;

void dap_init(int in, int out, const char *log_path) {
  in_fd = in;
  out_fd = out;
  if (log_path && *log_path) log_file = fopen(log_path, "w");
}

void dap_log(const char *fmt, ...) {
  if (!log_file) return;
  pthread_mutex_lock(&log_mu);
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  fprintf(log_file, "[%ld.%03ld] ", (long)ts.tv_sec, ts.tv_nsec / 1000000);
  va_list ap;
  va_start(ap, fmt);
  vfprintf(log_file, fmt, ap);
  va_end(ap);
  fputc('\n', log_file);
  fflush(log_file);
  pthread_mutex_unlock(&log_mu);
}

// ---------------------------------------------------------------------------------------------
// sending

static void write_all(const char *p, size_t n) {
  while (n > 0) {
    ssize_t w = write(out_fd, p, n);
    if (w < 0) {
      if (errno == EINTR) continue;
      return; // the client is gone; the reader thread notices EOF
    }
    p += w;
    n -= (size_t)w;
  }
}

extern volatile int g_forked;

void dap_send(Json *msg) {
  if (g_forked) { // (a forked child: the parent's connection isn't its own)
    json_free(msg);
    return;
  }
  pthread_mutex_lock(&send_mu);
  // "seq" goes first, for readable logs
  Json *seq = json_num(out_seq++);
  seq->key = strdup("seq");
  seq->next = msg->child;
  msg->child = seq;
  if (!msg->last) msg->last = seq;
  char *body = json_dump(msg);
  size_t n = strlen(body);
  char header[64];
  int hn = snprintf(header, sizeof header, "Content-Length: %zu\r\n\r\n", n);
  write_all(header, (size_t)hn);
  write_all(body, n);
  pthread_mutex_unlock(&send_mu);
  dap_log("-> %s", body);
  free(body);
  json_free(msg);
}

void dap_respond(const Json *req, bool success, const char *message, Json *body) {
  Json *r = json_obj();
  json_set(r, "type", json_str("response"));
  json_set(r, "request_seq", json_num(json_get_num(req, "seq", 0)));
  json_set(r, "success", json_bool(success));
  json_set(r, "command", json_str(json_get_str(req, "command", "")));
  if (message) json_set(r, "message", json_str(message));
  if (!success && message && !body) { // clients show body.error.format
    body = json_obj();
    Json *err = json_set(body, "error", json_obj());
    json_set(err, "id", json_num(1));
    json_set(err, "format", json_str(message));
    json_set(err, "showUser", json_bool(true));
  }
  json_set(r, "body", body ? body : json_obj());
  dap_send(r);
}

void dap_event(const char *event, Json *body) {
  Json *e = json_obj();
  json_set(e, "type", json_str("event"));
  json_set(e, "event", json_str(event));
  json_set(e, "body", body ? body : json_obj());
  dap_send(e);
}

// The end of a crashed process (from a signal handler): an output event, exited, terminated,
// written by hand (no allocation, no lock to wait for)
static void put_raw(const char *body) {
  char header[64];
  int hn = snprintf(header, sizeof header, "Content-Length: %zu\r\n\r\n", strlen(body));
  write_all(header, (size_t)hn);
  write_all(body, strlen(body));
}

void dap_crash(const char *message, int exit_code) {
  pthread_mutex_trylock(&send_mu); // (held by a thread that crashed in it? write anyway)
  char esc[1024], *o = esc;
  for (const char *p = message; *p && o < esc + sizeof esc - 8; p++) {
    if (*p == '"' || *p == '\\') *o++ = '\\';
    if (*p == '\n') { *o++ = '\\'; *o++ = 'n'; continue; }
    *o++ = *p;
  }
  *o = 0;
  char body[1400];
  snprintf(body, sizeof body,
           "{\"seq\":%d,\"type\":\"event\",\"event\":\"output\",\"body\":{\"category\":\"stderr\",\"output\":\"%s\"}}",
           out_seq++, esc);
  put_raw(body);
  snprintf(body, sizeof body, "{\"seq\":%d,\"type\":\"event\",\"event\":\"exited\",\"body\":{\"exitCode\":%d}}",
           out_seq++, exit_code);
  put_raw(body);
  snprintf(body, sizeof body, "{\"seq\":%d,\"type\":\"event\",\"event\":\"terminated\",\"body\":{}}", out_seq++);
  put_raw(body);
}

void dap_output(const char *category, const char *text, size_t len) {
  if (len == 0) return;
  Json *b = json_obj();
  json_set(b, "category", json_str(category));
  json_set(b, "output", json_strn(text, len));
  dap_event("output", b);
}

// ---------------------------------------------------------------------------------------------
// receiving

static void enqueue(Json *msg) {
  Node *n = malloc(sizeof *n);
  n->msg = msg;
  n->next = NULL;
  pthread_mutex_lock(&q_mu);
  if (q_tail)
    q_tail->next = n;
  else
    q_head = n;
  q_tail = n;
  atomic_fetch_add(&q_len, 1);
  pthread_cond_signal(&q_cv);
  pthread_mutex_unlock(&q_mu);
}

Json *dap_next(bool block) {
  pthread_mutex_lock(&q_mu);
  while (!q_head && block) pthread_cond_wait(&q_cv, &q_mu);
  Node *n = q_head;
  if (n) {
    q_head = n->next;
    if (!q_head) q_tail = NULL;
    atomic_fetch_sub(&q_len, 1);
  }
  pthread_mutex_unlock(&q_mu);
  if (!n) return NULL;
  Json *m = n->msg;
  free(n);
  return m;
}

bool dap_pending(void) { return atomic_load_explicit(&q_len, memory_order_relaxed) > 0; }

Json *dap_next_timeout(int ms) {
  struct timespec until;
  clock_gettime(CLOCK_REALTIME, &until);
  until.tv_sec += ms / 1000;
  until.tv_nsec += (long)(ms % 1000) * 1000000;
  if (until.tv_nsec >= 1000000000) {
    until.tv_sec++;
    until.tv_nsec -= 1000000000;
  }
  pthread_mutex_lock(&q_mu);
  while (!q_head)
    if (pthread_cond_timedwait(&q_cv, &q_mu, &until) != 0) break;
  pthread_mutex_unlock(&q_mu);
  return dap_next(false);
}

void dap_unget(Json *msg) {
  Node *n = malloc(sizeof *n);
  n->msg = msg;
  pthread_mutex_lock(&q_mu);
  n->next = q_head;
  q_head = n;
  if (!q_tail) q_tail = n;
  atomic_fetch_add(&q_len, 1);
  pthread_cond_signal(&q_cv);
  pthread_mutex_unlock(&q_mu);
}

int dap_request(const char *command, Json *args) {
  Json *r = json_obj();
  json_set(r, "type", json_str("request"));
  json_set(r, "command", json_str(command));
  json_set(r, "arguments", args);
  pthread_mutex_lock(&send_mu);
  int seq = out_seq; // (the seq dap_send gives it)
  pthread_mutex_unlock(&send_mu);
  dap_send(r);
  return seq;
}

typedef struct {
  char *buf;
  size_t len, cap;
} RBuf;

static bool fill(RBuf *b) {
  if (b->len == b->cap) {
    b->cap = b->cap ? b->cap * 2 : 65536;
    b->buf = realloc(b->buf, b->cap);
  }
  for (;;) {
    ssize_t r = read(in_fd, b->buf + b->len, b->cap - b->len);
    if (r < 0 && errno == EINTR) continue;
    if (r <= 0) return false;
    b->len += (size_t)r;
    return true;
  }
}

static void *reader_main(void *arg) {
  (void)arg;
  RBuf b = {0};
  for (;;) {
    // header
    char *sep;
    while (!(b.len >= 4 && (sep = memmem(b.buf, b.len, "\r\n\r\n", 4))))
      if (!fill(&b)) goto eof;
    size_t header_len = (size_t)(sep - b.buf) + 4;
    long content_len = -1;
    for (char *line = b.buf; line < sep;) {
      char *eol = memmem(line, (size_t)(sep - line) + 2, "\r\n", 2);
      if (!eol) break;
      if (strncasecmp(line, "Content-Length:", 15) == 0) content_len = strtol(line + 15, NULL, 10);
      line = eol + 2;
    }
    if (content_len < 0) { // garbage: drop the header and resync
      memmove(b.buf, b.buf + header_len, b.len - header_len);
      b.len -= header_len;
      continue;
    }
    while (b.len < header_len + (size_t)content_len)
      if (!fill(&b)) goto eof;
    const char *body = b.buf + header_len;
    dap_log("<- %.*s", (int)content_len, body);
    Json *msg = json_parse(body, (size_t)content_len);
    size_t used = header_len + (size_t)content_len;
    memmove(b.buf, b.buf + used, b.len - used);
    b.len -= used;
    if (!msg) {
      dap_log("malformed message dropped");
      continue;
    }
    if (reader_hook && reader_hook(msg)) continue;
    enqueue(msg);
  }
eof:
  dap_log("client closed the connection");
  Json *e = json_obj();
  json_set(e, "type", json_str("eof"));
  if (!reader_hook || !reader_hook(e)) enqueue(e);
  free(b.buf);
  return NULL;
}

void dap_start_reader(DapReaderHook hook) {
  reader_hook = hook;
  pthread_t t;
  pthread_create(&t, NULL, reader_main, NULL);
  pthread_detach(t);
}
