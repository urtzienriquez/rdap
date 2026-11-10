// Debug Adapter Protocol transport: Content-Length framed JSON over two file descriptors, a
// reader thread that queues incoming messages, and thread-safe sending.
#pragma once

#include <stdbool.h>
#include <stddef.h>

#include "json.h"

// called on the reader thread for every message before it is queued; returns true if it handled
// (and freed) the message itself
typedef bool (*DapReaderHook)(Json *msg);

void dap_init(int in_fd, int out_fd, const char *log_path);
void dap_start_reader(DapReaderHook hook);

// the next queued message (a request, or {"type":"eof"} once the client is gone); NULL when the
// queue is empty and `block` is false. The caller frees it.
Json *dap_next(bool block);
bool dap_pending(void); // cheap: anything queued?
Json *dap_next_timeout(int ms); // dap_next(), waiting at most `ms`
void dap_unget(Json *msg);       // back to the front of the queue
int dap_request(const char *command, Json *args); // a request to the client; returns its seq

void dap_send(Json *msg); // adds "seq"; frees msg
void dap_respond(const Json *req, bool success, const char *message, Json *body);
void dap_event(const char *event, Json *body);
void dap_output(const char *category, const char *text, size_t len);
void dap_crash(const char *message, int exit_code); // (async-signal-safe enough)

void dap_log(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
