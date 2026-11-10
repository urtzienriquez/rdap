// rdap: a debug adapter for R that embeds R itself. Shared declarations.
#pragma once

#define R_NO_REMAP
#include <Rinternals.h>
#include <stdatomic.h>
#include <stdbool.h>

#include "dap.h"
#include "json.h"

#define RDAP_VERSION "0.1.0"

// ---------------------------------------------------------------------------------------------
// state shared with the reader thread

enum { ST_IDLE, ST_RUNNING, ST_STOPPED, ST_DONE };
extern atomic_int g_state; // ST_*
extern atomic_int g_pause; // a pause was requested: stop at the next probe

// ---------------------------------------------------------------------------------------------
// rhost.c: the embedded R

extern SEXP g_rdap_env; // the environment of src/rdap.R's helpers
extern int g_in_eval;   // >0 while rdap itself evaluates R code: probes and error stops are off
extern volatile int g_forked; // this is a forked child (parallel::mclapply): it's not debugged

void capture_start(void); // fd 1/2 -> output events (call before R starts)
void capture_drain(void); // forward what is still buffered in the pipes
bool rhost_init(int nargs, char **args, char *err, size_t errlen);
void rhost_set_terminal(int fd); // the program's console: this terminal
void rhost_terminal_write(const char *s);
// call the helper `fn` of src/rdap.R with the given arguments (each an R value), with probes off;
// NULL if it signalled an error. The result is unprotected.
SEXP rhost_call(const char *fn, int nargs, ...);

// ---------------------------------------------------------------------------------------------
// debugger.c

void debugger_loop(void); // the main thread's loop; never returns
bool debugger_reader_hook(Json *msg);
void debugger_service_running(void);    // handle what can be handled while R runs
const char *debugger_browser_stop(void); // a browser() prompt: stop there, then what to type
void debugger_quit(int status);          // R's quit(): end the session
void debugger_crash_location(char *buf, size_t n);
void crash_handlers_install(void);

// probes and files (registered with R)
SEXP rdap_probe(SEXP call, SEXP op, SEXP args, SEXP rho); // .External2
SEXP rdap_instrument(SEXP exprs, SEXP path);
SEXP rdap_exception(SEXP header, SEXP message, SEXP kind, SEXP condition);
SEXP rdap_swap_body(SEXP fn, SEXP body);
SEXP rdap_instrument_closures(SEXP fns, SEXP paths);
SEXP rdap_env_bindings(SEXP env);
SEXP rdap_scope_end(SEXP target);
SEXP rdap_program_end(void);

int file_register(const char *path);
int probe_new(int file, int line, int col, int end_line, int end_col);
void probe_set_expr(int id, SEXP statement); // the statement a probe is for (for stepping into it)
void probe_set_toplevel(int id);              // a probe before a script's top-level statement
void file_instrumented(int file);

// ---------------------------------------------------------------------------------------------
// instrument.c

void instrument_init(SEXP probe_symbol);
SEXP instrument_exprs(SEXP exprs, int file);
SEXP instrument_call(SEXP e, int file);
SEXP instrument_body(SEXP body, SEXP fn_srcref, int file);
