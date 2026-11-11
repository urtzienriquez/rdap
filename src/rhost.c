// The embedded R: starting it, its console (output -> DAP output events), and calling rdap.R.
#define _GNU_SOURCE
#include "rdap.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define R_INTERFACE_PTRS 1
#include <R_ext/Parse.h>
#include <R_ext/Rdynload.h>
#include <Rembedded.h>
#include <Rinterface.h>

#ifndef RDAP_R_HOME
#define RDAP_R_HOME "/usr/lib/R"
#endif

// src/rdap.R, built in (RDAP_R_FILE: its absolute path, a string literal)
__asm__(".section .rodata\n"
        ".global rdap_r_source\n"
        "rdap_r_source:\n"
        ".incbin \"" RDAP_R_FILE "\"\n"
        ".byte 0\n"
        ".previous\n");
extern const char rdap_r_source[];

SEXP g_rdap_env;
int g_in_eval;
volatile int g_forked;
static int term_fd = -1; // the program's terminal (runInTerminal), if any

// ---------------------------------------------------------------------------------------------
// fd 1 and 2: anything written there directly (C code, child processes) becomes output events;
// the protocol has its own copy of the original stdout

static int cap_fd[2] = {-1, -1}; // read ends: stdout, stderr
static pthread_mutex_t cap_mu = PTHREAD_MUTEX_INITIALIZER;

static bool cap_read(int which) {
  char buf[8192];
  ssize_t n = read(cap_fd[which], buf, sizeof buf);
  if (n <= 0) return false;
  dap_output(which ? "stderr" : "stdout", buf, (size_t)n);
  return true;
}

// A pipe whose writers are all gone (fd 1/2 pointed at the program's terminal) is at its end for
// good: poll() reports it (POLLHUP) at once, every time, so it's dropped (else: a busy loop)
static void *cap_main(void *arg) {
  (void)arg;
  struct pollfd p[2] = {{cap_fd[0], POLLIN, 0}, {cap_fd[1], POLLIN, 0}};
  int open = 2;
  while (open > 0) {
    if (poll(p, 2, -1) < 0) {
      if (errno == EINTR) continue;
      return NULL;
    }
    pthread_mutex_lock(&cap_mu);
    for (int i = 0; i < 2; i++) {
      if (p[i].fd < 0 || !p[i].revents) continue;
      bool data = false;
      if (p[i].revents & POLLIN) data = cap_read(i);
      if (!data && (p[i].revents & (POLLHUP | POLLERR | POLLNVAL))) { // (what was left: read)
        p[i].fd = -1;
        open--;
      }
    }
    pthread_mutex_unlock(&cap_mu);
  }
  return NULL;
}

void capture_start(void) {
  for (int i = 0; i < 2; i++) {
    int p[2];
    if (pipe(p) != 0) return;
    dup2(p[1], i + 1);
    close(p[1]);
    fcntl(p[0], F_SETFL, fcntl(p[0], F_GETFL) | O_NONBLOCK);
    fcntl(p[0], F_SETFD, FD_CLOEXEC);
    cap_fd[i] = p[0];
  }
  setvbuf(stdout, NULL, _IONBF, 0);
  pthread_t t;
  pthread_create(&t, NULL, cap_main, NULL);
  pthread_detach(t);
}

void capture_drain(void) {
  if (cap_fd[0] < 0) return;
  fflush(stdout);
  fflush(stderr);
  pthread_mutex_lock(&cap_mu);
  for (int i = 0; i < 2; i++)
    while (cap_read(i)) {
    }
  pthread_mutex_unlock(&cap_mu);
}

// ---------------------------------------------------------------------------------------------
// R's console

static void con_write(const char *buf, int len, int otype) {
  if (g_forked) { // a forked child's output: to the pipes the parent forwards
    ssize_t w = write(otype ? 2 : 1, buf, (size_t)len);
    (void)w;
    return;
  }
  if (term_fd >= 0) {
    for (int done = 0; done < len;) {
      ssize_t w = write(term_fd, buf + done, (size_t)(len - done));
      if (w <= 0) break;
      done += (int)w;
    }
    return;
  }
  dap_output(otype ? "stderr" : "stdout", buf, (size_t)len);
}

void rhost_set_terminal(int fd) {
  term_fd = fd;
  dup2(fd, 1); // (what C code and child processes write too)
  dup2(fd, 2);
}

void rhost_terminal_write(const char *s) {
  if (term_fd >= 0) con_write(s, (int)strlen(s), 0);
}

// a line typed in the terminal (without its newline); false at its end
static bool terminal_line(const char *prompt, char *buf, int len) {
  con_write(prompt, (int)strlen(prompt), 0);
  int n = 0;
  while (n < len - 2) {
    char c;
    ssize_t r = read(term_fd, &c, 1);
    if (r <= 0) return n > 0 ? (buf[n] = 0, true) : false;
    if (c == '\n') break;
    if (c != '\r') buf[n++] = c;
  }
  buf[n] = 0;
  return true;
}

static int con_read(const char *prompt, unsigned char *buf, int len, int hist) {
  (void)hist;
  const char *answer;
  if (g_forked) {
    answer = "";
  } else if (strncmp(prompt, "Browse[", 7) == 0) { // browser() in the program: a breakpoint
    answer = debugger_browser_stop();
  } else if (term_fd >= 0) { // input for the program (readline(), scan(), menu(), ...)
    static char line[4096];
    answer = terminal_line(prompt, line, sizeof line) ? line : "";
  } else {
    static bool warned;
    if (!warned) {
      const char *msg = "rdap: the program asked for input, but there's no terminal for it (the editor didn't "
                        "open one): it gets empty lines\n";
      dap_output("console", msg, strlen(msg));
      warned = true;
    }
    answer = "";
  }
  snprintf((char *)buf, (size_t)len, "%s\n", answer);
  return 1;
}

static void con_message(const char *s) {
  dap_output("stderr", s, strlen(s));
  dap_output("stderr", "\n", 1);
}

static void con_busy(int which) { (void)which; }
static void con_events(void) { debugger_service_running(); }
static void con_cleanup(SA_TYPE save, int status, int run_last) {
  (void)save;
  (void)run_last;
  debugger_quit(status);
}

static void set_console(void) {
  R_Outputfile = NULL;
  R_Consolefile = NULL;
  ptr_R_WriteConsole = NULL;
  ptr_R_WriteConsoleEx = con_write;
  ptr_R_ReadConsole = con_read;
  ptr_R_ShowMessage = con_message;
  ptr_R_Busy = con_busy;
  ptr_R_ProcessEvents = con_events;
  ptr_R_CleanUp = con_cleanup;
}

// ---------------------------------------------------------------------------------------------
// forks and crashes

static void on_fork_child(void) { g_forked = 1; }

static void on_crash(int sig) {
  if (!g_forked) {
    const char *what = sig == SIGSEGV ? "segmentation fault"
                     : sig == SIGBUS  ? "bus error"
                     : sig == SIGILL  ? "illegal instruction"
                     : sig == SIGFPE  ? "arithmetic exception" : "fatal signal";
    char where[512], msg[1024];
    debugger_crash_location(where, sizeof where);
    snprintf(msg, sizeof msg,
             "\nrdap: R crashed (%s), in compiled code called after the R statement at %s\n", what, where);
    dap_crash(msg, 128 + sig);
  }
  signal(sig, SIG_DFL);
  raise(sig);
}

// (after R's own: R's would ask, at the console, what to do)
void crash_handlers_install(void) {
  static char altstack[1 << 16];
  stack_t ss = {.ss_sp = altstack, .ss_size = sizeof altstack};
  sigaltstack(&ss, NULL);
  struct sigaction sa = {0};
  sa.sa_handler = on_crash;
  sa.sa_flags = SA_ONSTACK;
  sigemptyset(&sa.sa_mask);
  int sigs[] = {SIGSEGV, SIGBUS, SIGILL, SIGFPE};
  for (size_t i = 0; i < sizeof sigs / sizeof *sigs; i++) sigaction(sigs[i], &sa, NULL);
  pthread_atfork(NULL, NULL, on_fork_child);
}

// ---------------------------------------------------------------------------------------------
// starting R

static const R_CallMethodDef call_methods[] = {
    {"rdap_instrument", (DL_FUNC)rdap_instrument, 2},
    {"rdap_exception", (DL_FUNC)rdap_exception, 4},
    {"rdap_swap_body", (DL_FUNC)rdap_swap_body, 2},
    {"rdap_instrument_closures", (DL_FUNC)rdap_instrument_closures, 2},
    {"rdap_env_bindings", (DL_FUNC)rdap_env_bindings, 1},
    {"rdap_scope_end", (DL_FUNC)rdap_scope_end, 1},
    {"rdap_program_end", (DL_FUNC)rdap_program_end, 0},
    {NULL, NULL, 0},
};
static const R_ExternalMethodDef external_methods[] = {
    {"rdap_probe", (DL_FUNC)rdap_probe, -1},
    {NULL, NULL, 0},
};

static bool eval_source(const char *code, SEXP env) {
  ParseStatus status;
  SEXP text = PROTECT(Rf_mkString(code));
  SEXP exprs = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    UNPROTECT(2);
    return false;
  }
  for (R_xlen_t i = 0; i < XLENGTH(exprs); i++) {
    int err = 0;
    R_tryEval(VECTOR_ELT(exprs, i), env, &err);
    if (err) {
      UNPROTECT(2);
      return false;
    }
  }
  UNPROTECT(2);
  return true;
}

bool rhost_init(int nargs, char **args, char *err, size_t errlen) {
  if (!getenv("R_HOME")) setenv("R_HOME", RDAP_R_HOME, 1);
  char *base[] = {"rdap", "--no-save", "--no-restore", "--quiet", "--no-readline", "--args"};
  int nbase = sizeof base / sizeof *base;
  char **argv = calloc((size_t)(nbase + nargs + 1), sizeof *argv);
  for (int i = 0; i < nbase; i++) argv[i] = base[i];
  for (int i = 0; i < nargs; i++) argv[nbase + i] = args[i];
  int argc = nbase + nargs;
  if (nargs == 0) argc--; // no "--args"

  Rf_initialize_R(argc, argv);
  R_Interactive = TRUE;
  set_console();
  setup_Rmainloop(); // (runs the profiles)
  set_console();     // again: a profile may have replaced them (colorout does)
  crash_handlers_install();

  DllInfo *dll = R_getEmbeddingDllInfo();
  R_registerRoutines(dll, NULL, call_methods, NULL, external_methods);
  R_useDynamicSymbols(dll, FALSE);

  g_rdap_env = R_NewEnv(R_BaseEnv, TRUE, 64);
  R_PreserveObject(g_rdap_env);
  g_in_eval++;
  bool ok = eval_source(rdap_r_source, g_rdap_env);
  g_in_eval--;
  if (!ok) {
    snprintf(err, errlen, "rdap: its R code failed to load");
    return false;
  }
  if (!rhost_call("init", 0)) {
    snprintf(err, errlen, "rdap: its R code failed to initialize");
    return false;
  }
  SEXP sym = rhost_call("probe_symbol", 0);
  if (!sym) {
    snprintf(err, errlen, "rdap: its probe routine isn't registered");
    return false;
  }
  instrument_init(sym);
  return true;
}

SEXP rhost_call(const char *fn, int nargs, ...) {
  SEXP f = Rf_findVarInFrame(g_rdap_env, Rf_install(fn));
  if (f == R_UnboundValue) {
    dap_log("rdap.R has no %s()", fn);
    return NULL;
  }
  SEXP call = PROTECT(Rf_allocLang(nargs + 1));
  SETCAR(call, f);
  va_list ap;
  va_start(ap, nargs);
  SEXP p = CDR(call);
  for (int i = 0; i < nargs; i++, p = CDR(p)) {
    SEXP arg = va_arg(ap, SEXP);
    // code and symbols are passed as such (in a call, they'd be evaluated)
    if (TYPEOF(arg) == LANGSXP || TYPEOF(arg) == SYMSXP || TYPEOF(arg) == PROMSXP) arg = Rf_lang2(R_QuoteSymbol, arg);
    SETCAR(p, arg);
  }
  va_end(ap);
  int err = 0;
  g_in_eval++;
  SEXP res = R_tryEvalSilent(call, g_rdap_env, &err);
  g_in_eval--;
  UNPROTECT(1);
  if (err) {
    dap_log("%s() failed: %s", fn, R_curErrorBuf());
    return NULL;
  }
  return res;
}
