// The debugger: files, probes and breakpoints; stopping, stepping; the requests.
#define _GNU_SOURCE
#include "rdap.h"

#include <ctype.h>
#include <limits.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <sys/stat.h>
#include <stdint.h>
#include <sys/uio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <R_ext/Parse.h>
#include <Rembedded.h>

atomic_int g_state = ST_IDLE;
atomic_int g_pause;
// R code the user gave is running (an evaluation, a breakpoint's condition): pause or terminate
// interrupt it (it may never end: repeat {})
static atomic_int user_code;

#define GROW(arr, n, cap)                                                                          \
  do {                                                                                             \
    if ((n) >= (cap)) {                                                                            \
      (cap) = (cap) ? (cap) * 2 : 16;                                                              \
      (arr) = realloc((arr), sizeof *(arr) * (size_t)(unsigned)(cap));                                   \
    }                                                                                              \
  } while (0)

static char *dup_nonempty(const char *s) { return s && *s ? strdup(s) : NULL; }

static const char *str0(SEXP x, const char *def) {
  if (!Rf_isString(x) || XLENGTH(x) < 1 || STRING_ELT(x, 0) == NA_STRING) return def;
  return Rf_translateCharUTF8(STRING_ELT(x, 0));
}

static SEXP list_get(SEXP list, const char *name) {
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  for (R_xlen_t i = 0; i < XLENGTH(list); i++)
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) return VECTOR_ELT(list, i);
  return R_NilValue;
}

static long term_pid; // the process holding the program's terminal open (runInTerminal)

static void terminal_close(void) {
  if (term_pid > 0) kill((pid_t)term_pid, SIGTERM); // (the terminal stays, to read)
  term_pid = 0;
}

static void exit_now(int code) {
  terminal_close();
  capture_drain();
  R_CleanTempDir();
  _exit(code);
}

// ---------------------------------------------------------------------------------------------
// files, probes, breakpoints

typedef struct {
  char *path; // real path; for code without a file (a package's function): rdap://<name>
  int *probes, nprobes, cap;
  bool instrumented;
  char *text; // code without a file: its source (deparsed), for the `source` request
  char *name; // ... and its name
} File;

typedef struct {
  int file, line, col, end_line, end_col;
  int *bps, nbps, cap; // indexes into bps[]
  bool toplevel;       // before a script's top-level statement
} Probe;

typedef struct {
  int id, file, req_line, line, hits;
  bool live, verified;
  bool is_fn;  // a function breakpoint: on `fname`, at the probe `probe` (0: not placed yet)
  char *fname;
  int probe;
  char *condition, *hit_condition, *log_message;
  SEXP cond_expr; // `condition` parsed (preserved), on its first use
  bool cond_bad;  // it doesn't parse
} Bp;

static File *files;
static int nfiles, cap_files;
static Probe *probes; // by id (0: unused)
static int nprobes, cap_probes;
static Bp *bps; // id: index + 1
static int nbps, cap_bps;

static bool break_uncaught = true, break_warning = false, break_handled = false;
static bool handled_hooked, handled_dirty; // tryCatch() hooked (break_handled); to be (re)done
static SEXP cur_condition;                 // the condition stopped at (preserved), else NULL
static const char *cur_condition_kind;
static void handled_hook_apply(void);

int file_register(const char *path) {
  for (int i = 0; i < nfiles; i++)
    if (strcmp(files[i].path, path) == 0) return i;
  GROW(files, nfiles, cap_files);
  files[nfiles] = (File){.path = strdup(path)};
  return nfiles++;
}

int probe_new(int file, int line, int col, int end_line, int end_col) {
  if (nprobes == 0) {
    GROW(probes, nprobes, cap_probes);
    probes[nprobes++] = (Probe){0};
  }
  GROW(probes, nprobes, cap_probes);
  probes[nprobes] = (Probe){.file = file, .line = line, .col = col, .end_line = end_line, .end_col = end_col};
  File *f = &files[file];
  GROW(f->probes, f->nprobes, f->cap);
  f->probes[f->nprobes++] = nprobes;
  return nprobes++;
}

static void probe_add_bp(int id, int bi) {
  Probe *p = &probes[id];
  GROW(p->bps, p->nbps, p->cap);
  p->bps[p->nbps++] = bi;
}

void probe_set_toplevel(int id) { probes[id].toplevel = true; }

// the statements of the probes (by id), preserved
static SEXP probe_exprs;

void probe_set_expr(int id, SEXP statement) {
  if (!probe_exprs || id >= XLENGTH(probe_exprs)) {
    R_xlen_t n = probe_exprs ? XLENGTH(probe_exprs) : 0, cap = n ? n : 1024;
    while (cap <= id) cap *= 2;
    SEXP grown = PROTECT(Rf_allocVector(VECSXP, cap));
    for (R_xlen_t i = 0; i < n; i++) SET_VECTOR_ELT(grown, i, VECTOR_ELT(probe_exprs, i));
    R_PreserveObject(grown);
    if (probe_exprs) R_ReleaseObject(probe_exprs);
    probe_exprs = grown;
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(probe_exprs, id, statement);
}

// A breakpoint goes to the first line at or after its own that has a statement, but not out of
// the statement its line is in (a loop's closing brace isn't the next statement after the loop)
static void resolve(int bi) {
  Bp *b = &bps[bi];
  File *f = &files[b->file];
  int lo = 0, hi = INT_MAX; // the innermost statement around the line (but not starting on it)
  for (int k = 0; k < f->nprobes; k++) {
    Probe *p = &probes[f->probes[k]];
    if (p->line < b->req_line && p->end_line >= b->req_line && p->end_line - p->line < hi - lo) {
      lo = p->line;
      hi = p->end_line;
    }
  }
  int best = INT_MAX;
  for (int k = 0; k < f->nprobes; k++) {
    int line = probes[f->probes[k]].line;
    if (line >= b->req_line && line <= hi && line < best) best = line;
  }
  b->verified = best != INT_MAX;
  b->line = b->verified ? best : b->req_line;
  if (!b->verified) return;
  for (int k = 0; k < f->nprobes; k++)
    if (probes[f->probes[k]].line == best) probe_add_bp(f->probes[k], bi);
}

static void file_resolve(int file) {
  File *f = &files[file];
  for (int k = 0; k < f->nprobes; k++) probes[f->probes[k]].nbps = 0;
  for (int i = 0; i < nbps; i++) {
    if (!bps[i].live) continue;
    if (!bps[i].is_fn && bps[i].file == file) resolve(i);
    if (bps[i].is_fn && bps[i].probe > 0 && probes[bps[i].probe].file == file) probe_add_bp(bps[i].probe, i);
  }
}

static Json *source_json(int file) {
  Json *src = json_obj();
  if (files[file].text) { // no file: the source comes from the `source` request
    json_set(src, "name", json_str(files[file].name));
    json_set(src, "sourceReference", json_num(file + 1));
    json_set(src, "presentationHint", json_str("deemphasize"));
  } else {
    const char *slash = strrchr(files[file].path, '/');
    json_set(src, "name", json_str(slash ? slash + 1 : files[file].path));
    json_set(src, "path", json_str(files[file].path));
  }
  return src;
}

static Json *bp_json(const Bp *b) {
  Json *j = json_obj();
  json_set(j, "id", json_num(b->id));
  json_set(j, "verified", json_bool(b->verified));
  if (b->is_fn) {
    if (b->probe > 0) {
      json_set(j, "line", json_num(probes[b->probe].line));
      json_set(j, "source", source_json(probes[b->probe].file));
    } else {
      json_set(j, "message", json_str("Pending: set once the function exists"));
    }
    return j;
  }
  json_set(j, "line", json_num(b->line));
  if (!b->verified)
    json_set(j, "message", json_str(files[b->file].instrumented
                                        ? "No R code on this line (nor after it, in its block)"
                                        : "Pending: set once the file's code is loaded (run, sourced, "
                                          "or its package loaded with devtools::load_all())"));
  return j;
}

void file_instrumented(int file) {
  Json *src = source_json(file);
  Json *loaded = json_obj();
  json_set(loaded, "reason", json_str(files[file].instrumented ? "changed" : "new"));
  json_set(loaded, "source", src);
  dap_event("loadedSource", loaded);

  files[file].instrumented = true;
  file_resolve(file);
  for (int i = 0; i < nbps; i++) {
    if (!bps[i].live || bps[i].file != file) continue;
    Json *body = json_obj();
    json_set(body, "reason", json_str("changed"));
    json_set(body, "breakpoint", bp_json(&bps[i]));
    dap_event("breakpoint", body);
  }
}

static void set_breakpoints(const Json *req) {
  Json *args = json_get(req, "arguments");
  const char *path = json_get_str(json_get(args, "source"), "path", NULL);
  int ref = (int)json_get_num(json_get(args, "source"), "sourceReference", 0);
  int session_id;
  if (!ref && path) // nvim-dap names a source it got by reference dap-src://<session>/<reference>/...
    if (sscanf(path, "dap-src://%d/%d", &session_id, &ref) != 2) ref = 0;
  int file;
  if (ref > 0 && ref <= nfiles) {
    file = ref - 1;
  } else if (path) {
    char norm[PATH_MAX];
    if (!realpath(path, norm)) snprintf(norm, sizeof norm, "%s", path);
    file = file_register(norm);
  } else {
    dap_respond(req, false, "setBreakpoints: the source has no path", NULL);
    return;
  }
  for (int i = 0; i < nbps; i++) {
    if (!bps[i].live || bps[i].is_fn || bps[i].file != file) continue;
    bps[i].live = false;
    free(bps[i].condition);
    if (bps[i].cond_expr) R_ReleaseObject(bps[i].cond_expr);
    free(bps[i].hit_condition);
    free(bps[i].log_message);
  }
  int first = nbps;
  Json *list = json_get(args, "breakpoints");
  for (Json *it = list ? list->child : NULL; it; it = it->next) {
    GROW(bps, nbps, cap_bps);
    bps[nbps] = (Bp){
        .id = nbps + 1,
        .file = file,
        .req_line = (int)json_get_num(it, "line", 0),
        .live = true,
        .condition = dup_nonempty(json_get_str(it, "condition", NULL)),
        .hit_condition = dup_nonempty(json_get_str(it, "hitCondition", NULL)),
        .log_message = dup_nonempty(json_get_str(it, "logMessage", NULL)),
    };
    nbps++;
  }
  file_resolve(file);
  Json *out = json_arr();
  for (int i = first; i < nbps; i++) json_push(out, bp_json(&bps[i]));
  Json *body = json_obj();
  json_set(body, "breakpoints", out);
  dap_respond(req, true, NULL, body);
}

static void set_exception_breakpoints(const Json *req) {
  Json *filters = json_get(json_get(req, "arguments"), "filters");
  break_uncaught = break_warning = false;
  for (Json *f = filters ? filters->child : NULL; f; f = f->next) {
    if (f->type != J_STR) continue;
    if (strcmp(f->s, "uncaught") == 0) break_uncaught = true;
    if (strcmp(f->s, "warning") == 0) break_warning = true;
    if (strcmp(f->s, "handled") == 0) break_handled = true;
  }
  handled_dirty = break_handled != handled_hooked;
  if (handled_dirty && g_rdap_env && !g_in_eval && atomic_load(&g_state) != ST_RUNNING) handled_hook_apply();
  dap_respond(req, true, NULL, NULL);
}

// "5" (the 5th hit), "== 5", ">= 5", "> 5", "< 5", "<= 5", "% 5" (every 5th)
static bool hit_ok(const char *cond, int hits) {
  while (*cond == ' ') cond++;
  char op[3] = "==";
  if (*cond == '%' || *cond == '>' || *cond == '<' || *cond == '=') {
    op[0] = *cond++;
    op[1] = *cond == '=' ? *cond++ : 0;
    if (op[0] == '=' && op[1] == 0) op[1] = '=';
  }
  char *end;
  long n = strtol(cond, &end, 10);
  if (end == cond) return true; // not understood: ignored
  if (op[0] == '%') return n > 0 && hits % n == 0;
  if (op[0] == '>') return op[1] ? hits >= n : hits > n;
  if (op[0] == '<') return op[1] ? hits <= n : hits < n;
  return hits == n;
}

// ---------------------------------------------------------------------------------------------
// stopping

enum { STEP_NONE, STEP_INTO, STEP_OVER, STEP_OUT, STEP_INTO_TARGET };
static int step_target_probe; // STEP_INTO_TARGET: the first probe of the function to step into
static int cur_probe = -1;    // the probe stopped at (-1: none)
static SEXP cur_targets;      // what stepInTargets offered (preserved), else NULL
static int step_in_target;    // the target the stepIn request named (0: none)
static int step_mode;
static SEXP step_env; // preserved while stepping
static int step_depth;
static SEXP last_stop_env;
static int last_stop_line = -1, last_stop_probe = -1;
static int last_probe;       // the probe that ran last
static int last_top_probe;   // ... at the top level (of a script)
static SEXP last_probe_env;  // (only compared by address)
static long probes_since_resume;
static bool stop_on_entry, terminating;

static SEXP cur_frames; // the stack while stopped (from stack() in rdap.R), else NULL

typedef struct {
  SEXP obj;  // preserved
  SEXP base; // preserved: where the rows' expressions (evaluateName) are valid
  char *evalname;
} Handle;
static Handle *handles; // variablesReference: index + 1
static int nhandles, cap_handles;

static SEXP handles_alive; // a list holding the handles' objects (preserved), else NULL

static int handle_new(SEXP obj, const char *evalname, SEXP base) {
  if (!handles_alive || 2 * (nhandles + 1) > XLENGTH(handles_alive)) { // (grown: copied over)
    R_xlen_t n = handles_alive ? XLENGTH(handles_alive) : 0;
    SEXP grown = PROTECT(Rf_allocVector(VECSXP, n ? 2 * n : 256));
    for (R_xlen_t i = 0; i < n; i++) SET_VECTOR_ELT(grown, i, VECTOR_ELT(handles_alive, i));
    R_PreserveObject(grown);
    if (handles_alive) R_ReleaseObject(handles_alive);
    handles_alive = grown;
    UNPROTECT(1);
  }
  SET_VECTOR_ELT(handles_alive, 2 * nhandles, obj);
  SET_VECTOR_ELT(handles_alive, 2 * nhandles + 1, base);
  GROW(handles, nhandles, cap_handles);
  handles[nhandles] = (Handle){obj, base, evalname ? strdup(evalname) : NULL};
  return ++nhandles;
}

static void handles_reset(void) {
  for (int i = 0; i < nhandles; i++) free(handles[i].evalname);
  nhandles = 0;
  if (handles_alive) R_ReleaseObject(handles_alive);
  handles_alive = NULL;
}

// (further down)
static bool fbp_pending, fbp_dirty; // a function breakpoint isn't placed yet; a new one is
static int nwatches;
static void fbps_place(void);
static bool check_watches(int prev_probe, Json **hit_ids, char **text);
static void watches_refresh(void);
static void watches_hook(void);
static void set_memory_reference(Json *j, SEXP obj);
static bool evaluate_address_form(const Json *req, const char *expr, SEXP env);
static void step_into_prepare(int probe, SEXP env);
static void instrument_new_namespaces(void);
static int step_into_target(int target);

// number of function frames on the stack
static int current_depth(void) {
  static SEXP call;
  if (!call) {
    call = PROTECT(Rf_lang1(Rf_install("sys.calls")));
    call = Rf_lang2(Rf_install("length"), call);
    UNPROTECT(1);
    R_PreserveObject(call);
  }
  int err = 0;
  g_in_eval++;
  SEXP r = R_tryEvalSilent(call, R_BaseEnv, &err);
  g_in_eval--;
  return err ? 0 : Rf_asInteger(r);
}

static void handle_request_safe(Json *req);

enum { RESUME_NONE = -1, RESUME_CONTINUE, RESUME_INTO, RESUME_OVER, RESUME_OUT };

static int serve_stopped(void) {
  for (;;) {
    Json *m = dap_next(true);
    if (strcmp(json_get_str(m, "type", ""), "eof") == 0) exit_now(0);
    const char *cmd = json_get_str(m, "command", "");
    int resume = RESUME_NONE;
    if (terminating && strcmp(cmd, "disconnect") != 0) {
      dap_respond(m, false, "The program is terminating", NULL);
    } else if (strcmp(cmd, "continue") == 0) {
      Json *body = json_obj();
      json_set(body, "allThreadsContinued", json_bool(true));
      dap_respond(m, true, NULL, body);
      resume = RESUME_CONTINUE;
    } else if (strcmp(cmd, "next") == 0) {
      dap_respond(m, true, NULL, NULL);
      resume = RESUME_OVER;
    } else if (strcmp(cmd, "stepIn") == 0) {
      step_in_target = (int)json_get_num(json_get(m, "arguments"), "targetId", 0);
      dap_respond(m, true, NULL, NULL);
      resume = RESUME_INTO;
    } else if (strcmp(cmd, "stepOut") == 0) {
      dap_respond(m, true, NULL, NULL);
      resume = RESUME_OUT;
    } else {
      handle_request_safe(m);
    }
    json_free(m);
    if (resume != RESUME_NONE) return resume;
  }
}

// list(path, line, col) of a probe: where the top frame is when it stops there
static SEXP probe_top(int id) {
  SEXP top = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("path"));
  SET_STRING_ELT(names, 1, Rf_mkChar("line"));
  SET_STRING_ELT(names, 2, Rf_mkChar("col"));
  Rf_setAttrib(top, R_NamesSymbol, names);
  SET_VECTOR_ELT(top, 0, Rf_mkString(files[probes[id].file].path));
  SET_VECTOR_ELT(top, 1, Rf_ScalarInteger(probes[id].line));
  SET_VECTOR_ELT(top, 2, Rf_ScalarInteger(probes[id].col));
  UNPROTECT(2);
  return top;
}

static SEXP get_stack(SEXP top, bool exception) {
  SEXP exc = PROTECT(Rf_ScalarLogical(exception));
  SEXP frames = rhost_call("stack", 2, top, exc);
  UNPROTECT(1);
  return frames && TYPEOF(frames) == VECSXP ? frames : Rf_allocVector(VECSXP, 0);
}

// Stop: report it, answer requests until told to go on, then set up stepping. `top` says where the
// top frame is (see stack() in rdap.R), if its frame is `top_env` (NULL: no matter); the caller
// protects it. `probe`: the probe stopped at, or -1.
static void stop_here(const char *reason, SEXP top, SEXP top_env_expected, int probe, bool exception,
                      const char *description, const char *text, Json *hit_ids) {
  atomic_store(&g_state, ST_STOPPED);
  atomic_store(&g_pause, 0);
  if (step_env) R_ReleaseObject(step_env);
  step_env = NULL;
  step_mode = STEP_NONE;
  capture_drain();

  int depth = current_depth();
  SEXP frames = get_stack(top, exception);
  if (top_env_expected && XLENGTH(frames) > 0 && list_get(VECTOR_ELT(frames, 0), "env") != top_env_expected)
    frames = get_stack(R_NilValue, exception); // `top` is about another frame
  R_PreserveObject(frames);
  cur_frames = frames;
  SEXP top_env = R_GlobalEnv;
  int top_line = -1;
  if (XLENGTH(frames) > 0) {
    top_env = list_get(VECTOR_ELT(frames, 0), "env");
    top_line = Rf_asInteger(list_get(VECTOR_ELT(frames, 0), "line"));
  }

  Json *body = json_obj();
  json_set(body, "reason", json_str(reason));
  json_set(body, "threadId", json_num(1));
  json_set(body, "allThreadsStopped", json_bool(true));
  if (description) json_set(body, "description", json_str(description));
  if (text) json_set(body, "text", json_str(text));
  if (hit_ids) json_set(body, "hitBreakpointIds", hit_ids);
  dap_event("stopped", body);

  cur_probe = probe;
  step_in_target = 0;
  int resume = serve_stopped();
  watches_refresh();
  watches_hook();
  int target_probe = 0;
  if (resume == RESUME_INTO && step_in_target > 0) target_probe = step_into_target(step_in_target);
  else if (resume == RESUME_INTO && probe > 0) step_into_prepare(probe, top_env);
  if (cur_targets) R_ReleaseObject(cur_targets);
  cur_targets = NULL;
  cur_probe = -1;

  handles_reset();
  cur_frames = NULL;
  step_mode = resume == RESUME_INTO ? STEP_INTO : resume == RESUME_OVER ? STEP_OVER
            : resume == RESUME_OUT  ? STEP_OUT  : STEP_NONE;
  if (target_probe > 0) {
    step_mode = STEP_INTO_TARGET;
    step_target_probe = target_probe;
  }
  if (step_mode != STEP_NONE) {
    step_env = top_env;
    R_PreserveObject(step_env);
    step_depth = depth;
  }
  last_stop_env = top_env; // (kept alive by `frames` until here; only compared by address)
  last_stop_line = top_line;
  last_stop_probe = probe;
  probes_since_resume = 0;
  R_ReleaseObject(frames);
  atomic_store(&g_state, ST_RUNNING);
}

// the depth of the frame whose environment is rho: it doesn't change while the frame lives, so it's
// worked out once and kept on the environment (the global environment: every time, its code runs
// at different depths: a script's, a sourced file's)
static int frame_depth(SEXP rho) {
  static SEXP s_depth;
  if (!s_depth) s_depth = Rf_install(".rdap_depth");
  if (rho == R_GlobalEnv || TYPEOF(rho) != ENVSXP) return current_depth();
  SEXP d = Rf_getAttrib(rho, s_depth);
  if (TYPEOF(d) == INTSXP) return INTEGER(d)[0];
  int depth = current_depth();
  SEXP v = PROTECT(Rf_ScalarInteger(depth));
  Rf_setAttrib(rho, s_depth, v);
  UNPROTECT(1);
  return depth;
}

static bool step_matches(int id, SEXP rho) {
  switch (step_mode) {
  case STEP_INTO: return true;
  case STEP_INTO_TARGET: // into that function; else (the statement ran without calling it) as over
    if (id == step_target_probe) return true;
    // fall through
  case STEP_OVER:
    if (rho == step_env && rho != R_GlobalEnv) return true; // (a function's frame: one per call)
    {
      int d = frame_depth(rho);
      return (rho == step_env && d == step_depth) || d < step_depth;
    }
  case STEP_OUT: return frame_depth(rho) < step_depth;
  default: return false;
  }
}

static void console_msg(const char *fmt, const char *a, const char *b) {
  char msg[1024];
  int n = snprintf(msg, sizeof msg, fmt, a, b);
  dap_output("console", msg, n < 0 ? 0 : (size_t)n < sizeof msg ? (size_t)n : sizeof msg - 1);
}

// a breakpoint's condition (evaluated in the frame); one that fails stops (so it gets noticed)
static bool condition_holds(Bp *b, SEXP rho) {
  if (!b->cond_expr && !b->cond_bad) {
    ParseStatus status;
    SEXP text = PROTECT(Rf_mkString(b->condition));
    SEXP exprs = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
    if (status == PARSE_OK && XLENGTH(exprs) == 1) {
      b->cond_expr = VECTOR_ELT(exprs, 0);
      R_PreserveObject(b->cond_expr);
    } else {
      b->cond_bad = true;
      console_msg("rdap: the breakpoint condition `%s` isn't one R expression%s\n", b->condition, "");
    }
    UNPROTECT(2);
  }
  if (b->cond_bad) return true;
  int err = 0;
  g_in_eval++;
  atomic_fetch_add(&user_code, 1);
  SEXP r = R_tryEvalSilent(b->cond_expr, rho, &err);
  atomic_fetch_sub(&user_code, 1);
  g_in_eval--;
  if (err) {
    console_msg("rdap: the breakpoint condition `%s` failed: %s", b->condition, R_curErrorBuf());
    return true;
  }
  return Rf_asLogical(r) == TRUE;
}

// the breakpoints at a probe: stop (true), log, or nothing
static bool check_breakpoints(int id, SEXP rho, Json **hit_ids) {
  bool stop = false;
  for (int j = 0; j < probes[id].nbps; j++) {
    int bi = probes[id].bps[j];
    if (!bps[bi].live) continue;
    if (bps[bi].condition && !condition_holds(&bps[bi], rho)) continue;
    bps[bi].hits++;
    if (bps[bi].hit_condition && !hit_ok(bps[bi].hit_condition, bps[bi].hits)) continue;
    if (bps[bi].log_message) {
      SEXP msg = PROTECT(Rf_mkString(bps[bi].log_message));
      SEXP out = rhost_call("interpolate", 2, msg, rho);
      UNPROTECT(1);
      const char *s = str0(out, bps[bi].log_message);
      dap_output("console", s, strlen(s));
      dap_output("console", "\n", 1);
      continue;
    }
    stop = true;
    if (!*hit_ids) *hit_ids = json_arr();
    json_push(*hit_ids, json_num(bps[bi].id));
  }
  return stop;
}

// .External2(rdap_probe, <id>): before every instrumented statement
SEXP rdap_probe(SEXP call, SEXP op, SEXP args, SEXP rho) {
  (void)call;
  (void)op;
  if (g_in_eval || g_forked) return R_NilValue;
  int id = INTEGER(CADR(args))[0];
  int prev_probe = last_probe;
  last_probe = id;
  bool toplevel = probes[id].toplevel;
  if (toplevel) last_top_probe = id;
  last_probe_env = rho;
  probes_since_resume++;
  if (dap_pending()) debugger_service_running();

  // right after a stop, another statement on the same line doesn't stop again (the same one
  // does: a loop's next iteration)
  bool same_spot = probes_since_resume == 1 && rho == last_stop_env && probes[id].line == last_stop_line &&
                   id != last_stop_probe;
  if (fbp_pending && (fbp_dirty || toplevel)) fbps_place();
  if (handled_dirty) handled_hook_apply();
  if (toplevel) instrument_new_namespaces(); // (load_all() ran?)
  const char *reason = NULL;
  Json *hit_ids = NULL;
  char *watch_text = NULL;
  if (nwatches && check_watches(prev_probe, &hit_ids, &watch_text)) reason = "data breakpoint";
  if (!reason && !same_spot && probes[id].nbps && check_breakpoints(id, rho, &hit_ids)) reason = "breakpoint";
  if (!reason && !same_spot && step_mode != STEP_NONE && step_matches(id, rho)) reason = "step";
  if (!reason && atomic_load(&g_pause)) reason = "pause";
  if (!reason) return R_NilValue;

  SEXP top = PROTECT(probe_top(id));
  stop_here(reason, top, NULL, id, false, watch_text ? "Value changed" : NULL, watch_text, hit_ids);
  UNPROTECT(1);
  free(watch_text);
  return R_NilValue;
}

// .Call(rdap_exception, header, message, kind): an error/warning nothing handled
SEXP rdap_exception(SEXP header, SEXP message, SEXP kind, SEXP condition) {
  if (g_in_eval || g_forked || atomic_load(&g_state) != ST_RUNNING) return R_NilValue;
  const char *k = str0(kind, "");
  bool on = strcmp(k, "error") == 0 ? break_uncaught : strcmp(k, "handled") == 0 ? break_handled : break_warning;
  if (!on) return R_NilValue;
  cur_condition = condition;
  R_PreserveObject(condition);
  cur_condition_kind = strcmp(k, "error") == 0 ? "unhandled" : strcmp(k, "handled") == 0 ? "always" : "userUnhandled";
  stop_here("exception", R_NilValue, NULL, -1, true, str0(header, "Error"), str0(message, ""), NULL);
  R_ReleaseObject(condition);
  cur_condition = NULL;
  return R_NilValue;
}

// .Call(rdap_instrument_closures, fns, paths): functions loaded from files with source references
// (a package's, devtools::load_all()) get probes in place, in their files (paths: normalized)
SEXP rdap_instrument_closures(SEXP fns, SEXP paths) {
  SEXP s_srcref = Rf_install("srcref"), s_srcfile = Rf_install("srcfile");
  int *done = calloc((size_t)XLENGTH(fns) + 1, sizeof *done), ndone = 0;
  for (R_xlen_t i = 0; i < XLENGTH(fns); i++) {
    SEXP fn = VECTOR_ELT(fns, i);
    if (TYPEOF(fn) != CLOSXP) continue;
    SEXP path = PROTECT(Rf_ScalarString(STRING_ELT(paths, i)));
    int file = file_register(CHAR(STRING_ELT(paths, i)));
    SEXP ref = Rf_getAttrib(fn, s_srcref);
    SEXP srcfile = Rf_getAttrib(ref, s_srcfile);
    if (TYPEOF(srcfile) == ENVSXP) Rf_defineVar(Rf_install("rdap_path"), path, srcfile); // (stack())
    SEXP body = TYPEOF(BODY(fn)) == BCODESXP ? R_ClosureExpr(fn) : BODY(fn);
    SET_BODY(fn, instrument_body(body, ref, file));
    UNPROTECT(1);
    bool seen = false;
    for (int k = 0; k < ndone; k++) seen |= done[k] == file;
    if (!seen) done[ndone++] = file;
  }
  for (int k = 0; k < ndone; k++) file_instrumented(done[k]);
  free(done);
  return Rf_ScalarInteger(ndone);
}

// the namespaces loaded (which, and which environment each is): changes when a package is loaded
// (or reloaded: devtools::load_all())
static uintptr_t namespaces_signature(void) {
  static SEXP registry;
  if (!registry) {
    registry = rhost_call("namespace_registry", 0);
    if (!registry) return 0;
    R_PreserveObject(registry);
  }
  SEXP names = PROTECT(R_lsInternal3(registry, TRUE, FALSE));
  uintptr_t sig = (uintptr_t)XLENGTH(names);
  for (R_xlen_t i = 0; i < XLENGTH(names); i++)
    sig ^= (uintptr_t)Rf_findVarInFrame3(registry, Rf_installTrChar(STRING_ELT(names, i)), FALSE) * 2654435761u;
  UNPROTECT(1);
  return sig;
}

// packages loaded with source references since last time: their functions get probes
static void instrument_new_namespaces(void) {
  static uintptr_t last;
  uintptr_t sig = namespaces_signature();
  if (sig == last) return;
  last = sig;
  rhost_call("instrument_loaded", 0);
}

// .Call(rdap_swap_body, f, body): f gets `body`; returns its old one (to put back)
SEXP rdap_swap_body(SEXP fn, SEXP body) {
  if (TYPEOF(fn) != CLOSXP) Rf_error("rdap_swap_body: not a closure");
  SEXP old = BODY(fn);
  SET_BODY(fn, body);
  return old;
}

const char *debugger_browser_stop(void) {
  if (g_in_eval || g_forked || atomic_load(&g_state) != ST_RUNNING) return "c";
  // where browser() is: the statement whose probe ran last, if it ran in the browsed frame (R's
  // own srcref, outside the browser's context, is where that frame was called from)
  SEXP top = PROTECT(last_probe > 0 ? probe_top(last_probe) : R_NilValue);
  stop_here("breakpoint", top, last_probe > 0 ? last_probe_env : NULL, -1, false, "browser()", NULL, NULL);
  UNPROTECT(1);
  return "c"; // stepping goes on through the probes
}

// .Call(rdap_env_bindings, env): list(names, kinds, values), without forcing promises. kinds:
// 0 value, 1 unforced promise (value: its expression), 2 active binding, 3 missing, 4 `...`
SEXP rdap_env_bindings(SEXP env) {
  if (TYPEOF(env) != ENVSXP) Rf_error("rdap_env_bindings: not an environment");
  SEXP names = PROTECT(R_lsInternal3(env, TRUE, TRUE));
  R_xlen_t n = XLENGTH(names);
  SEXP kinds = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP values = PROTECT(Rf_allocVector(VECSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP sym = Rf_installTrChar(STRING_ELT(names, i));
    int kind = 0;
    SEXP v = R_NilValue;
    if (R_BindingIsActive(sym, env)) {
      kind = 2;
    } else {
      SEXP x = Rf_findVarInFrame3(env, sym, TRUE);
      if (x == R_UnboundValue) {
        kind = 2;
      } else if (TYPEOF(x) == PROMSXP) {
        if (PRVALUE(x) != R_UnboundValue) {
          v = PRVALUE(x);
        } else {
          kind = 1;
          v = R_PromiseExpr(x);
        }
      } else if (x == R_MissingArg) {
        kind = 3;
      } else if (TYPEOF(x) == DOTSXP) {
        kind = 4;
        v = Rf_ScalarInteger(Rf_length(x));
      } else {
        v = x;
      }
    }
    INTEGER(kinds)[i] = kind;
    SET_VECTOR_ELT(values, i, v);
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_VECTOR_ELT(out, 0, names);
  SET_VECTOR_ELT(out, 1, kinds);
  SET_VECTOR_ELT(out, 2, values);
  SET_STRING_ELT(out_names, 0, Rf_mkChar("names"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("kinds"));
  SET_STRING_ELT(out_names, 2, Rf_mkChar("values"));
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(5);
  return out;
}

// ---------------------------------------------------------------------------------------------
// requests about the stopped program

static SEXP frame_at(const Json *args, const char *key, bool required) {
  if (!cur_frames) return NULL;
  int id = (int)json_get_num(args, key, 0);
  if (id < 1 || id > XLENGTH(cur_frames)) return required || id != 0 ? NULL : R_NilValue;
  return VECTOR_ELT(cur_frames, id - 1);
}

static void stack_trace(const Json *req) {
  if (!cur_frames) {
    dap_respond(req, false, "The program isn't stopped", NULL);
    return;
  }
  Json *args = json_get(req, "arguments");
  int n = (int)XLENGTH(cur_frames);
  int start = (int)json_get_num(args, "startFrame", 0);
  int levels = (int)json_get_num(args, "levels", 0);
  int end = levels > 0 && start + levels < n ? start + levels : n;
  Json *arr = json_arr();
  for (int i = start; i < end; i++) {
    SEXP f = VECTOR_ELT(cur_frames, i);
    Json *j = json_push(arr, json_obj());
    json_set(j, "id", json_num(i + 1));
    json_set(j, "name", json_str(str0(list_get(f, "name"), "?")));
    int line = Rf_asInteger(list_get(f, "line"));
    int col = Rf_asInteger(list_get(f, "col"));
    json_set(j, "line", json_num(line > 0 ? line : 0));
    json_set(j, "column", json_num(col > 0 ? col : 0));
    const char *path = str0(list_get(f, "path"), NULL);
    if (path && line > 0) {
      json_set(j, "source", source_json(file_register(path)));
    } else {
      json_set(j, "presentationHint", json_str("subtle"));
    }
  }
  Json *body = json_obj();
  json_set(body, "stackFrames", arr);
  json_set(body, "totalFrames", json_num(n));
  dap_respond(req, true, NULL, body);
}

static Json *scope(const char *name, SEXP env, bool expensive, const char *hint) {
  Json *s = json_obj();
  json_set(s, "name", json_str(name));
  json_set(s, "variablesReference", json_num(handle_new(env, NULL, env)));
  json_set(s, "expensive", json_bool(expensive));
  if (hint) json_set(s, "presentationHint", json_str(hint));
  return s;
}

static void scopes(const Json *req) {
  SEXP f = frame_at(json_get(req, "arguments"), "frameId", true);
  if (!f) {
    dap_respond(req, false, "No such frame", NULL);
    return;
  }
  SEXP env = list_get(f, "env");
  Json *arr = json_arr();
  if (cur_condition && f == VECTOR_ELT(cur_frames, 0)) { // the condition stopped at
    SEXP view = rhost_call("condition_view", 1, cur_condition);
    if (view) {
      PROTECT(view);
      json_push(arr, scope("Exception", view, false, NULL));
      UNPROTECT(1);
    }
  }
  if (env != R_GlobalEnv) json_push(arr, scope("Locals", env, false, "locals"));
  json_push(arr, scope("Global", R_GlobalEnv, env != R_GlobalEnv, NULL));
  Json *body = json_obj();
  json_set(body, "scopes", arr);
  dap_respond(req, true, NULL, body);
}

static void variables(const Json *req) {
  int ref = (int)json_get_num(json_get(req, "arguments"), "variablesReference", 0);
  if (!cur_frames || ref < 1 || ref > nhandles) {
    // a reference from an earlier stop (a UI refreshing late): nothing there now, not an error
    Json *body = json_obj();
    json_set(body, "variables", json_arr());
    dap_respond(req, true, NULL, body);
    return;
  }
  Handle h = handles[ref - 1];
  SEXP en = PROTECT(h.evalname ? Rf_mkString(h.evalname) : R_NilValue);
  SEXP rows = rhost_call("children", 2, h.obj, en);
  PROTECT(rows ? rows : R_NilValue);
  Json *arr = json_arr();
  for (R_xlen_t i = 0; rows && TYPEOF(rows) == VECSXP && i < XLENGTH(rows); i++) {
    SEXP r = VECTOR_ELT(rows, i);
    Json *v = json_push(arr, json_obj());
    json_set(v, "name", json_str(str0(VECTOR_ELT(r, 0), "?")));
    json_set(v, "value", json_str(str0(VECTOR_ELT(r, 1), "")));
    json_set(v, "type", json_str(str0(VECTOR_ELT(r, 2), "")));
    const char *evalname = str0(VECTOR_ELT(r, 3), NULL);
    if (evalname) json_set(v, "evaluateName", json_str(evalname));
    SEXP child = VECTOR_ELT(r, 4);
    json_set(v, "variablesReference", json_num(child != R_NilValue ? handle_new(child, evalname, h.base) : 0));
    set_memory_reference(v, VECTOR_ELT(r, 5));
  }
  UNPROTECT(2);
  Json *body = json_obj();
  json_set(body, "variables", arr);
  dap_respond(req, true, NULL, body);
}

static void evaluate(const Json *req) {
  Json *args = json_get(req, "arguments");
  const char *expr = json_get_str(args, "expression", "");
  const char *context = json_get_str(args, "context", "repl");
  if (!cur_frames) {
    dap_respond(req, false, "Evaluating needs a stopped program", NULL);
    return;
  }
  SEXP f = frame_at(args, "frameId", false);
  if (!f) {
    dap_respond(req, false, "No such frame", NULL);
    return;
  }
  SEXP env = f != R_NilValue ? list_get(f, "env")
           : XLENGTH(cur_frames) > 0 ? list_get(VECTOR_ELT(cur_frames, 0), "env") : R_GlobalEnv;
  if (evaluate_address_form(req, expr, env)) return;
  SEXP ex = PROTECT(Rf_mkString(expr));
  SEXP ctx = PROTECT(Rf_mkString(context));
  atomic_fetch_add(&user_code, 1);
  SEXP r = rhost_call("evaluate", 3, ex, env, ctx);
  atomic_fetch_sub(&user_code, 1);
  PROTECT(r ? r : R_NilValue);
  if (!r) {
    dap_respond(req, false, "rdap: evaluating failed", NULL);
  } else if (Rf_asLogical(list_get(r, "ok")) != TRUE) {
    dap_respond(req, false, str0(list_get(r, "result"), "error"), NULL);
  } else {
    Json *body = json_obj();
    json_set(body, "result", json_str(str0(list_get(r, "result"), "")));
    json_set(body, "type", json_str(str0(list_get(r, "type"), "")));
    SEXP child = list_get(r, "child");
    json_set(body, "variablesReference", json_num(child != R_NilValue ? handle_new(child, expr, env) : 0));
    set_memory_reference(body, list_get(r, "obj"));
    dap_respond(req, true, NULL, body);
  }
  UNPROTECT(3);
}

// ---------------------------------------------------------------------------------------------
// setting values, completions

static const char *last_error(void) {
  static char buf[1024];
  snprintf(buf, sizeof buf, "%s", R_curErrorBuf());
  size_t n = strlen(buf);
  while (n && (buf[n - 1] == '\n' || buf[n - 1] == ' ')) buf[--n] = 0;
  return buf;
}

// respond with a value row from rdap.R (set_variable(), set_expression())
static void respond_row(const Json *req, SEXP row, const char *evalname, SEXP base) {
  Json *body = json_obj();
  json_set(body, "value", json_str(str0(VECTOR_ELT(row, 1), "")));
  json_set(body, "type", json_str(str0(VECTOR_ELT(row, 2), "")));
  SEXP child = VECTOR_ELT(row, 4);
  json_set(body, "variablesReference", json_num(child != R_NilValue ? handle_new(child, evalname, base) : 0));
  set_memory_reference(body, VECTOR_ELT(row, 5));
  dap_respond(req, true, NULL, body);
}

static void set_variable(const Json *req) {
  Json *args = json_get(req, "arguments");
  int ref = (int)json_get_num(args, "variablesReference", 0);
  if (!cur_frames || ref < 1 || ref > nhandles) {
    dap_respond(req, false, "The program isn't stopped", NULL);
    return;
  }
  Handle h = handles[ref - 1];
  SEXP en = PROTECT(h.evalname ? Rf_mkString(h.evalname) : R_NilValue);
  SEXP name = PROTECT(Rf_mkString(json_get_str(args, "name", "")));
  SEXP value = PROTECT(Rf_mkString(json_get_str(args, "value", "")));
  SEXP row = rhost_call("set_variable", 6, h.obj, en, h.base, name, value, h.base);
  if (!row) {
    dap_respond(req, false, last_error(), NULL);
  } else {
    PROTECT(row);
    respond_row(req, row, str0(VECTOR_ELT(row, 3), NULL), h.base);
    UNPROTECT(1);
  }
  UNPROTECT(3);
}

static SEXP frame_env_or_top(const Json *args) {
  SEXP f = frame_at(args, "frameId", false);
  if (!f) return NULL;
  return f != R_NilValue ? list_get(f, "env")
       : XLENGTH(cur_frames) > 0 ? list_get(VECTOR_ELT(cur_frames, 0), "env") : R_GlobalEnv;
}

static void set_expression(const Json *req) {
  Json *args = json_get(req, "arguments");
  SEXP env = cur_frames ? frame_env_or_top(args) : NULL;
  if (!env) {
    dap_respond(req, false, "The program isn't stopped", NULL);
    return;
  }
  const char *expr = json_get_str(args, "expression", "");
  SEXP ex = PROTECT(Rf_mkString(expr));
  SEXP value = PROTECT(Rf_mkString(json_get_str(args, "value", "")));
  SEXP row = rhost_call("set_expression", 3, ex, value, env);
  if (!row) {
    dap_respond(req, false, last_error(), NULL);
  } else {
    PROTECT(row);
    respond_row(req, row, expr, env);
    UNPROTECT(1);
  }
  UNPROTECT(2);
}

static bool columns_from_1 = true;

static void completions(const Json *req) {
  Json *args = json_get(req, "arguments");
  SEXP env = cur_frames ? frame_env_or_top(args) : R_GlobalEnv;
  if (!env || !g_rdap_env) env = R_GlobalEnv;
  const char *text = json_get_str(args, "text", "");
  int column = (int)json_get_num(args, "column", (double)strlen(text) + 1);
  if (!columns_from_1) column++;
  Json *targets = json_arr();
  if (g_rdap_env) {
    SEXP t = PROTECT(Rf_mkString(text));
    SEXP c = PROTECT(Rf_ScalarInteger(column));
    SEXP r = rhost_call("completions", 3, t, c, env);
    if (r) {
      PROTECT(r);
      int len = (int)strlen(str0(list_get(r, "token"), ""));
      SEXP items = list_get(r, "items");
      for (R_xlen_t i = 0; Rf_isString(items) && i < XLENGTH(items) && i < 500; i++) {
        const char *item = Rf_translateCharUTF8(STRING_ELT(items, i));
        Json *t = json_push(targets, json_obj());
        json_set(t, "label", json_str(item));
        json_set(t, "text", json_str(item));
        json_set(t, "start", json_num(column - len - (columns_from_1 ? 0 : 1)));
        json_set(t, "length", json_num(len));
      }
      UNPROTECT(1);
    }
    UNPROTECT(2);
  }
  Json *body = json_obj();
  json_set(body, "targets", targets);
  dap_respond(req, true, NULL, body);
}

// ---------------------------------------------------------------------------------------------
// function breakpoints: at the first statement of the function. A function that has no probes
// (a package's) is deparsed into a source of its own (rdap://<name>, shown through the `source`
// request), instrumented, and its body replaced in place, so every reference to it (a package's
// namespace, its exports, S3 tables) gets the instrumented one.

static int instrument_closure(SEXP fn, const char *name) {
  char path[512];
  snprintf(path, sizeof path, "rdap://%s", name);
  SEXP label = PROTECT(Rf_mkString(name));
  SEXP src = rhost_call("function_source", 2, fn, label);
  if (!src) {
    UNPROTECT(1);
    return -1;
  }
  PROTECT(src);
  int file = file_register(path);
  free(files[file].text);
  free(files[file].name);
  files[file].text = strdup(str0(list_get(src, "text"), ""));
  files[file].name = strdup(name);
  SEXP srcfile = list_get(src, "srcfile");
  if (TYPEOF(srcfile) == ENVSXP) {
    SEXP p = PROTECT(Rf_mkString(path));
    Rf_defineVar(Rf_install("rdap_path"), p, srcfile);
    UNPROTECT(1);
  }
  SEXP fun = PROTECT(instrument_call(list_get(src, "fun"), file));
  SEXP body = CADDR(fun);
  SET_BODY(fn, body);
  file_instrumented(file);
  int id = -1;
  if (TYPEOF(body) == LANGSXP && TYPEOF(CADR(body)) == LANGSXP && TYPEOF(CADDR(CADR(body))) == INTSXP)
    id = INTEGER(CADDR(CADR(body)))[0];
  UNPROTECT(3);
  return id;
}

// Stepping into a statement: the functions it calls (or, a UseMethod(), the method it calls) that
// have no probes get them now, so that the step goes into them.
static void step_into_prepare(int probe, SEXP env) {
  if (!probe_exprs || probe >= XLENGTH(probe_exprs)) return;
  SEXP targets = rhost_call("step_targets", 2, VECTOR_ELT(probe_exprs, probe), env);
  if (!targets || TYPEOF(targets) != VECSXP) return;
  PROTECT(targets);
  SEXP fns = list_get(targets, "fns"), labels = list_get(targets, "labels");
  for (R_xlen_t i = 0; i < XLENGTH(fns); i++)
    instrument_closure(VECTOR_ELT(fns, i), Rf_translateCharUTF8(STRING_ELT(labels, i)));
  UNPROTECT(1);
}

// stepInTargets: the functions the statement stopped at calls
static void step_in_targets(const Json *req) {
  SEXP f = frame_at(json_get(req, "arguments"), "frameId", true);
  Json *targets = json_arr();
  if (f && f == VECTOR_ELT(cur_frames, 0) && cur_probe > 0 && probe_exprs && cur_probe < XLENGTH(probe_exprs)) {
    SEXP all = PROTECT(Rf_ScalarLogical(TRUE));
    SEXP t = rhost_call("step_targets", 3, VECTOR_ELT(probe_exprs, cur_probe), list_get(f, "env"), all);
    UNPROTECT(1);
    if (t) {
      if (cur_targets) R_ReleaseObject(cur_targets);
      cur_targets = t;
      R_PreserveObject(t);
      SEXP labels = list_get(t, "labels");
      for (R_xlen_t i = 0; i < XLENGTH(labels); i++) {
        Json *j = json_push(targets, json_obj());
        json_set(j, "id", json_num((double)i + 1));
        char label[512];
        snprintf(label, sizeof label, "%s()", Rf_translateCharUTF8(STRING_ELT(labels, i)));
        json_set(j, "label", json_str(label));
      }
    }
  }
  Json *body = json_obj();
  json_set(body, "targets", targets);
  dap_respond(req, true, NULL, body);
}

// the first probe of the target `target` (stepInTargets), which gets probes if it has none; 0 if
// that can't be
static int step_into_target(int target) {
  if (!cur_targets) return 0;
  SEXP fns = list_get(cur_targets, "fns"), labels = list_get(cur_targets, "labels");
  if (target < 1 || target > XLENGTH(fns)) return 0;
  SEXP fn = VECTOR_ELT(fns, target - 1);
  SEXP e = rhost_call("entry_probe", 1, fn);
  int id = e ? Rf_asInteger(e) : NA_INTEGER;
  if (id == NA_INTEGER || id <= 0 || id >= nprobes)
    id = instrument_closure(fn, Rf_translateCharUTF8(STRING_ELT(labels, target - 1)));
  return id > 0 ? id : 0;
}

static bool fbp_place(int bi) {
  SEXP name = PROTECT(Rf_mkString(bps[bi].fname));
  SEXP fn = rhost_call("find_function", 1, name);
  UNPROTECT(1);
  if (!fn || fn == R_NilValue) return false;
  PROTECT(fn);
  SEXP e = rhost_call("entry_probe", 1, fn);
  int id = e ? Rf_asInteger(e) : NA_INTEGER;
  if (id == NA_INTEGER || id <= 0 || id >= nprobes) id = instrument_closure(fn, bps[bi].fname);
  UNPROTECT(1);
  if (id <= 0) return false;
  bps[bi].probe = id;
  bps[bi].verified = true;
  bps[bi].line = probes[id].line;
  probe_add_bp(id, bi);
  return true;
}

// places the function breakpoints not placed yet (the functions may exist now)
static void fbps_place(void) {
  fbp_pending = fbp_dirty = false;
  for (int i = 0; i < nbps; i++) {
    if (!bps[i].live || !bps[i].is_fn || bps[i].probe > 0) continue;
    if (!fbp_place(i)) {
      fbp_pending = true;
      continue;
    }
    Json *body = json_obj();
    json_set(body, "reason", json_str("changed"));
    json_set(body, "breakpoint", bp_json(&bps[i]));
    dap_event("breakpoint", body);
  }
}

static void set_function_breakpoints(const Json *req) {
  for (int i = 0; i < nbps; i++) {
    Bp *b = &bps[i];
    if (!b->live || !b->is_fn) continue;
    if (b->probe > 0) { // off its probe
      Probe *p = &probes[b->probe];
      int k = 0;
      for (int j = 0; j < p->nbps; j++)
        if (p->bps[j] != i) p->bps[k++] = p->bps[j];
      p->nbps = k;
    }
    b->live = false;
    free(b->fname);
    free(b->condition);
    free(b->hit_condition);
    if (b->cond_expr) R_ReleaseObject(b->cond_expr);
  }
  int first = nbps;
  Json *list = json_get(json_get(req, "arguments"), "breakpoints");
  for (Json *it = list ? list->child : NULL; it; it = it->next) {
    const char *name = json_get_str(it, "name", "");
    if (!*name) continue;
    GROW(bps, nbps, cap_bps);
    bps[nbps] = (Bp){
        .id = nbps + 1,
        .file = -1,
        .live = true,
        .is_fn = true,
        .fname = strdup(name),
        .condition = dup_nonempty(json_get_str(it, "condition", NULL)),
        .hit_condition = dup_nonempty(json_get_str(it, "hitCondition", NULL)),
    };
    nbps++;
  }
  // placed now if R can run code now; else at the next probe
  bool can_eval = g_rdap_env && !g_in_eval && atomic_load(&g_state) != ST_RUNNING;
  for (int i = first; i < nbps; i++)
    if (!(can_eval && fbp_place(i))) fbp_pending = fbp_dirty = true;
  Json *out = json_arr();
  for (int i = first; i < nbps; i++) json_push(out, bp_json(&bps[i]));
  Json *body = json_obj();
  json_set(body, "breakpoints", out);
  dap_respond(req, true, NULL, body);
}

// ---------------------------------------------------------------------------------------------
// watchpoints (data breakpoints): a variable (a binding in an environment) that changes stops
// the program at the next statement. Each probe compares the binding with a copy (identical()),
// so changes in place are seen too.

typedef struct {
  char *name;    // the variable, or the expression
  SEXP env, sym; // env preserved
  SEXP expr;     // an expression (preserved; evaluated in env), else NULL: the variable sym
  char *frame;   // env is the frame of this call ("my_func()"): the watch ends when it returns
  bool hooked;   // ... and rdap will hear of that (an on.exit() in the frame)
  bool ended;    // ... and it returned
} WatchTarget;   // what dataBreakpointInfo described: dataId "w<index>"
static WatchTarget *targets;
static int ntargets, cap_targets;

typedef struct {
  int id, target;
  SEXP copy; // preserved: the value; NULL: unbound; an unforced promise: an argument not evaluated yet
  SEXP cond; // a condition (parsed, preserved) a change has to meet, or NULL
  char *hit_condition;
  int hits;
} Watch;
static Watch *watches;
static int cap_watches, next_watch_id = 1000000;

// what a binding holds, without running anything: NULL if unbound or an active binding; an
// argument not evaluated yet: its promise
static SEXP binding_value(SEXP env, SEXP sym) {
  if (!R_existsVarInFrame(env, sym) || R_BindingIsActive(sym, env)) return NULL; // (the latter errs if unbound)
  SEXP x = Rf_findVarInFrame3(env, sym, TRUE);
  if (x == R_UnboundValue) return NULL;
  if (TYPEOF(x) == PROMSXP && PRVALUE(x) != R_UnboundValue) return PRVALUE(x);
  return x;
}

// a watched expression's value (NULL if it fails: an element not there, say)
static SEXP target_value(WatchTarget *t) {
  if (!t->expr) return binding_value(t->env, t->sym);
  int err = 0;
  g_in_eval++;
  SEXP v = R_tryEvalSilent(t->expr, t->env, &err);
  g_in_eval--;
  return err ? NULL : v;
}

static SEXP preserved_copy(SEXP x) {
  if (!x) return NULL;
  if (TYPEOF(x) != PROMSXP) x = Rf_duplicate(x); // (a promise: kept, to see it evaluated)
  R_PreserveObject(x);
  return x;
}

static void watches_refresh(void) {
  for (int i = 0; i < nwatches; i++) {
    if (watches[i].copy) R_ReleaseObject(watches[i].copy);
    watches[i].copy = preserved_copy(target_value(&targets[watches[i].target]));
  }
}

// on resuming (from a probe, inside the program's frames): the frames of watched variables get an
// on.exit() that tells rdap they returned
static void watches_hook(void) {
  for (int i = 0; i < nwatches; i++) {
    WatchTarget *t = &targets[watches[i].target];
    if (!t->frame || t->hooked || t->ended) continue;
    t->hooked = true;
    SEXP fn = Rf_findVarInFrame(g_rdap_env, Rf_install("scope_end"));
    SEXP id = PROTECT(Rf_ScalarInteger(watches[i].target));
    SEXP inner = PROTECT(Rf_lang2(fn, id));
    SEXP yes = PROTECT(Rf_ScalarLogical(TRUE));
    SEXP call = PROTECT(Rf_lang3(Rf_install("on.exit"), inner, yes));
    SET_TAG(CDDR(call), Rf_install("add"));
    Rf_eval(call, t->env); // (not R_tryEval: on.exit() looks for the frame up to the top level)
    UNPROTECT(4);
  }
}

// .Call(rdap_scope_end, target): a watched variable's frame returned
SEXP rdap_scope_end(SEXP target) {
  int t = Rf_asInteger(target);
  if (t >= 0 && t < ntargets) targets[t].ended = true;
  return R_NilValue;
}

static const char *summary(SEXP x, char *buf, size_t n) {
  if (!x) return "<unbound>";
  if (TYPEOF(x) == PROMSXP) return "<not evaluated yet>";
  SEXP s = rhost_call("summary1", 1, x);
  snprintf(buf, n, "%s", str0(s, "?"));
  return buf;
}

static void add_text(char **text, const char *msg) {
  if (*text) { // several at once
    char *joined;
    if (asprintf(&joined, "%s; %s", *text, msg) < 0) joined = NULL;
    free(*text);
    *text = joined;
  } else {
    *text = strdup(msg);
  }
}

// the watches whose variable changed (the change: in the statements after the probe `prev_probe`),
// or went out of scope
static bool check_watches(int prev_probe, Json **hit_ids, char **text) {
  bool hit = false;
  for (int i = 0; i < nwatches; i++) {
    WatchTarget *t = &targets[watches[i].target];
    char a[256], b[256], msg[700];
    if (t->ended) { // (gdb: "Watchpoint 2 deleted because the program has left the block...")
      snprintf(msg, sizeof msg, "%s went out of scope (%s returned): no longer watched", t->name, t->frame);
      add_text(text, msg);
      if (!*hit_ids) *hit_ids = json_arr();
      json_push(*hit_ids, json_num(watches[i].id));
      Json *bp = json_obj();
      json_set(bp, "id", json_num(watches[i].id));
      json_set(bp, "verified", json_bool(false));
      Json *body = json_obj();
      json_set(body, "reason", json_str("removed"));
      json_set(body, "breakpoint", bp);
      dap_event("breakpoint", body);
      if (watches[i].copy) R_ReleaseObject(watches[i].copy);
      if (watches[i].cond) R_ReleaseObject(watches[i].cond);
      free(watches[i].hit_condition);
      watches[i--] = watches[--nwatches];
      hit = true;
      continue;
    }
    SEXP cur = target_value(t);
    SEXP old = watches[i].copy;
    if (!old && !cur) continue;
    if (old && TYPEOF(old) == PROMSXP) {
      if (cur == old) continue; // not evaluated yet
      if (Rf_findVarInFrame3(t->env, t->sym, TRUE) == old) { // evaluated now: not a change
        R_ReleaseObject(old);
        watches[i].copy = preserved_copy(cur);
        continue;
      }
    } else if (old && cur && TYPEOF(cur) != PROMSXP && R_compute_identical(old, cur, 16)) {
      continue;
    }
    // a change: does it count? (condition, hit count)
    bool counts = true;
    if (watches[i].cond) {
      int err = 0;
      g_in_eval++;
      SEXP r = R_tryEvalSilent(watches[i].cond, t->env, &err);
      g_in_eval--;
      counts = err || Rf_asLogical(r) == TRUE;
    }
    if (counts && watches[i].hit_condition) counts = hit_ok(watches[i].hit_condition, ++watches[i].hits);
    else if (counts) watches[i].hits++;
    if (!counts) {
      if (old) R_ReleaseObject(old);
      watches[i].copy = preserved_copy(cur);
      continue;
    }
    char where[64] = "";
    if (prev_probe > 0) snprintf(where, sizeof where, " (line %d)", probes[prev_probe].line);
    snprintf(msg, sizeof msg, "%s changed%s: %s -> %s", t->name, where, summary(old, a, sizeof a),
             summary(cur, b, sizeof b));
    add_text(text, msg);
    if (old) R_ReleaseObject(old);
    watches[i].copy = preserved_copy(cur);
    if (!*hit_ids) *hit_ids = json_arr();
    json_push(*hit_ids, json_num(watches[i].id));
    hit = true;
  }
  return hit;
}

// .Call(rdap_program_end): the script's last statement ran; there are no probes after it, so the
// watches are checked here
// watches are checked here; and stepping stops here once more, to see the program's final state
// (as gdb does at the end of main)
SEXP rdap_program_end(void) {
  if (g_in_eval || g_forked || atomic_load(&g_state) != ST_RUNNING) return R_NilValue;
  Json *hit_ids = NULL;
  char *text = NULL;
  const char *reason = NULL, *description = NULL;
  if (nwatches && check_watches(last_probe, &hit_ids, &text)) {
    reason = "data breakpoint";
    description = "Value changed";
  } else if (step_mode != STEP_NONE) {
    reason = "step";
    description = "End of the script";
    text = strdup("The script has run to its end: step or continue to exit");
  }
  if (reason) {
    SEXP top = PROTECT(last_top_probe > 0 ? probe_top(last_top_probe) : R_NilValue);
    if (last_top_probe > 0) // (where: the end of the last statement)
      SET_VECTOR_ELT(top, 1, Rf_ScalarInteger(probes[last_top_probe].end_line));
    stop_here(reason, top, NULL, -1, false, description, text, hit_ids);
    UNPROTECT(1);
  }
  free(text);
  return R_NilValue;
}

static bool is_name(const char *s) {
  if (!*s || !(isalpha((unsigned char)*s) || *s == '.')) return false;
  for (; *s; s++)
    if (!(isalnum((unsigned char)*s) || *s == '.' || *s == '_')) return false;
  return true;
}

static void data_breakpoint_info(const Json *req) {
  Json *args = json_get(req, "arguments");
  const char *name = json_get_str(args, "name", "");
  int ref = (int)json_get_num(args, "variablesReference", 0);
  SEXP env = NULL, expr = NULL;
  const char *expr_text = NULL;
  SEXP keep = PROTECT(Rf_allocVector(VECSXP, 2)); // (expr_text's R string, expr)
  if (cur_frames && ref > 0 && ref <= nhandles && TYPEOF(handles[ref - 1].obj) != ENVSXP) {
    // a member of a value (l's a): its expression (l$a), where the value's expressions are valid
    Handle h = handles[ref - 1];
    SEXP en = PROTECT(h.evalname ? Rf_mkString(h.evalname) : R_NilValue);
    SEXP n = PROTECT(Rf_mkString(name));
    SEXP e = rhost_call("child_expr", 3, h.obj, en, n);
    UNPROTECT(2);
    if (e && Rf_isString(e)) {
      SET_VECTOR_ELT(keep, 0, e);
      expr_text = str0(e, NULL);
      env = h.base;
    }
  } else if (cur_frames && !is_name(name) && *name) { // an expression (the watches' l$a)
    SEXP fenv = frame_env_or_top(args);
    SEXP n = PROTECT(Rf_mkString(name));
    SEXP w = fenv ? rhost_call("watchable", 1, n) : NULL;
    UNPROTECT(1);
    if (w && Rf_asLogical(list_get(w, "ok")) == TRUE) {
      SET_VECTOR_ELT(keep, 0, Rf_mkString(name));
      expr_text = name;
      env = fenv;
      const char *root = str0(list_get(w, "root"), NULL);
      if (root) { // where its variable is (a global's expression isn't a frame's)
        SEXP r = PROTECT(Rf_mkString(root));
        SEXP where = rhost_call("where_bound", 2, r, fenv);
        UNPROTECT(1);
        if (where && TYPEOF(where) == ENVSXP) env = where;
      }
    }
  }
  if (expr_text) {
    ParseStatus status;
    SEXP exprs = PROTECT(R_ParseVector(VECTOR_ELT(keep, 0), -1, &status, R_NilValue));
    if (status == PARSE_OK && XLENGTH(exprs) == 1) {
      expr = VECTOR_ELT(exprs, 0);
      SET_VECTOR_ELT(keep, 1, expr);
    } else {
      env = NULL;
    }
    UNPROTECT(1);
  } else if (cur_frames && is_name(name)) {
    if (ref > 0 && ref <= nhandles) {
      if (TYPEOF(handles[ref - 1].obj) == ENVSXP) env = handles[ref - 1].obj;
    } else {
      SEXP fenv = frame_env_or_top(args);
      if (fenv) {
        SEXP n = PROTECT(Rf_mkString(name));
        SEXP w = rhost_call("where_bound", 2, n, fenv);
        UNPROTECT(1);
        env = w && TYPEOF(w) == ENVSXP ? w : fenv; // not bound yet: where it would be assigned
      }
    }
  }
  Json *body = json_obj();
  if (!env) {
    json_set(body, "dataId", json_null());
    json_set(body, "description",
             json_str("rdap can watch variables, and expressions that only read values (l$a, x[[2]], df$x[3])"));
    dap_respond(req, true, NULL, body);
    UNPROTECT(1);
    return;
  }
  R_PreserveObject(env);
  if (expr) R_PreserveObject(expr);
  GROW(targets, ntargets, cap_targets);
  targets[ntargets] = (WatchTarget){strdup(expr_text ? expr_text : name), env,
                                    expr ? NULL : Rf_install(name), expr, NULL, false, false};
  for (R_xlen_t i = 0; i < XLENGTH(cur_frames); i++) { // a function's frame?
    SEXP f = VECTOR_ELT(cur_frames, i);
    if (list_get(f, "env") == env && strcmp(str0(list_get(f, "kind"), ""), "function") == 0)
      targets[ntargets].frame = strdup(str0(list_get(f, "name"), "the function"));
  }
  char id[32];
  snprintf(id, sizeof id, "w%d", ntargets++);
  json_set(body, "dataId", json_str(id));
  json_set(body, "description", json_str(targets[ntargets - 1].name));
  Json *types = json_set(body, "accessTypes", json_arr());
  json_push(types, json_str("write"));
  json_set(body, "canPersist", json_bool(false));
  dap_respond(req, true, NULL, body);
  UNPROTECT(1);
}

static void set_data_breakpoints(const Json *req) {
  for (int i = 0; i < nwatches; i++) {
    if (watches[i].copy) R_ReleaseObject(watches[i].copy);
    if (watches[i].cond) R_ReleaseObject(watches[i].cond);
    free(watches[i].hit_condition);
  }
  nwatches = 0;
  Json *out = json_arr();
  Json *list = json_get(json_get(req, "arguments"), "breakpoints");
  for (Json *it = list ? list->child : NULL; it; it = it->next) {
    const char *data_id = json_get_str(it, "dataId", "");
    int t = data_id[0] == 'w' ? atoi(data_id + 1) : -1;
    Json *b = json_push(out, json_obj());
    if (t < 0 || t >= ntargets) {
      json_set(b, "verified", json_bool(false));
      json_set(b, "message", json_str("Unknown data breakpoint"));
      continue;
    }
    GROW(watches, nwatches, cap_watches);
    watches[nwatches] = (Watch){next_watch_id++, t, preserved_copy(target_value(&targets[t])), NULL,
                                dup_nonempty(json_get_str(it, "hitCondition", NULL)), 0};
    const char *cond = json_get_str(it, "condition", NULL);
    if (cond && *cond) {
      ParseStatus status;
      SEXP text = PROTECT(Rf_mkString(cond));
      SEXP exprs = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
      if (status == PARSE_OK && XLENGTH(exprs) == 1) {
        watches[nwatches].cond = VECTOR_ELT(exprs, 0);
        R_PreserveObject(watches[nwatches].cond);
      }
      UNPROTECT(2);
    }
    json_set(b, "id", json_num(watches[nwatches].id));
    json_set(b, "verified", json_bool(true));
    nwatches++;
  }
  Json *body = json_obj();
  json_set(body, "breakpoints", out);
  dap_respond(req, true, NULL, body);
}

// ---------------------------------------------------------------------------------------------
// memory: where an R value's data is (a vector's elements; other objects: the object itself)

static bool memory_of(SEXP x, uintptr_t *address, size_t *size) {
  size_t elt = 0;
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP: elt = 4; break;
  case REALSXP: elt = 8; break;
  case CPLXSXP: elt = 16; break;
  case RAWSXP: elt = 1; break;
  case STRSXP:
  case VECSXP:
  case EXPRSXP: elt = sizeof(SEXP); break;
  default: break;
  }
  if (elt && XLENGTH(x) > 0) {
    *address = (uintptr_t)DATAPTR_RO(x);
    *size = elt * (size_t)XLENGTH(x);
  } else {
    *address = (uintptr_t)x;
    *size = 56; // the object's header (64-bit)
  }
  return true;
}

static void set_memory_reference(Json *j, SEXP obj) {
  if (!obj || obj == R_NilValue) return;
  uintptr_t a;
  size_t n;
  if (!memory_of(obj, &a, &n)) return;
  char ref[32];
  snprintf(ref, sizeof ref, "0x%lx", (unsigned long)a);
  json_set(j, "memoryReference", json_str(ref));
}

// what a C debugger's memory view asks (dapper's, say): `(void *)&(x)`: where x's data is,
// `sizeof(x)`: its size, `(void *)(x)`: x as an address. None of these is R code.
static bool evaluate_address_form(const Json *req, const char *expr, SEXP env) {
  static const struct {
    const char *prefix;
    int kind;
  } forms[] = {{"(void *)&(", 0}, {"sizeof(", 1}, {"(void *)(", 2}};
  size_t len = strlen(expr);
  for (size_t f = 0; f < sizeof forms / sizeof *forms; f++) {
    size_t pl = strlen(forms[f].prefix);
    if (len <= pl || strncmp(expr, forms[f].prefix, pl) != 0 || expr[len - 1] != ')') continue;
    char *inner = strndup(expr + pl, len - pl - 1);
    SEXP code = PROTECT(Rf_mkString(inner));
    SEXP v = rhost_call("value_of", 2, code, env);
    free(inner);
    if (!v) {
      UNPROTECT(1);
      dap_respond(req, false, last_error(), NULL);
      return true;
    }
    PROTECT(v);
    uintptr_t a;
    size_t n;
    memory_of(v, &a, &n);
    if (forms[f].kind == 2 && (TYPEOF(v) == REALSXP || TYPEOF(v) == INTSXP) && XLENGTH(v) == 1)
      a = (uintptr_t)Rf_asReal(v); // a number: that address
    char result[64];
    if (forms[f].kind == 1) snprintf(result, sizeof result, "%zu", n);
    else snprintf(result, sizeof result, "0x%lx", (unsigned long)a);
    Json *body = json_obj();
    json_set(body, "result", json_str(result));
    json_set(body, "type", json_str(forms[f].kind == 1 ? "size_t" : "void *"));
    json_set(body, "variablesReference", json_num(0));
    if (forms[f].kind != 1) json_set(body, "memoryReference", json_str(result));
    dap_respond(req, true, NULL, body);
    UNPROTECT(2);
    return true;
  }
  return false;
}

static char *base64(const unsigned char *p, size_t n) {
  static const char tbl[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  char *out = malloc((n + 2) / 3 * 4 + 1), *o = out;
  for (size_t i = 0; i < n; i += 3) {
    unsigned v = (unsigned)p[i] << 16 | (i + 1 < n ? (unsigned)p[i + 1] << 8 : 0) | (i + 2 < n ? p[i + 2] : 0);
    *o++ = tbl[v >> 18 & 63];
    *o++ = tbl[v >> 12 & 63];
    *o++ = i + 1 < n ? tbl[v >> 6 & 63] : '=';
    *o++ = i + 2 < n ? tbl[v & 63] : '=';
  }
  *o = 0;
  return out;
}

static void read_memory(const Json *req) {
  Json *args = json_get(req, "arguments");
  uintptr_t a = (uintptr_t)strtoull(json_get_str(args, "memoryReference", "0"), NULL, 0) +
                (uintptr_t)(long)json_get_num(args, "offset", 0);
  size_t n = (size_t)json_get_num(args, "count", 0);
  if (n > (1 << 20)) n = 1 << 20;
  unsigned char *buf = malloc(n ? n : 1);
  size_t got = 0;
  long page = sysconf(_SC_PAGESIZE);
  while (got < n) { // (a page at a time: an unmapped one ends it, without a crash)
    size_t chunk = (size_t)page - (a + got) % (size_t)page;
    if (chunk > n - got) chunk = n - got;
    struct iovec local = {buf + got, chunk}, remote = {(void *)(a + got), chunk};
    ssize_t r = process_vm_readv(getpid(), &local, 1, &remote, 1, 0);
    if (r <= 0) break;
    got += (size_t)r;
  }
  char addr[32];
  snprintf(addr, sizeof addr, "0x%lx", (unsigned long)a);
  char *data = base64(buf, got);
  Json *body = json_obj();
  json_set(body, "address", json_str(addr));
  json_set(body, "data", json_str(data));
  json_set(body, "unreadableBytes", json_num((double)(n - got)));
  dap_respond(req, true, NULL, body);
  free(data);
  free(buf);
}

// the source of code without a file
static void source(const Json *req) {
  Json *args = json_get(req, "arguments");
  int ref = (int)json_get_num(args, "sourceReference", 0);
  if (ref <= 0) ref = (int)json_get_num(json_get(args, "source"), "sourceReference", 0);
  if (ref <= 0 || ref > nfiles || !files[ref - 1].text) {
    dap_respond(req, false, "No such source", NULL);
    return;
  }
  Json *body = json_obj();
  json_set(body, "content", json_str(files[ref - 1].text));
  json_set(body, "mimeType", json_str("text/x-r"));
  dap_respond(req, true, NULL, body);
}

// ---------------------------------------------------------------------------------------------
// the session

static char *program;
static char *package_root; // launch's `package` (or the package of a tests/testthat file): load_all()ed first
static bool testthat;      // the program is a testthat file
static bool client_runs_terminals; // (initialize: supportsRunInTerminalRequest)

// The program's terminal: the client opens one (runInTerminal) running a placeholder that says which
// it is (its tty, through a FIFO) and then just keeps it open. -1 if that doesn't work.
static int terminal_open(const char *cwd, const char *title) {
  char dir[] = "/tmp/rdap-XXXXXX";
  if (!mkdtemp(dir)) return -1;
  char fifo[sizeof dir + 8];
  snprintf(fifo, sizeof fifo, "%s/tty", dir);
  int fd = -1;
  if (mkfifo(fifo, 0600) != 0) goto out;
  int ff = open(fifo, O_RDWR | O_NONBLOCK); // (read-write: never blocks, nor sees an end)
  if (ff < 0) goto out;
  Json *args = json_obj();
  json_set(args, "kind", json_str("integrated"));
  json_set(args, "title", json_str(title));
  json_set(args, "cwd", json_str(cwd));
  Json *argv = json_set(args, "args", json_arr());
  json_push(argv, json_str("sh"));
  json_push(argv, json_str("-c"));
  json_push(argv, json_str("tty > \"$1\"; exec sleep 2147483647"));
  json_push(argv, json_str("rdap"));
  json_push(argv, json_str(fifo));
  int seq = dap_request("runInTerminal", args);
  Json *stash[64];
  int nstash = 0;
  Json *resp = NULL;
  for (int waited = 0; !resp && waited < 10000; waited += 100) {
    Json *m = dap_next_timeout(100);
    if (!m) continue;
    if (strcmp(json_get_str(m, "type", ""), "response") == 0 && (int)json_get_num(m, "request_seq", -1) == seq)
      resp = m;
    else if (nstash < 64)
      stash[nstash++] = m;
    else
      json_free(m);
  }
  while (nstash > 0) dap_unget(stash[--nstash]);
  if (resp && json_get_bool(resp, "success", false)) {
    Json *body = json_get(resp, "body");
    term_pid = (long)json_get_num(body, "processId", json_get_num(body, "shellProcessId", 0));
    struct pollfd p = {ff, POLLIN, 0};
    char tty[256] = "";
    if (poll(&p, 1, 5000) == 1) {
      ssize_t n = read(ff, tty, sizeof tty - 1);
      tty[n > 0 ? n : 0] = 0;
      tty[strcspn(tty, "\n")] = 0;
    }
    if (tty[0] == '/') fd = open(tty, O_RDWR | O_NOCTTY);
    if (fd < 0) terminal_close();
  }
  json_free(resp);
  close(ff);
out:
  unlink(fifo);
  rmdir(dir);
  return fd;
}

static void exception_info(const Json *req) {
  if (!cur_condition) {
    dap_respond(req, false, "Not stopped at an exception", NULL);
    return;
  }
  SEXP cls = PROTECT(Rf_getAttrib(cur_condition, R_ClassSymbol));
  SEXP view = PROTECT(rhost_call("condition_view", 1, cur_condition));
  SEXP text = rhost_call("stack_text", 1, cur_frames);
  PROTECT(text ? text : R_NilValue);
  char types[512] = "";
  for (R_xlen_t i = 0; Rf_isString(cls) && i < XLENGTH(cls); i++) {
    if (i) strncat(types, ", ", sizeof types - strlen(types) - 1);
    strncat(types, CHAR(STRING_ELT(cls, i)), sizeof types - strlen(types) - 1);
  }
  const char *message = view ? str0(list_get(view, "message"), "") : "";
  Json *body = json_obj();
  json_set(body, "exceptionId", json_str(Rf_isString(cls) && XLENGTH(cls) ? CHAR(STRING_ELT(cls, 0)) : "condition"));
  json_set(body, "description", json_str(message));
  json_set(body, "breakMode", json_str(cur_condition_kind));
  Json *details = json_set(body, "details", json_obj());
  json_set(details, "message", json_str(message));
  json_set(details, "typeName", json_str(types));
  json_set(details, "stackTrace", json_str(str0(text, "")));
  dap_respond(req, true, NULL, body);
  UNPROTECT(3);
}

// tryCatch() hooked or not, as the "handled" filter says (R code runs: at a stop, idle, or a probe)
static void handled_hook_apply(void) {
  handled_dirty = false;
  SEXP on = PROTECT(Rf_ScalarLogical(break_handled));
  SEXP ok = rhost_call("set_handled_hook", 1, on);
  UNPROTECT(1);
  if (ok && Rf_asLogical(ok) == TRUE) handled_hooked = break_handled;
}

static Json *capabilities(void) {
  Json *c = json_obj();
  json_set(c, "supportsConfigurationDoneRequest", json_bool(true));
  json_set(c, "supportsConditionalBreakpoints", json_bool(true));
  json_set(c, "supportsHitConditionalBreakpoints", json_bool(true));
  json_set(c, "supportsLogPoints", json_bool(true));
  json_set(c, "supportsEvaluateForHovers", json_bool(true));
  json_set(c, "supportsTerminateRequest", json_bool(true));
  json_set(c, "supportsSetVariable", json_bool(true));
  json_set(c, "supportsSetExpression", json_bool(true));
  json_set(c, "supportsCompletionsRequest", json_bool(true));
  Json *trig = json_set(c, "completionTriggerCharacters", json_arr());
  json_push(trig, json_str("$"));
  json_push(trig, json_str("@"));
  json_set(c, "supportsFunctionBreakpoints", json_bool(true));
  json_set(c, "supportsDataBreakpoints", json_bool(true));
  json_set(c, "supportsReadMemoryRequest", json_bool(true));
  Json *filters = json_set(c, "exceptionBreakpointFilters", json_arr());
  Json *f = json_push(filters, json_obj());
  json_set(f, "filter", json_str("uncaught"));
  json_set(f, "label", json_str("Uncaught errors"));
  json_set(f, "description", json_str("Stop where an error nothing handles is signalled"));
  json_set(f, "default", json_bool(true));
  f = json_push(filters, json_obj());
  json_set(f, "filter", json_str("warning"));
  json_set(f, "label", json_str("Warnings"));
  json_set(f, "description", json_str("Stop where a warning is signalled"));
  json_set(f, "default", json_bool(false));
  f = json_push(filters, json_obj());
  json_set(f, "filter", json_str("handled"));
  json_set(f, "label", json_str("Handled errors"));
  json_set(f, "description", json_str("Stop where an error a try() or tryCatch() will handle is signalled"));
  json_set(f, "default", json_bool(false));
  json_set(c, "supportsExceptionInfoRequest", json_bool(true));
  json_set(c, "supportsStepInTargetsRequest", json_bool(true));
  return c;
}

static void launch(const Json *req) {
  if (program) {
    dap_respond(req, false, "rdap debugs one program per session", NULL);
    return;
  }
  Json *a = json_get(req, "arguments");
  const char *prog = json_get_str(a, "program", NULL);
  const char *cwd = json_get_str(a, "cwd", NULL);
  char msg[PATH_MAX + 128], path[PATH_MAX], joined[PATH_MAX];
  if (!prog || !*prog) {
    dap_respond(req, false, "launch: `program` (the R script to debug) is missing", NULL);
    return;
  }
  if (prog[0] != '/' && cwd && *cwd) {
    snprintf(joined, sizeof joined, "%s/%s", cwd, prog);
    prog = joined;
  }
  if (!realpath(prog, path)) {
    snprintf(msg, sizeof msg, "launch: no such file: %s", prog);
    dap_respond(req, false, msg, NULL);
    return;
  }
  char dir[PATH_MAX];
  if (cwd && *cwd) {
    snprintf(dir, sizeof dir, "%s", cwd);
  } else {
    snprintf(dir, sizeof dir, "%s", path);
    *strrchr(dir, '/') = 0;
  }
  if (chdir(dir) != 0) {
    snprintf(msg, sizeof msg, "launch: can't change to the directory %s", dir);
    dap_respond(req, false, msg, NULL);
    return;
  }
  Json *env = json_get(a, "env");
  for (Json *e = env && env->type == J_OBJ ? env->child : NULL; e; e = e->next) {
    if (e->type == J_STR) setenv(e->key, e->s, 1);
    else if (e->type == J_NULL) unsetenv(e->key);
  }
  Json *args = json_get(a, "args");
  int nargs = 0;
  char **argv = calloc(json_len(args) + 1, sizeof *argv);
  for (Json *x = args && args->type == J_ARR ? args->child : NULL; x; x = x->next)
    if (x->type == J_STR) argv[nargs++] = x->s;
  stop_on_entry = json_get_bool(a, "stopOnEntry", false);
  const char *pkg = json_get_str(a, "package", NULL);
  char root[PATH_MAX];
  if (pkg && *pkg && realpath(pkg, root)) {
    package_root = strdup(root);
  } else if (strstr(path, "/tests/testthat/")) { // a test file: its package (up from tests/testthat)
    snprintf(root, sizeof root, "%s", path);
    *strstr(root, "/tests/testthat/") = 0;
    char desc[PATH_MAX + 16];
    snprintf(desc, sizeof desc, "%s/DESCRIPTION", root);
    if (access(desc, R_OK) == 0) {
      package_root = strdup(root);
      testthat = true;
    }
  }
  const char *console = json_get_str(a, "console", client_runs_terminals ? "integratedTerminal" : "internalConsole");
  if (strcmp(console, "internalConsole") != 0 && client_runs_terminals) {
    char title[PATH_MAX + 8];
    snprintf(title, sizeof title, "R: %s", strrchr(path, '/') + 1);
    int fd = terminal_open(dir, title);
    if (fd >= 0) rhost_set_terminal(fd);
    else dap_output("console", "rdap: no terminal for the program: its output goes here\n", 56);
  }

  char err[256];
  bool ok = rhost_init(nargs, argv, err, sizeof err);
  free(argv);
  if (!ok) {
    dap_respond(req, false, err, NULL);
    return;
  }
  program = strdup(path);
  dap_respond(req, true, NULL, NULL);
  dap_event("initialized", NULL);
}

static void run_program(void) {
  atomic_store(&g_state, ST_RUNNING);
  step_mode = stop_on_entry ? STEP_INTO : STEP_NONE;
  SEXP fn = Rf_findVarInFrame(g_rdap_env, Rf_install("run_file"));
  SEXP arg = PROTECT(Rf_mkString(program));
  SEXP pkg = PROTECT(package_root ? Rf_mkString(package_root) : R_NilValue);
  SEXP tt = PROTECT(Rf_ScalarLogical(testthat));
  SEXP call = PROTECT(Rf_lang4(fn, arg, pkg, tt));
  int err = 0;
  R_tryEval(call, R_GlobalEnv, &err); // (errors print)
  UNPROTECT(4);
  capture_drain();
  atomic_store(&g_state, ST_DONE);
  step_mode = STEP_NONE;
  rhost_terminal_write(err ? "\n[the program ended with an error]\n" : "\n[the program ended]\n");
  Json *body = json_obj();
  json_set(body, "exitCode", json_num(err ? 1 : 0));
  dap_event("exited", body);
  dap_event("terminated", NULL);
}

// where R was (the last statement that ran), for a crash report: no allocation (signal handler)
void debugger_crash_location(char *buf, size_t n) {
  if (last_probe <= 0 || last_probe >= nprobes) {
    snprintf(buf, n, "unknown");
    return;
  }
  const char *path = files[probes[last_probe].file].path;
  const char *slash = strrchr(path, '/');
  snprintf(buf, n, "%s:%d", slash ? slash + 1 : path, probes[last_probe].line);
}

void debugger_quit(int status) {
  capture_drain();
  Json *body = json_obj();
  json_set(body, "exitCode", json_num(status));
  dap_event("exited", body);
  dap_event("terminated", NULL);
  exit_now(status);
}

// requests answered the same in any state
static void handle_request(Json *req) {
  const char *cmd = json_get_str(req, "command", "");
  if (strcmp(cmd, "threads") == 0) {
    Json *body = json_obj();
    Json *t = json_push(json_set(body, "threads", json_arr()), json_obj());
    json_set(t, "id", json_num(1));
    json_set(t, "name", json_str("R"));
    dap_respond(req, true, NULL, body);
  } else if (strcmp(cmd, "setBreakpoints") == 0) {
    set_breakpoints(req);
  } else if (strcmp(cmd, "setExceptionBreakpoints") == 0) {
    set_exception_breakpoints(req);
  } else if (strcmp(cmd, "stackTrace") == 0) {
    stack_trace(req);
  } else if (strcmp(cmd, "scopes") == 0) {
    scopes(req);
  } else if (strcmp(cmd, "variables") == 0) {
    variables(req);
  } else if (strcmp(cmd, "evaluate") == 0) {
    evaluate(req);
  } else if (strcmp(cmd, "setVariable") == 0) {
    set_variable(req);
  } else if (strcmp(cmd, "setExpression") == 0) {
    set_expression(req);
  } else if (strcmp(cmd, "completions") == 0) {
    completions(req);
  } else if (strcmp(cmd, "setFunctionBreakpoints") == 0) {
    set_function_breakpoints(req);
  } else if (strcmp(cmd, "dataBreakpointInfo") == 0) {
    data_breakpoint_info(req);
  } else if (strcmp(cmd, "setDataBreakpoints") == 0) {
    set_data_breakpoints(req);
  } else if (strcmp(cmd, "readMemory") == 0) {
    read_memory(req);
  } else if (strcmp(cmd, "source") == 0) {
    source(req);
  } else if (strcmp(cmd, "exceptionInfo") == 0) {
    exception_info(req);
  } else if (strcmp(cmd, "stepInTargets") == 0) {
    step_in_targets(req);
  } else if (strcmp(cmd, "pause") == 0) {
    dap_respond(req, true, NULL, NULL);
  } else if (strcmp(cmd, "terminate") == 0) {
    dap_respond(req, true, NULL, NULL);
    if (atomic_load(&g_state) == ST_STOPPED) {
      terminating = true; // (exits on disconnect)
      dap_event("terminated", NULL);
    } else if (atomic_load(&g_state) != ST_DONE) {
      dap_event("terminated", NULL);
    }
  } else if (strcmp(cmd, "disconnect") == 0) {
    dap_respond(req, true, NULL, NULL);
    exit_now(0);
  } else if (strcmp(cmd, "continue") == 0 || strcmp(cmd, "next") == 0 || strcmp(cmd, "stepIn") == 0 ||
             strcmp(cmd, "stepOut") == 0) {
    dap_respond(req, false, "The program isn't stopped", NULL);
  } else {
    char msg[128];
    snprintf(msg, sizeof msg, "rdap doesn't support '%s'", cmd);
    dap_respond(req, false, msg, NULL);
  }
}

static void handle_request_cb(void *req) { handle_request(req); }

// an R error in handling a request (a bug) fails the request, not the session
static void handle_request_safe(Json *req) {
  if (!g_rdap_env) { // (R isn't running yet)
    handle_request(req);
  } else if (!R_ToplevelExec(handle_request_cb, req)) {
    char msg[1200];
    snprintf(msg, sizeof msg, "rdap: %s failed: %s", json_get_str(req, "command", "?"), last_error());
    dap_log("%s", msg);
    dap_respond(req, false, msg, NULL);
  }
}

void debugger_loop(void) {
  for (;;) {
    Json *m = dap_next(true);
    if (strcmp(json_get_str(m, "type", ""), "eof") == 0) exit_now(0);
    const char *cmd = json_get_str(m, "command", "");
    if (strcmp(cmd, "initialize") == 0) {
      columns_from_1 = json_get_bool(json_get(m, "arguments"), "columnsStartAt1", true);
      client_runs_terminals = json_get_bool(json_get(m, "arguments"), "supportsRunInTerminalRequest", false);
      dap_respond(m, true, NULL, capabilities());
    } else if (strcmp(cmd, "launch") == 0) {
      launch(m);
    } else if (strcmp(cmd, "attach") == 0) {
      dap_respond(m, false, "rdap launches programs; it can't attach to a running R", NULL);
    } else if (strcmp(cmd, "configurationDone") == 0) {
      if (!program || atomic_load(&g_state) != ST_IDLE) {
        dap_respond(m, false, "configurationDone: nothing to run", NULL);
      } else {
        dap_respond(m, true, NULL, NULL);
        json_free(m);
        run_program();
        continue;
      }
    } else {
      handle_request_safe(m);
    }
    json_free(m);
  }
}

// while R runs, handle what doesn't need it stopped (breakpoints, mostly)
void debugger_service_running(void) {
  if (g_in_eval || g_forked || atomic_load(&g_state) != ST_RUNNING) return;
  Json *m;
  while ((m = dap_next(false))) {
    if (strcmp(json_get_str(m, "type", ""), "eof") == 0) exit_now(0);
    handle_request_safe(m);
    json_free(m);
  }
}

// on the reader thread: what must work even while R is busy
bool debugger_reader_hook(Json *m) {
  int state = atomic_load(&g_state);
  if (strcmp(json_get_str(m, "type", ""), "eof") == 0) {
    if (state == ST_RUNNING) {
      terminal_close();
      _exit(0); // the client is gone, and R is busy
    }
    return false;
  }
  const char *cmd = json_get_str(m, "command", "");
  bool stopping = strcmp(cmd, "pause") == 0 || strcmp(cmd, "terminate") == 0 || strcmp(cmd, "disconnect") == 0;
  if (stopping && atomic_load(&user_code) > 0) kill(getpid(), SIGINT); // (R: an interrupt, as Ctrl-C)
  if (strcmp(cmd, "pause") == 0) {
    if (state == ST_RUNNING) atomic_store(&g_pause, 1);
    dap_respond(m, true, NULL, NULL);
    json_free(m);
    return true;
  }
  if (state == ST_RUNNING && (strcmp(cmd, "terminate") == 0 || strcmp(cmd, "disconnect") == 0)) {
    dap_respond(m, true, NULL, NULL);
    if (strcmp(cmd, "terminate") == 0) dap_event("terminated", NULL);
    json_free(m);
    terminal_close();
    _exit(0);
  }
  return false;
}
