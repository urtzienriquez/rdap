// Instrumenting parsed code: before every statement of every `{` (and of every function body, and
// every top-level expression) goes a probe, .External2(<rdap_probe>, <id>), whose id says where the
// statement is. The probe is where breakpoints, stepping and pausing stop the program. The
// statements keep their srcrefs (the probe gets its statement's), so printed functions, R's
// srcref tracking and tracebacks are unchanged.
#include "rdap.h"

static SEXP s_brace, s_function, s_srcref, s_srcfile, s_whole, s_dcolon, s_tcolon;
static SEXP fn_external2, probe_symbol;
static int cur_file;

// calls whose arguments are code as data: left alone
static const char *quoting[] = {"quote", "bquote", "expression", "substitute", "alist", "~",
                                "expr", "exprs", "quo", "quos", "enquo", "enquos", "Quote"};
static SEXP s_quoting[sizeof quoting / sizeof *quoting];

void instrument_init(SEXP sym) {
  probe_symbol = sym;
  R_PreserveObject(sym);
  fn_external2 = Rf_findFun(Rf_install(".External2"), R_BaseEnv);
  s_brace = Rf_install("{");
  s_function = Rf_install("function");
  s_srcref = Rf_install("srcref");
  s_srcfile = Rf_install("srcfile");
  s_whole = Rf_install("wholeSrcref");
  s_dcolon = Rf_install("::");
  s_tcolon = Rf_install(":::");
  for (size_t i = 0; i < sizeof quoting / sizeof *quoting; i++) s_quoting[i] = Rf_install(quoting[i]);
}

static bool is_quoting(SEXP head) {
  if (TYPEOF(head) == LANGSXP && (CAR(head) == s_dcolon || CAR(head) == s_tcolon) && Rf_length(head) == 3)
    head = CADDR(head); // rlang::expr
  if (TYPEOF(head) != SYMSXP) return false;
  for (size_t i = 0; i < sizeof s_quoting / sizeof *s_quoting; i++)
    if (head == s_quoting[i]) return true;
  return false;
}

static bool valid_srcref(SEXP ref) { return TYPEOF(ref) == INTSXP && XLENGTH(ref) >= 6; }

static SEXP mk_probe(SEXP ref) {
  const int *r = INTEGER(ref);
  int id = probe_new(cur_file, r[0], r[4], r[2], r[5]);
  SEXP sid = PROTECT(Rf_ScalarInteger(id));
  SEXP call = Rf_lang3(fn_external2, probe_symbol, sid);
  UNPROTECT(1);
  return call;
}

// { <probe for ref>; e }, with srcrefs
static SEXP probed_block(SEXP e, SEXP ref) {
  PROTECT(e);
  SEXP blk = PROTECT(Rf_allocLang(3));
  SETCAR(blk, s_brace);
  SETCADR(blk, mk_probe(ref));
  SETCADDR(blk, e);
  probe_set_expr(INTEGER(CADDR(CADR(blk)))[0], e);
  SEXP refs = PROTECT(Rf_allocVector(VECSXP, 3));
  for (int i = 0; i < 3; i++) SET_VECTOR_ELT(refs, i, ref);
  Rf_setAttrib(blk, s_srcref, refs);
  Rf_setAttrib(blk, s_srcfile, Rf_getAttrib(ref, s_srcfile));
  UNPROTECT(3);
  return blk;
}

static SEXP instr(SEXP e);

static SEXP instr_args(SEXP e) {
  for (SEXP a = CDR(e); a != R_NilValue; a = CDR(a))
    if (TYPEOF(CAR(a)) == LANGSXP) SETCAR(a, instr(CAR(a)));
  return e;
}

static SEXP instr_block(SEXP e) {
  SEXP refs = Rf_getAttrib(e, s_srcref);
  int n = Rf_length(e) - 1; // statements
  if (n <= 0 || TYPEOF(refs) != VECSXP || XLENGTH(refs) != n + 1) return instr_args(e);
  PROTECT(refs);
  SEXP out = PROTECT(Rf_allocLang(1 + 2 * n));
  SEXP out_refs = PROTECT(Rf_allocVector(VECSXP, 1 + 2 * n));
  SETCAR(out, s_brace);
  SET_VECTOR_ELT(out_refs, 0, VECTOR_ELT(refs, 0));
  SEXP p = CDR(out), s = CDR(e);
  for (int k = 1; k <= n; k++, s = CDR(s)) {
    SEXP ref = VECTOR_ELT(refs, k);
    SETCAR(p, valid_srcref(ref) ? mk_probe(ref) : R_NilValue);
    SET_VECTOR_ELT(out_refs, 2 * k - 1, ref);
    int id = valid_srcref(ref) ? INTEGER(CADDR(CAR(p)))[0] : 0;
    p = CDR(p);
    SETCAR(p, instr(CAR(s)));
    if (id) probe_set_expr(id, CAR(p));
    SET_VECTOR_ELT(out_refs, 2 * k, ref);
    p = CDR(p);
  }
  Rf_setAttrib(out, s_srcref, out_refs);
  Rf_setAttrib(out, s_srcfile, Rf_getAttrib(e, s_srcfile));
  Rf_setAttrib(out, s_whole, Rf_getAttrib(e, s_whole));
  UNPROTECT(3);
  return out;
}

// function(<formals>, <body>, <srcref>)
static SEXP instr_function(SEXP e) {
  SEXP body_cell = CDR(e) != R_NilValue ? CDDR(e) : R_NilValue;
  if (body_cell == R_NilValue) return e;
  SEXP body = CAR(body_cell);
  SEXP ref = CDR(body_cell) != R_NilValue ? CADR(body_cell) : R_NilValue;
  if (TYPEOF(body) == LANGSXP && CAR(body) == s_brace) {
    SETCAR(body_cell, instr(body));
  } else if (valid_srcref(ref)) { // function(x) x + 1: { <probe>; x + 1 }
    SETCAR(body_cell, probed_block(instr(body), ref));
  } else if (TYPEOF(body) == LANGSXP) {
    SETCAR(body_cell, instr(body));
  }
  return e;
}

static SEXP instr(SEXP e) {
  if (TYPEOF(e) != LANGSXP) return e;
  SEXP head = CAR(e);
  if (is_quoting(head)) return e;
  if (head == s_brace) return instr_block(e);
  if (head == s_function) return instr_function(e);
  if (TYPEOF(head) == LANGSXP) SETCAR(e, instr(head)); // (function(x) {...})(1)
  return instr_args(e);
}

SEXP instrument_exprs(SEXP exprs, int file) {
  cur_file = file;
  R_xlen_t n = XLENGTH(exprs);
  SEXP refs = PROTECT(Rf_getAttrib(exprs, s_srcref));
  SEXP out = PROTECT(Rf_allocVector(EXPRSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP e = instr(VECTOR_ELT(exprs, i));
    SEXP ref = TYPEOF(refs) == VECSXP && i < XLENGTH(refs) ? VECTOR_ELT(refs, i) : R_NilValue;
    SEXP blk = valid_srcref(ref) ? probed_block(e, ref) : e;
    SET_VECTOR_ELT(out, i, blk);
    if (blk != e) probe_set_toplevel(INTEGER(CADDR(CADR(blk)))[0]); // (a script's statement)
  }
  UNPROTECT(2);
  return out;
}

// a closure's body (with srcrefs: a function loaded from a file), instrumented; its probes in `file`
SEXP instrument_body(SEXP body, SEXP fn_srcref, int file) {
  cur_file = file;
  if (TYPEOF(body) == LANGSXP && CAR(body) == s_brace) return instr(body);
  if (valid_srcref(fn_srcref)) return probed_block(instr(body), fn_srcref); // function(x) x + 1
  return instr(body);
}

// one parsed expression (a function(...) call, say), instrumented in place; its probes in `file`
SEXP instrument_call(SEXP e, int file) {
  cur_file = file;
  return instr(e);
}

// .Call(rdap_instrument, <parsed file>, <its normalized path>)
SEXP rdap_instrument(SEXP exprs, SEXP path) {
  if (TYPEOF(exprs) != EXPRSXP || !Rf_isString(path) || XLENGTH(path) != 1)
    Rf_error("rdap_instrument: expected parsed code and a path");
  int file = file_register(CHAR(STRING_ELT(path, 0)));
  SEXP srcfile = Rf_getAttrib(exprs, s_srcfile);
  if (TYPEOF(srcfile) == ENVSXP) Rf_defineVar(Rf_install("rdap_path"), path, srcfile); // (stack())
  SEXP out = PROTECT(instrument_exprs(exprs, file));
  file_instrumented(file);
  UNPROTECT(1);
  return out;
}
