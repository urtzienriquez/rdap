# rdap's R side. Evaluated into a private environment (parent: the base environment) when R
# starts; rdap's C code calls these functions, and the program being debugged reaches the C
# routines through them.

self <- environment()

C_instrument <- NULL
C_exception <- NULL
C_env_bindings <- NULL
C_scope_end <- NULL
C_swap_body <- NULL
C_instrument_closures <- NULL
C_program_end <- NULL

init <- function() {
  sym <- function(name) getNativeSymbolInfo(name, "(embedding)")
  C_instrument <<- sym("rdap_instrument")
  C_exception <<- sym("rdap_exception")
  C_env_bindings <<- sym("rdap_env_bindings")
  C_scope_end <<- sym("rdap_scope_end")
  C_swap_body <<- sym("rdap_swap_body")
  C_instrument_closures <<- sym("rdap_instrument_closures")
  C_program_end <<- sym("rdap_program_end")
  # warnings print as they happen (nothing prints the deferred ones: there's no REPL); code the
  # program parses itself keeps its source references
  options(warn = 1L, keep.source = TRUE)
  tools <- new.env(parent = emptyenv())
  tools$source <- source
  attach(tools, name = "rdap:tools", pos = 2L, warn.conflicts = FALSE)
  invisible(NULL)
}

# what instrumented code calls (through .External2)
probe_symbol <- function() getNativeSymbolInfo("rdap_probe", "(embedding)")$address

# ------------------------------------------------------------------------------------------------
# running code

# Runs a script like Rscript does (visible values are printed). `.rdap_script` marks the frame as
# a script's (see stack()); errors that nothing handles reach on_error() with the stack intact.
run_file <- function(path, package = NULL, testthat = FALSE) {
  document <- !is.null(document_kind(path))
  if (!document) .rdap_script <- path # (a document's frame: run_document()'s)
  .rdap_envir <- globalenv()
  if (!is.null(package)) load_package(package, testthat)
  if (testthat) suppressPackageStartupMessages(library(testthat))
  code <- if (!document) .Call(C_instrument, parse(path, keep.source = TRUE), path)
  withCallingHandlers(
    if (document) run_document(path) else for (e in code) {
      r <- withVisible(eval(e, .rdap_envir))
      if (r$visible) print(r$value)
    },
    error = on_error,
    warning = on_warning
  )
  .Call(C_program_end) # (watchpoints: the last statement's changes)
  invisible(NULL)
}

# ------------------------------------------------------------------------------------------------
# documents: Rnw (knitrmini, knitr), R Markdown, Quarto. Their R chunks run in order, as knitting
# them would (eval, engine, error, child, ref.label and <<label>> references); breakpoints and the
# stack are in the document's own lines (the code is parsed from the document with everything else
# blanked). Nothing is woven: no output document.

document_kind <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("rnw", "snw")) "rnw" else if (ext %in% c("rmd", "qmd", "rmarkdown")) "md" else NULL
}

# knitr's chunk header: "label, opt = value, ..." (values left unevaluated)
parse_params <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) return(list(label = NULL, opts = list()))
  first <- sub(",.*$", "", text)
  label <- NULL
  if (!grepl("=", first)) {
    label <- gsub("^[\"']|[\"']$", "", trimws(first))
    text <- if (grepl(",", text)) sub("^[^,]*,", "", text) else ""
  }
  opts <- if (nzchar(trimws(text))) {
    tryCatch(as.list(str2lang(paste0("alist(", text, ")")))[-1L], error = function(e) list())
  } else {
    list()
  }
  if (is.null(label) && !is.null(opts$label)) label <- as.character(opts$label)
  list(label = label, opts = opts)
}

# Quarto's "#| key: value" options
pipe_options <- function(lines) {
  opts <- list()
  for (l in lines[grepl("^\\s*#\\|", lines)]) {
    m <- regmatches(l, regexec("^\\s*#\\|\\s*([A-Za-z0-9_.-]+)\\s*:\\s*(.*)$", l))[[1L]]
    if (!length(m)) next
    key <- gsub("-", ".", m[2L])
    value <- trimws(m[3L])
    opts[[key]] <- if (tolower(value) %in% c("true", "yes")) TRUE
      else if (tolower(value) %in% c("false", "no")) FALSE
      else tryCatch(str2lang(value), error = function(e) gsub("^[\"']|[\"']$", "", value))
  }
  opts
}

# list(exprs: the R code, parsed; chunks: list(label, engine, opts, items: list of expression
# indexes or chunk labels (references), in order))
document_code <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (document_kind(path) == "rnw") {
    begin <- "^\\s*<<(.*)>>=.*$"
    end <- "^\\s*@\\s*(%+.*|)$"
  } else {
    begin <- "^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+)( *[ ,].*)?\\}\\s*$"
    end <- "^[\t >]*```+\\s*$"
  }
  ref <- "^\\s*<<(.+)>>\\s*$"
  code <- character(length(lines))
  chunks <- list()
  cur <- NULL
  close_chunk <- function() {
    p <- parse_params(cur$params)
    cur$label <- if (is.null(p$label)) paste0("unnamed-chunk-", length(chunks) + 1L) else p$label
    cur$opts <- utils::modifyList(p$opts, pipe_options(lines[cur$lines]))
    chunks[[length(chunks) + 1L]] <<- cur
  }
  for (i in seq_along(lines)) {
    l <- lines[i]
    if (is.null(cur)) {
      if (grepl(begin, l, perl = TRUE)) {
        m <- regmatches(l, regexec(begin, l, perl = TRUE))[[1L]]
        rnw <- document_kind(path) == "rnw"
        cur <- list(start = i, engine = if (rnw) "r" else tolower(m[2L]),
                    params = if (rnw) m[2L] else sub("^[ ,]+", "", m[3L]),
                    lines = integer(), refs = list())
      }
    } else if (grepl(end, l, perl = TRUE)) {
      close_chunk()
      cur <- NULL
    } else if (grepl(ref, l, perl = TRUE)) {
      cur$refs[[length(cur$refs) + 1L]] <- list(line = i, label = trimws(sub(ref, "\\1", l, perl = TRUE)))
    } else {
      cur$lines <- c(cur$lines, i)
      if (cur$engine == "r") code[i] <- l
    }
  }
  if (!is.null(cur)) close_chunk()
  exprs <- parse(text = code, keep.source = TRUE, srcfile = srcfilecopy(path, code))
  first <- vapply(attr(exprs, "srcref"), function(r) r[[1L]], 0L)
  for (k in seq_along(chunks)) { # each chunk's expressions and references, by line
    ch <- chunks[[k]]
    mine <- which(first >= ch$start & first <= max(c(ch$lines, ch$start)))
    at <- c(first[mine], vapply(ch$refs, function(r) r$line, 0L))
    items <- c(as.list(mine), lapply(ch$refs, function(r) r$label))
    chunks[[k]]$items <- items[order(at)]
  }
  list(exprs = exprs, chunks = chunks, lines = lines)
}

# a chunk option: the chunk's (evaluated now, in the global environment), else the knitting
# package's default (knitrmini's, else knitr's), else `default`
chunk_option <- function(chunk, key, default) {
  v <- chunk$opts[[key]]
  if (!is.null(v)) return(eval(v, globalenv()))
  for (pkg in c("knitrmini", "knitr")) {
    if (!(pkg %in% loadedNamespaces())) next
    d <- tryCatch(get("opts_chunk", envir = asNamespace(pkg))$get(key), error = function(e) NULL)
    if (!is.null(d)) return(d)
  }
  default
}

# Runs a document's R chunks (a script frame: .rdap_script)
run_document <- function(path) {
  .rdap_script <- path
  .rdap_envir <- globalenv()
  doc <- document_code(path)
  code <- .Call(C_instrument, doc$exprs, path)
  if (document_kind(path) == "md" && requireNamespace("knitr", quietly = TRUE)) {
    p <- tryCatch(knitr::knit_params(doc$lines), error = function(e) list())
    if (length(p)) assign("params", stats::setNames(lapply(p, `[[`, "value"), vapply(p, `[[`, "", "name")),
                          envir = globalenv())
  }
  labels <- vapply(doc$chunks, function(ch) ch$label, "")
  # a chunk's expressions, its references expanded (ref.label first)
  expand <- function(ch, depth = 0L) {
    if (depth > 20L) stop("rdap: chunk references go round in circles")
    refs <- if (!is.null(ch$opts$ref.label)) as.list(eval(ch$opts$ref.label, globalenv())) else list()
    out <- integer()
    for (it in c(refs, ch$items)) {
      if (is.numeric(it)) out <- c(out, it)
      else for (j in which(labels == it)) out <- c(out, expand(doc$chunks[[j]], depth + 1L))
    }
    out
  }
  for (ch in doc$chunks) {
    if (ch$engine != "r") next
    ev <- chunk_option(ch, "eval", TRUE)
    if (isFALSE(ev)) next
    if (!is.null(ch$opts$child)) {
      .rdap_line <- ch$start # (where this document is, while its child runs: stack())
      for (child in eval(ch$opts$child, globalenv()))
        run_document(normalizePath(file.path(dirname(path), child), mustWork = TRUE))
      next
    }
    items <- expand(ch)
    if (is.numeric(ev)) items <- items[ev]
    errors_ok <- isTRUE(chunk_option(ch, "error", FALSE))
    for (k in items) {
      r <- if (errors_ok) {
        tryCatch(withVisible(eval(code[[k]], .rdap_envir)), error = function(e) {
          message("Error in chunk ", ch$label, ": ", conditionMessage(e))
          NULL
        })
      } else {
        withVisible(eval(code[[k]], .rdap_envir))
      }
      if (!is.null(r) && r$visible) print(r$value)
    }
  }
  invisible(NULL)
}

# a package being developed: loaded from its sources (its functions get probes in their files)
load_package <- function(root, helpers) {
  if (!requireNamespace("pkgload", quietly = TRUE))
    stop("rdap: debugging a package needs the pkgload package (devtools has it)")
  pkgload::load_all(root, export_all = TRUE, helpers = helpers, quiet = TRUE)
  instrument_loaded()
}

namespace_registry <- function() .Internal(getNamespaceRegistry())

# The functions of the loaded namespaces that have source references to files (a package loaded
# with devtools::load_all()), not instrumented yet, get probes in place
ns_seen <- new.env()
instrument_loaded <- function() {
  fns <- list()
  paths <- character()
  for (ns in setdiff(loadedNamespaces(), "base")) {
    env <- asNamespace(ns)
    if (identical(ns_seen[[ns]], env)) next
    assign(ns, env, envir = ns_seen)
    b <- .Call(C_env_bindings, env) # (values only: installed packages' lazy-loaded ones are left be)
    for (i in which(b$kinds == 0L)) {
      f <- b$values[[i]]
      if (!is.function(f) || is.primitive(f)) next
      ref <- attr(f, "srcref")
      if (is.null(ref)) next
      path <- srcfile_path(attr(ref, "srcfile"))
      if (is.na(path) || !is.na(entry_probe(f))) next
      fns[[length(fns) + 1L]] <- f
      paths[[length(paths) + 1L]] <- path
    }
  }
  if (length(fns)) .Call(C_instrument_closures, fns, paths)
  invisible(length(fns))
}

# a watched variable's frame returned (an on.exit() rdap put there)
scope_end <- function(target) .Call(C_scope_end, target)

condition_header <- function(cnd, what) {
  call <- conditionCall(cnd)
  if (is.null(call)) what else paste0(what, " in ", paste(deparse(call, nlines = 1L), collapse = ""))
}
on_error <- function(e) .Call(C_exception, condition_header(e, "Error"), conditionMessage(e), "error", e)
on_warning <- function(w) .Call(C_exception, condition_header(w, "Warning"), conditionMessage(w), "warning", w)

# Errors that a tryCatch() (or try()) will handle: while the "handled errors" filter is on,
# tryCatch()'s body runs its code inside withCallingHandlers(), whose handler sees an error before
# tryCatch()'s own does (its stack still there)
handled_error <- function(e, classes) {
  if (inherits(e, classes)) .Call(C_exception, condition_header(e, "Error"), conditionMessage(e), "handled", e)
}
tryCatch_body <- NULL # base::tryCatch's own body, while it's replaced
set_handled_hook <- function(on) {
  f <- base::tryCatch
  if (on && is.null(tryCatch_body)) {
    b <- body(f)
    last <- b[[length(b)]] # tryCatchList(expr, classes, parentenv, handlers)
    if (!(is.call(last) && identical(last[[1L]], quote(tryCatchList)))) return(FALSE)
    handler <- call("function", as.pairlist(alist(e = )), as.call(list(handled_error, quote(e), quote(classes))))
    last[[2L]] <- call("withCallingHandlers", quote(expr), error = handler)
    b[[length(b)]] <- last
    tryCatch_body <<- .Call(C_swap_body, f, b)
  } else if (!on && !is.null(tryCatch_body)) {
    .Call(C_swap_body, f, tryCatch_body)
    tryCatch_body <<- NULL
  }
  TRUE
}

# the condition stopped at, for the Exception scope: list(message, call, class)
condition_view <- function(cnd) {
  c(list(message = conditionMessage(cnd), call = conditionCall(cnd), class = class(cnd)),
    unclass(cnd)[setdiff(names(cnd), c("message", "call"))])
}

# the R stack, as text (exceptionInfo)
stack_text <- function(frames) {
  paste(vapply(frames, function(f) {
    if (is.na(f$path) || f$line <= 0) f$name else sprintf("%s at %s:%d", f$name, basename(f$path), f$line)
  }, ""), collapse = "\n")
}

# Replaces base::source() (attached ahead of base) so that sourced files get breakpoints too.
# Anything this doesn't handle (connections, URLs, echo = TRUE) goes to base::source().
source <- function(file, local = FALSE, echo = verbose, print.eval = echo, ...,
                   verbose = getOption("verbose"), chdir = FALSE, encoding = "unknown") {
  plain <- is.character(file) && length(file) == 1L && !grepl("^[a-z]+://", file) &&
    file.exists(file) && !isTRUE(echo) && length(list(...)) == 0L
  if (!plain) {
    return(base::source(file, local = local, echo = echo, print.eval = print.eval, ...,
                        verbose = verbose, chdir = chdir, encoding = encoding))
  }
  .rdap_envir <- if (isTRUE(local)) parent.frame()
    else if (isFALSE(local)) globalenv()
    else if (is.environment(local)) local
    else stop("'local' must be TRUE, FALSE or an environment")
  path <- normalizePath(file, mustWork = TRUE)
  .rdap_script <- path
  if (chdir) {
    owd <- setwd(dirname(path))
    on.exit(setwd(owd), add = TRUE)
  }
  code <- .Call(C_instrument, parse(path, keep.source = TRUE, encoding = encoding), path)
  res <- list(value = NULL, visible = FALSE)
  for (e in code) {
    res <- withVisible(eval(e, .rdap_envir))
    if (print.eval && res$visible) print(res$value)
  }
  invisible(res)
}

# ------------------------------------------------------------------------------------------------
# the call stack

# frames a script frame runs its code through: not shown
script_plumbing <- c("withCallingHandlers", "withVisible", "eval", "print", "invisible",
                     "tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch")
# frames between code and what runs it (test_that() -> ... -> eval())
code_plumbing <- c("eval", "evalq", "eval_bare", "withCallingHandlers", "withRestarts", "withOneRestart",
                   "doWithOneRestart", "tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch", "try",
                   "force", "local", "with", "with.default", "suppressWarnings", "suppressMessages",
                   "test_code", "source_file", "expect_error", "quasi_capture")

# where signalling a condition starts: an exception's stack is cut there
signalling <- list(base::stop, base::warning, base::.handleSimpleError, base::signalCondition,
                   base::.signalSimpleWarning, base::message)

is_signalling <- function(f) {
  if (identical(environment(f), self)) return(!identical(f, run_file) && !identical(f, source))
  for (s in signalling) if (identical(f, s)) return(TRUE)
  FALSE
}

srcfile_path <- function(sf) {
  if (!is.environment(sf)) return(NA_character_)
  if (!is.null(sf$rdap_path)) return(sf$rdap_path)
  f <- sf$filename
  if (is.null(f) || !nzchar(f) || startsWith(f, "<")) return(NA_character_)
  if (!is.null(sf$wd) && !startsWith(f, "/")) f <- file.path(sf$wd, f)
  if (file.exists(f)) normalizePath(f) else NA_character_
}

# list(path, line, col) of a srcref (or of such a list, as a probe gives it)
srcref_loc <- function(ref) {
  if (is.null(ref)) return(NULL)
  if (is.list(ref)) return(ref)
  list(path = srcfile_path(attr(ref, "srcfile")), line = ref[[1L]], col = ref[[5L]])
}

call_label <- function(call) {
  h <- call[[1L]]
  if (is.symbol(h)) return(paste0(as.character(h), "()"))
  if (is.call(h) && (identical(h[[1L]], quote(`::`)) || identical(h[[1L]], quote(`:::`))))
    return(paste0(paste(deparse(h), collapse = ""), "()"))
  "<anonymous>()"
}

call_head <- function(call) {
  h <- call[[1L]]
  if (is.symbol(h)) as.character(h) else ""
}

# The frames below the caller (rdap's C code), top first: list(name, path, line, col, env, kind).
# `top` is where the top frame is (a srcref or list(path, line, col)); for an exception
# (`exception`), it's where the signalling started.
stack <- function(top, exception) {
  n <- sys.nframe() - 1L
  if (n < 1L) return(list())
  calls <- sys.calls()[seq_len(n)]
  frames <- sys.frames()[seq_len(n)]
  if (exception) {
    for (i in seq_len(n)) {
      if (is_signalling(sys.function(i))) {
        top <- attr(calls[[i]], "srcref")
        n <- i - 1L
        break
      }
    }
  }
  shown <- list()
  i <- 1L
  while (i <= n) {
    env <- frames[[i]]
    if (exists(".rdap_script", envir = env, inherits = FALSE)) {
      path <- get(".rdap_script", envir = env)
      shown[[length(shown) + 1L]] <- list(kind = "script", raw = i, name = basename(path),
                                          env = get(".rdap_envir", envir = env), path = path,
                                          line = get0(".rdap_line", envir = env, inherits = FALSE))
      i <- i + 1L
      while (i <= n && call_head(calls[[i]]) %in% script_plumbing) i <- i + 1L
    } else {
      name <- call_label(calls[[i]])
      if (call_head(calls[[i]]) %in% c("eval", "evalq")) {
        # code eval()ed in an environment (test_that()'s block, local(), with()): eval() shows as two
        # frames, eval()'s and the evaluation's (whose environment is the code's): the latter only,
        # named after what's evaluating it
        if (i < n && call_head(calls[[i + 1L]]) %in% c("eval", "evalq")) {
          i <- i + 1L
          next
        }
        if (identical(sys.function(i), base::eval)) { # (only one: its envir argument)
          e <- get0("envir", envir = env, inherits = FALSE)
          if (is.environment(e)) env <- e
        }
        for (j in rev(seq_len(i - 1L))) {
          if (call_head(calls[[j]]) %in% c(code_plumbing, script_plumbing)) next
          name <- call_label(calls[[j]])
          break
        }
      }
      shown[[length(shown) + 1L]] <- list(kind = "function", raw = i, name = name, env = env, path = NA_character_)
      i <- i + 1L
    }
  }
  # below the first script frame: rdap's own (run_file() running a document)
  first_script <- Position(function(f) f$kind == "script", shown)
  if (!is.na(first_script) && first_script > 1L) shown <- shown[-seq_len(first_script - 1L)]
  m <- length(shown)
  out <- vector("list", m)
  for (k in seq_len(m)) {
    f <- shown[[k]]
    # where a frame is: where it called the next one from (the top frame: `top`)
    loc <- srcref_loc(if (k < m) attr(calls[[shown[[k + 1L]]$raw]], "srcref") else top)
    if (is.null(loc) && !is.null(f$line)) loc <- list(path = f$path, line = f$line, col = 1L)
    path <- if (!is.null(loc) && !is.na(loc$path)) loc$path else f$path
    out[[m - k + 1L]] <- list(
      name = f$name, kind = f$kind, env = f$env,
      path = if (is.null(path)) NA_character_ else path,
      line = if (is.null(loc)) 0L else as.integer(loc$line),
      col = if (is.null(loc)) 0L else as.integer(loc$col)
    )
  }
  out
}

# ------------------------------------------------------------------------------------------------
# values

clip <- function(s, n = 200L) if (nchar(s) > n) paste0(substr(s, 1L, n - 1L), "…") else s

summary1 <- function(x) {
  s <- tryCatch(summary_impl(x), error = function(e) NULL)
  if (is.null(s) || length(s) != 1L || is.na(s)) s <- paste0("<", class(x)[1L], ">")
  clip(s)
}

summary_impl <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.environment(x)) {
    name <- environmentName(x)
    return(if (nzchar(name)) paste0("<environment: ", name, ">") else format(x))
  }
  if (is.function(x)) {
    a <- deparse(args(x))
    return(if (length(a) > 1L) paste(trimws(a[-length(a)]), collapse = " ") else "function")
  }
  if (is.data.frame(x)) return(sprintf("data.frame [%d × %d]", nrow(x), ncol(x)))
  if (is.atomic(x) && length(x) == 1L && !is.object(x) && is.null(dim(x))) {
    if (is.character(x)) return(encodeString(x, quote = "\""))
    return(format(x))
  }
  if (is.atomic(x) && length(x) == 0L && !is.object(x)) return(paste0(typeof(x), "(0)"))
  if (isS4(x)) return(paste0("<S4 object of class ", class(x)[1L], ">"))
  out <- utils::capture.output(utils::str(x, max.level = 0L, give.attr = FALSE, vec.len = 4L,
                                          nchar.max = 60L, list.len = 0L))
  trimws(out[1L])
}

type1 <- function(x) class(x)[1L]

# attributes worth showing (not those every such value has)
plain_attributes <- c("names", "dim", "dimnames", "class", "row.names", "levels", "tsp", "comment",
                      "srcref", "srcfile", "wholeSrcref")
extra_attributes <- function(x) {
  if (isS4(x)) return(list()) # (its slots are its attributes)
  a <- attributes(x)
  a[setdiff(names(a), plain_attributes)]
}

has_children <- function(x) {
  if (inherits(x, c("rdap_range", "rdap_rows", "rdap_matrix_rows")) || is.environment(x)) return(TRUE)
  if (isS4(x)) return(length(methods::slotNames(x)) > 0L)
  if (is.function(x)) return(FALSE)
  if (length(extra_attributes(x))) return(TRUE)
  if (is.list(x) || is.pairlist(x) || is.expression(x)) return(length(x) > 0L)
  if (is.atomic(x)) return(length(x) > 1L)
  FALSE
}

backtick <- function(name) if (make.names(name) == name) name else paste0("`", gsub("`", "\\\\`", name), "`")

# a row of the variables view (`obj`: the value itself, for its memory address)
row <- function(name, value, type, evalname, child, obj = NULL) {
  list(name = name, value = value, type = type, evaluateName = evalname, child = child, obj = obj)
}

value_row <- function(name, v, evalname, obj = v) {
  row(name, summary1(v), type1(v), evalname, if (has_children(v)) v else NULL, obj)
}

# The rows shown under a value (`evalname`: the expression that gives it, or NULL)
children <- function(x, evalname) {
  if (inherits(x, "rdap_range")) return(elements(x$obj, x$from, x$to, x$evalname))
  if (inherits(x, "rdap_rows")) return(df_rows(x$obj, x$from, x$to, x$evalname))
  if (inherits(x, "rdap_matrix_rows")) return(matrix_rows(x$obj, x$from, x$to, x$evalname))
  if (is.environment(x)) return(env_children(x, evalname))
  rows <- value_children(x, evalname)
  extra <- extra_attributes(x)
  if (length(extra)) {
    rows[[length(rows) + 1L]] <- row("attributes", summary1(extra), "list",
                                     if (!is.null(evalname)) paste0("attributes(", evalname, ")"), extra)
  }
  rows
}

value_children <- function(x, evalname) {
  if (is.data.frame(x)) {
    return(c(elements(x, 1L, length(x), evalname),
             list(row("[rows]", sprintf("%d rows", nrow(x)), "", NULL,
                      structure(list(obj = x, from = 1L, to = nrow(x), evalname = evalname), class = "rdap_rows")))))
  }
  if (is.matrix(x)) return(matrix_rows(x, 1L, nrow(x), evalname))
  if (is.factor(x)) {
    rows <- elements(structure(as.character(x), names = names(x)), 1L, length(x), evalname)
    return(c(rows, list(value_row("levels", levels(x), if (!is.null(evalname)) paste0("levels(", evalname, ")")))))
  }
  if (isS4(x)) {
    return(lapply(methods::slotNames(x), function(s) {
      value_row(s, methods::slot(x, s), if (is.null(evalname)) NULL else paste0(evalname, "@", backtick(s)))
    }))
  }
  if (is.pairlist(x)) x <- as.list(x)
  if (is.list(x) || is.expression(x) || is.atomic(x)) return(elements(x, 1L, length(x), evalname))
  list()
}

# rows from..to of x, in ranges of 100 (of 10000, ...) when there are more
ranges <- function(x, from, to, evalname, class) {
  n <- to - from + 1L
  size <- 100L
  while (n / size > 100L) size <- size * 100L
  lapply(seq(from, to, by = size), function(s) {
    e <- min(s + size - 1L, to)
    row(sprintf("[%d..%d]", s, e), "", "", NULL,
        structure(list(obj = x, from = s, to = e, evalname = evalname), class = class))
  })
}

# a data frame's rows: each one's cells
df_rows <- function(x, from, to, evalname) {
  if (to - from + 1L > 100L) return(ranges(x, from, to, evalname, "rdap_rows"))
  rn <- if (is.character(attr(x, "row.names"))) rownames(x) else NULL
  lapply(seq.int(from, to), function(i) {
    cells <- lapply(x, function(col) col[[i]])
    value <- paste(vapply(names(cells), function(n) paste0(n, " = ", summary1(cells[[n]])), ""), collapse = ", ")
    en <- if (!is.null(evalname)) sprintf("%s[%d, ]", evalname, i)
    row(if (is.null(rn)) sprintf("[%d,]", i) else rn[i], clip(value), "row", en, as.list(x[i, , drop = FALSE]))
  })
}

# a matrix's rows
matrix_rows <- function(x, from, to, evalname) {
  if (to - from + 1L > 100L) return(ranges(x, from, to, evalname, "rdap_matrix_rows"))
  rn <- rownames(x)
  lapply(seq.int(from, to), function(i) {
    r <- x[i, ]
    en <- if (!is.null(evalname)) sprintf("%s[%d, ]", evalname, i)
    row(if (is.null(rn)) sprintf("[%d,]", i) else rn[i], summary1(r), type1(r), en,
        if (length(r) > 1L) r else NULL)
  })
}

elements <- function(x, from, to, evalname) {
  if (to - from + 1L > 100L) return(ranges(x, from, to, evalname, "rdap_range"))
  nms <- names(x)
  listy <- is.list(x) || is.expression(x)
  lapply(seq.int(from, to), function(i) {
    nm <- if (!is.null(nms) && !is.na(nms[i]) && nzchar(nms[i])) nms[i] else NULL
    v <- if (listy) x[[i]] else unname(x[i])
    label <- if (!is.null(nm)) nm else sprintf(if (listy) "[[%d]]" else "[%d]", i)
    en <- if (is.null(evalname)) NULL
      else if (is.null(nm)) sprintf(if (listy) "%s[[%d]]" else "%s[%d]", evalname, i)
      else if (listy) paste0(evalname, "$", backtick(nm))
      else sprintf("%s[[%s]]", evalname, encodeString(nm, quote = "\""))
    value_row(label, v, en, if (listy) v else NULL) # (x[i] is a copy: no address of its own)
  })
}

env_children <- function(env, evalname) {
  b <- .Call(C_env_bindings, env)
  hide_dots <- identical(env, globalenv())
  rows <- list()
  for (i in seq_along(b$names)) {
    name <- b$names[[i]]
    if (hide_dots && startsWith(name, ".")) next
    en <- if (is.null(evalname)) backtick(name) else paste0(evalname, "$", backtick(name))
    v <- b$values[[i]]
    rows[[length(rows) + 1L]] <- switch(
      b$kinds[[i]] + 1L,
      value_row(name, v, en),                                                        # 0: value
      row(name, clip(paste0("<promise: ", paste(deparse(v), collapse = " "), ">")),  # 1: unforced
          "promise", en, NULL),
      row(name, "<active binding>", "active binding", en, NULL),                     # 2
      row(name, "<missing>", "missing", en, NULL),                                   # 3
      row(name, sprintf("<%d arguments>", v), "...", NULL, NULL)                     # 4: ...
    )
  }
  rows
}

# ------------------------------------------------------------------------------------------------
# evaluating

# list(ok, result, type, child) (or list(ok = FALSE, result = message))
evaluate <- function(text, env, context) {
  exprs <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) e)
  if (inherits(exprs, "error")) return(list(ok = FALSE, result = conditionMessage(exprs)))
  value <- NULL
  visible <- FALSE
  repl <- identical(context, "repl")
  err <- NULL
  out <- utils::capture.output(err <- tryCatch({
    for (e in exprs) {
      r <- withVisible(eval(e, env))
      value <- r$value
      visible <- r$visible
      if (repl && visible) print(value)
    }
    NULL
  }, error = function(e) e, interrupt = function(e) simpleError("Interrupted (by pause)", NULL)))
  if (!is.null(err)) {
    msg <- paste0(condition_header(err, "Error"), ": ", conditionMessage(err))
    return(list(ok = FALSE, result = paste(c(out, msg), collapse = "\n")))
  }
  if (repl) {
    return(list(ok = TRUE, result = paste(out, collapse = "\n"), type = if (visible) type1(value) else "",
                child = if (visible && has_children(value)) value else NULL,
                obj = if (visible) value else NULL))
  }
  list(ok = TRUE, result = summary1(value), type = type1(value),
       child = if (has_children(value)) value else NULL, obj = value)
}

# a log point's message, with each {expression} replaced by its value
interpolate <- function(msg, env) {
  m <- gregexpr("\\{[^{}]*\\}", msg)
  parts <- regmatches(msg, m)[[1L]]
  if (length(parts) == 0L) return(msg)
  values <- vapply(parts, function(p) {
    code <- substr(p, 2L, nchar(p) - 1L)
    tryCatch(paste(format(eval(parse(text = code), env)), collapse = " "),
             error = function(e) paste0("<error: ", conditionMessage(e), ">"))
  }, "")
  regmatches(msg, m) <- list(values)
  msg
}

# ------------------------------------------------------------------------------------------------
# setting values

# Sets the row `name` under `container` (an environment, or a value whose rows have expressions,
# valid in `base`) to the value of `value` (R code, evaluated in `frame`). Returns its new row.
set_variable <- function(container, evalname, base, name, value, frame) {
  v <- eval(parse(text = value), frame)
  if (is.environment(container)) {
    assign(name, v, envir = container)
    en <- if (is.null(evalname)) backtick(name) else paste0(evalname, "$", backtick(name))
    return(value_row(name, get(name, envir = container), en))
  }
  rows <- children(container, evalname)
  r <- Find(function(r) identical(r$name, name), rows)
  if (is.null(r) || is.null(r$evaluateName)) stop("rdap can't set this one")
  target <- str2lang(r$evaluateName)
  eval(call("<-", target, v), base)
  value_row(name, eval(target, base), r$evaluateName)
}

set_expression <- function(expr, value, frame) {
  target <- str2lang(expr)
  eval(call("<-", target, eval(parse(text = value), frame)), frame)
  value_row(expr, eval(target, frame), expr)
}

# ------------------------------------------------------------------------------------------------
# completions: list(token (what they replace), items)

completions <- function(text, column, env) {
  line <- substr(text, 1L, column - 1L)
  # obj$na: obj's names (obj evaluated where the program is stopped)
  m <- regmatches(line, regexec("([[:alnum:]._]+(\\$[[:alnum:]._]+)*)\\$([[:alnum:]._]*)$", line))[[1L]]
  if (length(m)) {
    obj <- tryCatch(eval(parse(text = m[2L]), env), error = function(e) NULL)
    nms <- if (is.environment(obj)) ls(obj) else names(obj)
    nms <- nms[startsWith(nms, m[4L])]
    return(list(token = m[4L], items = vapply(nms, backtick, "", USE.NAMES = FALSE)))
  }
  token <- regmatches(line, regexpr("[[:alnum:]._:]*$", line))
  # the frame's variables (and its parents' up to the global environment), then R's own engine
  local <- character()
  e <- env
  while (!identical(e, emptyenv()) && !identical(e, baseenv())) {
    local <- c(local, ls(e, all.names = startsWith(token, ".")))
    if (identical(e, globalenv())) break
    e <- parent.env(e)
  }
  local <- local[startsWith(local, token)]
  rc <- tryCatch({
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(nchar(line))
    token <- utils:::.guessTokenFromLine()
    utils:::.completeToken()
    utils:::.retrieveCompletions()
  }, error = function(e) character())
  list(token = token, items = unique(c(sort(unique(local)), rc)))
}

# ------------------------------------------------------------------------------------------------
# function breakpoints

# the closure that `name` (f, pkg::f, pkg:::f) names, or NULL
find_function <- function(name) {
  m <- regmatches(name, regexec("^([[:alnum:].]+):::?(.+)$", name))[[1L]]
  f <- if (length(m)) {
    tryCatch(get(m[3L], envir = asNamespace(m[2L]), mode = "function"), error = function(e) NULL)
  } else {
    get0(name, envir = globalenv(), mode = "function")
  }
  if (is.null(f) || is.primitive(f)) NULL else f
}

# the id of the probe that starts the body of `f` (instrumented code), or NA
entry_probe <- function(f) {
  b <- body(f)
  if (is.call(b) && identical(b[[1L]], quote(`{`)) && length(b) >= 2L) {
    p <- b[[2L]]
    if (is.call(p) && identical(p[[1L]], base::.External2) && length(p) == 3L) return(p[[3L]])
  }
  NA_integer_
}

# `f` as source code, parsed (for code that wasn't loaded from a file: a package's): list(text,
# fun: the parsed function(...) call, srcfile)
function_source <- function(f, label) {
  b <- body(f)
  if (!(is.call(b) && identical(b[[1L]], quote(`{`)))) body(f) <- call("{", b) # (lines for its statement)
  lines <- deparse(f, width.cutoff = 80L, control = c("keepInteger", "keepNA", "niceNames", "showAttributes"))
  sf <- srcfilecopy(label, lines)
  exprs <- parse(text = lines, keep.source = TRUE, srcfile = sf)
  list(text = paste(lines, collapse = "\n"), fun = exprs[[1L]], srcfile = sf)
}

# ------------------------------------------------------------------------------------------------
# watchpoints, memory

where_bound <- function(name, env) {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) return(env)
    env <- parent.env(env)
  }
  NULL
}

# An expression that only reads values (names, constants, $, @, [[ ]], [ ], ::): watching one runs
# nothing (it's evaluated before every statement). list(ok, root: the variable it starts from)
watchable <- function(text) {
  e <- tryCatch(str2lang(text), error = function(err) NULL)
  if (is.null(e)) return(list(ok = FALSE))
  ok <- function(e) {
    if (!is.call(e)) return(is.symbol(e) || (is.atomic(e) && length(e) == 1L))
    h <- e[[1L]]
    if (!is.symbol(h) || !(as.character(h) %in% c("$", "@", "[[", "[", "::", ":::", "(", "-"))) return(FALSE)
    args <- as.list(e)[-1L]
    for (i in seq_along(args)) { # (args[[i]] may be empty: x[, 1])
      if (is.call(args[[i]])) {
        if (!ok(args[[i]])) return(FALSE)
      } else if (!(is.symbol(args[[i]]) || (is.atomic(args[[i]]) && length(args[[i]]) == 1L))) {
        return(FALSE)
      }
    }
    TRUE
  }
  root <- function(e) {
    if (is.symbol(e)) return(as.character(e))
    if (is.call(e) && !(as.character(e[[1L]]) %in% c("::", ":::")) && length(e) >= 2L) return(root(e[[2L]]))
    NA_character_
  }
  list(ok = ok(e), root = root(e))
}

# the expression of the row `name` under `container` (shown with the expression `evalname`)
child_expr <- function(container, evalname, name) {
  r <- Find(function(r) identical(r$name, name), children(container, evalname))
  if (is.null(r)) NULL else r$evaluateName
}

# the value of R code (for the address forms of evaluate)
value_of <- function(text, env) eval(parse(text = text), env)

# What stepping into `expr` (a statement, in the frame `env`) should go into: the closures it calls
# that have no probes (not base R's: stepping into eval() or stop() would only be noise), and for
# a UseMethod() the method it dispatches to. list(fns, labels)
step_targets <- function(expr, env, all = FALSE) { # (all: also those with probes)
  fns <- list()
  labels <- character()
  add <- function(f, name) {
    if (!is.function(f) || is.primitive(f) || (!all && !is.na(entry_probe(f)))) return()
    ns <- topenv(environment(f))
    pkg <- if (isNamespace(ns)) getNamespaceName(ns) else ""
    if (identical(pkg, "base")) return()
    for (g in fns) if (identical(g, f)) return()
    fns[[length(fns) + 1L]] <<- f
    labels[[length(labels) + 1L]] <<- if (nzchar(pkg)) paste0(pkg, "::", name) else name
  }
  walk <- function(e) {
    if (!is.call(e)) return()
    h <- e[[1L]]
    if (is.symbol(h)) {
      name <- as.character(h)
      if (name %in% c("quote", "bquote", "~", "function")) return()
      f <- get0(name, envir = env, mode = "function")
      if (name == "UseMethod") method_of(e, env, add)
      else if (is_base_generic(f) && length(e) >= 2L && is.symbol(e[[2L]])) {
        # print(x): (base's) generic's method for x (only a variable: nothing evaluated)
        obj <- get0(as.character(e[[2L]]), envir = env)
        if (!is.null(obj)) dispatch(name, obj, env, add)
      } else add(f, name)
    } else if (is.call(h) && (identical(h[[1L]], quote(`::`)) || identical(h[[1L]], quote(`:::`)))) {
      name <- paste(deparse(h), collapse = "")
      add(find_function(name), name)
    } else {
      walk(h)
    }
    args <- as.list(e)[-1L]
    for (i in seq_along(args)) if (is.call(args[[i]])) walk(args[[i]]) # (args[[i]] may be empty: x[, 1])
  }
  tryCatch(walk(expr), error = function(e) NULL)
  list(fns = fns, labels = labels)
}

# the S3 method UseMethod(generic[, object]) in `env` (the generic's frame) dispatches to
method_of <- function(call, env, add) {
  generic <- eval(call[[2L]], env)
  obj <- if (length(call) >= 3L) eval(call[[3L]], env) else {
    first <- names(formals(sys.function(which_frame(env))))[1L]
    if (is.na(first) || first == "...") return() else get(first, envir = env)
  }
  dispatch(generic, obj, env, add)
}

dispatch <- function(generic, obj, env, add) {
  for (cls in c(.class2(obj), "default")) {
    m <- utils::getS3method(generic, cls, optional = TRUE, envir = env)
    if (!is.null(m)) return(add(m, paste0(generic, ".", cls)))
  }
}

is_base_generic <- function(f) {
  if (!is.function(f) || is.primitive(f) || !isBaseNamespace(topenv(environment(f)))) return(FALSE)
  b <- body(f)
  is.call(b) && identical(b[[1L]], quote(UseMethod)) ||
    is.call(b) && identical(b[[1L]], quote(`{`)) && length(b) == 2L && is.call(b[[2L]]) && identical(b[[2L]][[1L]], quote(UseMethod))
}

# the number of the frame whose environment is `env`
which_frame <- function(env) {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) if (identical(frames[[i]], env)) return(i)
  stop("not a frame")
}
