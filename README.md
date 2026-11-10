# rdap

A debugger for R scripts and documents (Rnw, R Markdown, Quarto) that speaks the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/)
(so it works from nvim-dap, VS Code, Helix, ...). It's one small C program that runs R inside
itself. The editor starts it and talks to it over stdin/stdout: no ports, no R package to install,
nothing to type into an R console.

```lua
-- nvim-dap, with this repository on 'runtimepath'
require("rdap").setup() -- { command = "/path/to/rdap" } if it isn't on $PATH
```

That adds two configurations, for R scripts and documents (filetypes `r`, `rnoweb`, `rmd`,
`quarto`): *Debug script* and *Debug this file*. *Debug script* runs the current file, unless that
file is one a script loads: then it runs the script last debugged that loaded it, or else a script
next to it (or in the working directory) that `source()`s it; for an Rnw with a
`% !TeX root = main.Rnw` line, the root. That way, starting or restarting from inside a sourced
file, where stepping into one of its functions leaves you, debugs the script instead of a file that
only defines functions.

To choose which call to step into on a line (`f(g(x))`), map `require("dap").step_into({ askForTargets = true })`.

Without the module, plain nvim-dap:

```lua
dap.adapters.r = { type = "executable", command = "rdap" }
dap.configurations.r = { { type = "r", request = "launch", name = "Debug file", program = "${file}" } }
```

## Features

- line breakpoints, also in files the script `source()`s (pending until the file runs)
- conditional breakpoints, hit counts (`3`, `>= 3`, `% 3`), log points (`i is {i}`)
- step over / into / out, continue, pause (even in an endless loop), stop on entry
- step into package functions too (and into the S3 method `print(x)`/`summary(fit)` dispatches
  to): they get probes when you step into a call to them, and show as their deparsed source
  (`rdap://stats::sd`). Base R's own functions are stepped over, the way gdb skips libc
- stops where an uncaught error is signalled, with the stack intact (and, optionally, on warnings)
- `browser()` in the code stops like a breakpoint
- function breakpoints by name (`my_fun`, `stats::sd`), also in packages: a function the script
  didn't load from a file is shown as its deparsed source (`rdap://stats::sd`), with breakpoints
  and stepping in it
- watchpoints (data breakpoints): stop when a variable changes, in place too (`x[2] <- 0`), or
  is first assigned; the stop says from what to what, and on which line
- set variables (also list elements, `l$a`, and expressions), completions in the debug console
- memory: where a value's data is (`memoryReference`, `readMemory`); also answers a C debugger's
  memory-view questions (`(void *)&(x)`, `sizeof(x)`), so gdb-oriented UIs (dapper) show it
- the call stack, with the line each frame is at, across scripts and sourced files
- variables: locals and globals; lists, data frames, environments, S4 slots; long vectors in
  ranges; unevaluated arguments shown as promises, *not* forced
- evaluate in any frame (REPL, watches, hover)
- the program's output (also from C code and child processes) as stdout/stderr
- `terminate` works even while R is busy
- the program's own terminal (the editor's runInTerminal: dapper's Program I/O): its output, and
  input for `readline()`, `scan()`, `menu()`
- stop on errors that `try()`/`tryCatch()` handle (filter), with the condition shown (an
  *Exception* scope, and `exceptionInfo`: class, message, stack)
- step into a chosen call (step-in targets)
- variables: matrices by rows, factors (labels and levels), data frames by columns and by rows,
  attributes
- watchpoints on expressions that only read values (`l$a`, `df$x[3]`, `obj@slot`), with conditions
  and hit counts; a function's local variable that goes out of scope ends its watch (and says so)
- stepping past the end of the script stops there once more, to see the final state
- packages: functions loaded with source references (`devtools::load_all()`, in the script or by
  the `package` option) get their probes in their own files, so breakpoints in `R/*.R` work, and
  stepping goes into them; a file in a package's `tests/testthat` runs with its package (and
  testthat) loaded, and `test_that()` blocks are frames with their own variables
- documents: an Rnw (knitrmini, knitr), R Markdown or Quarto file runs its R chunks in order, as
  knitting would (`eval`, also the default from `opts_chunk$set()`; other engines skipped; `error`;
  `child` documents; `ref.label` and `<<label>>` references; Quarto `#|` options; the YAML
  `params`), with breakpoints and the stack in the document's lines. Nothing is woven. `\Sexpr{}`
  isn't run
- forked workers (`parallel::mclapply`) aren't debugged, nor do they disturb the session; a crash
  in compiled code is reported with the last R statement that ran

## Build

Needs R built as a shared library (`libR.so`; Debian/Ubuntu's `r-base` is), a C compiler, make.

```sh
make            # build/rdap
make install    # ~/.local/bin/rdap (PREFIX=... to change)
make test       # real debug sessions against tests/fixtures (python3)
```

## Launch options

| option        | default                | |
|---------------|------------------------|-|
| `program`     | (required)             | the R script |
| `cwd`         | the script's directory | working directory |
| `args`        | `[]`                   | for `commandArgs(trailingOnly = TRUE)` |
| `env`         | `{}`                   | environment variables (`null` unsets) |
| `stopOnEntry` | `false`                | stop before the first statement |
| `console`     | `integratedTerminal` (if the editor can) | or `internalConsole`: output as output events, no input |
| `package`     | (a test file's package)  | a package's directory, `load_all()`ed before the program |

Exception filters: `uncaught` (default on), `warning`, `handled`.

## How it works

rdap embeds R (`Rf_initialize_R`) and owns its console, so all output arrives as callbacks, and
the protocol keeps the real stdin/stdout to itself.

When code is loaded (the script, and any file it `source()`s), rdap rewrites the parsed code: before
every statement it puts a probe, `.External2(<rdap_probe>, <id>)`, where the id says which
statement (file, line) it is. Statements keep their source references, so printed functions,
tracebacks and R's own srcref tracking are unchanged. A probe is a C function that, in the common
case, only checks a few flags. When there is a breakpoint there, or stepping or a pause says so,
it *stops right there*: it blocks inside C and answers the editor's requests on R's own thread,
with the exact frame in hand. Nothing is typed into a `Browse>` prompt and nothing is parsed from
console output, which is what makes other R debuggers fragile.

- **Stepping** is bookkeeping: *over* stops at the next probe in the same frame or a caller, *into*
  at the next probe, *out* at the next probe in a caller (by frame identity and depth).
- **Errors**: `withCallingHandlers()` around the whole program sees errors that nothing else
  handles, while their stack is still there.
- **The stack** comes from `sys.calls()`/`sys.frames()` (from the probe, they are exactly the
  program's frames); each frame's line is the srcref of the call it made.
- **Variables** are read without evaluating anything: promises are inspected, not forced.

The cost: about 1.6× slower in a tight loop of trivial statements, less in real code. Code that
wasn't loaded by the debugged script (packages) gets its probes when you step into it or name it
in a function breakpoint.

## Not yet

- a data viewer
- debugging inside forked workers
- macOS/Windows (Linux only so far)

Each watchpoint costs an `identical()` (an expression's, an evaluation) per statement: slow on big
data frames.
