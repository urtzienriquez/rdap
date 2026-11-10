"""rdap's tests: real debug sessions against the scripts in fixtures/. Run: make test (or
python3 tests/run.py [name ...]); RDAP_TEST_LOG=dir keeps each session's protocol log."""

import os
import sys
import time
import traceback

from dap import Client

FIX = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fixtures")
TESTS = []


def test(f):
    TESTS.append(f)
    return f


def fx(name):
    return os.path.join(FIX, name)


def eq(actual, expected, what=""):
    if actual != expected:
        raise AssertionError(f"{what}: expected {expected!r}, got {actual!r}")


def where(frame):
    return (frame["name"], os.path.basename(frame.get("source", {}).get("path", "")), frame["line"])


# ----------------------------------------------------------------------------------------------


@test
def breakpoint_stack_and_stepping(c):
    c.start(fx("basic.R"), {fx("basic.R"): [2]})
    body, frames = c.stopped()
    eq(body["reason"], "breakpoint", "reason")
    eq([where(f) for f in frames], [("add()", "basic.R", 2), ("compute()", "basic.R", 9), ("basic.R", "basic.R", 15)], "stack")
    loc = c.locals(frames[0]["id"])
    eq(loc["a"]["value"], "<promise: x>", "an unforced argument")
    eq(c.evaluate("a + b", frames[0]["id"])["body"]["result"], "[1] 2", "evaluate")
    eq(c.evaluate("total", frames[1]["id"])["body"]["result"], "[1] 0", "evaluate in the caller's frame")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("add()", "basic.R", 3), "step over")
    eq(c.locals(frames[0]["id"])["s"]["value"], "2", "a local")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("compute()", "basic.R", 9), "step over off the end of a function")
    c.request("stepIn", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("add()", "basic.R", 2), "step in")
    c.request("stepOut", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("compute()", "basic.R", 9), "step out")
    c.request("setBreakpoints", source={"path": fx("basic.R")}, breakpoints=[])
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    if "result: 18" not in c.output("stdout"):
        raise AssertionError("missing output: " + c.output())


@test
def sourced_file_step_off_the_end(c):
    """the session from the bug report: step into a sourced function, step past its end"""
    res = c.start(fx("my_script.R"), {fx("my_script.R"): [7], fx("my_func.R"): [3]})
    eq(res[fx("my_func.R")][0]["verified"], False, "a breakpoint in a file not loaded yet: pending")
    _, frames = c.stopped()
    eq(where(frames[0]), ("my_script.R", "my_script.R", 7), "top-level breakpoint")
    changed = [e for e in c.events if e["event"] == "breakpoint"]
    eq(any(e["body"]["breakpoint"]["verified"] for e in changed), True, "the pending breakpoint, once loaded")
    loaded = [os.path.basename(e["body"]["source"]["path"]) for e in c.events if e["event"] == "loadedSource"]
    eq(loaded, ["my_script.R", "my_func.R"], "loadedSource events")
    c.request("stepIn", threadId=1)
    _, frames = c.stopped()
    eq([where(f) for f in frames], [("my_func()", "my_func.R", 2), ("my_script.R", "my_script.R", 7)], "into the sourced function")
    c.request("next", threadId=1)
    body, frames = c.stopped()
    eq(where(frames[0]), ("my_func()", "my_func.R", 3), "step over (onto a breakpoint)")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("my_func()", "my_func.R", 4), "step over")
    c.request("next", threadId=1)  # off the end of the function, and of the script
    body, frames = c.stopped()
    eq((body.get("description"), where(frames[0])), ("End of the script", ("my_script.R", "my_script.R", 7)), "the end")
    c.request("next", threadId=1)
    eq(c.finish(), 0, "the program ends")


@test
def uncaught_error(c):
    c.start(fx("error.R"))
    body, frames = c.stopped()
    eq(body["reason"], "exception", "reason")
    eq(body.get("text"), "boom: 2", "message")
    eq([where(f) for f in frames], [("f()", "error.R", 3), ("g()", "error.R", 5), ("error.R", "error.R", 6)], "stack")
    eq(c.evaluate("y", frames[0]["id"])["body"]["result"], "[1] 2", "evaluate at the error")
    c.request("continue", threadId=1)
    eq(c.finish(), 1, "exit code")
    if "boom: 2" not in c.output("stderr"):
        raise AssertionError("the error isn't printed: " + c.output())


@test
def uncaught_error_filter_off(c):
    c.start(fx("error.R"), filters=())
    eq(c.finish(), 1, "exit code")
    eq(len([e for e in c.events if e["event"] == "stopped"]), 0, "stops")


@test
def conditions_logpoints_hit_counts(c):
    c.start(fx("loop.R"), {fx("loop.R"): [{"line": 2, "condition": "i == 7"}]})
    _, frames = c.stopped()
    eq(c.locals(frames[0]["id"], "Global")["i"]["value"], "7", "condition")
    c.request("setBreakpoints", source={"path": fx("loop.R")}, breakpoints=[{"line": 2, "logMessage": "i is {i}"}])
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    out = c.output("console")
    if "i is 8" not in out or "i is 10" not in out:
        raise AssertionError("log points: " + out)


@test
def hit_count(c):
    c.start(fx("loop.R"), {fx("loop.R"): [{"line": 2, "hitCondition": "3"}]})
    _, frames = c.stopped()
    eq(c.locals(frames[0]["id"], "Global")["i"]["value"], "3", "the third hit")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def pause_and_terminate(c):
    c.start(fx("busy.R"))
    time.sleep(0.3)
    c.request("pause", threadId=1)
    body, frames = c.stopped()
    eq(body["reason"], "pause", "reason")
    if int(c.locals(frames[0]["id"], "Global")["i"]["value"]) <= 0:
        raise AssertionError("i didn't count")
    c.request("terminate")
    c.event("terminated")
    c.request("disconnect")
    eq(c.proc.wait(timeout=5), 0, "rdap's exit code")


@test
def terminate_while_running(c):
    c.start(fx("sleep.R"))
    time.sleep(0.3)
    t = time.time()
    c.request("terminate")
    c.proc.wait(timeout=5)
    if time.time() - t > 3:
        raise AssertionError("too slow")


@test
def output(c):
    c.start(fx("output.R"))
    eq(c.finish(), 0, "exit code")
    out, err = c.output("stdout"), c.output("stderr")
    for s in ("hello", "from-child", "[1] 42"):
        if s not in out:
            raise AssertionError(f"{s!r} not in stdout: {out!r}")
    if "to stderr" not in err:
        raise AssertionError(f"stderr: {err!r}")


@test
def browser_call(c):
    c.start(fx("browser.R"))
    body, frames = c.stopped()
    eq((body["reason"], where(frames[0])), ("breakpoint", ("f()", "browser.R", 3)), "browser()")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("f()", "browser.R", 4), "step over from browser()")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def stop_on_entry(c):
    c.start(fx("basic.R"), stopOnEntry=True)
    _, frames = c.stopped()
    eq(where(frames[0]), ("basic.R", "basic.R", 1), "entry")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def variables(c):
    c.start(fx("vars.R"), {fx("vars.R"): [6]})
    _, frames = c.stopped()
    g = c.locals(frames[0]["id"], "Global")
    eq(g["df"]["value"], "data.frame [3 × 2]", "a data frame")
    df = c.children(g["df"]["variablesReference"])
    eq(list(df), ["a", "b", "[rows]"], "its columns, and its rows")
    r2 = c.children(c.children(df["[rows]"]["variablesReference"])["[2,]"]["variablesReference"])
    eq((r2["b"]["value"], r2["b"]["evaluateName"]), ('"y"', "df[2, ]$b"), "a row's cell")
    eq(df["b"]["evaluateName"], "df$b", "evaluateName")
    l = c.children(g["l"]["variablesReference"])
    eq(l["s"]["value"], '"two"', "a string")
    eq(c.children(l["nested"]["variablesReference"])["deep"]["value"], "TRUE", "nested")
    long = c.children(g["long"]["variablesReference"])
    eq(list(long), ["[1..100]", "[101..200]", "[201..250]"], "ranges")
    eq(c.children(long["[201..250]"]["variablesReference"])["[250]"]["value"], "250", "an element")
    eq(g["fn"]["value"], "function (x, y = 2)", "a function")
    r = c.evaluate("l$nested", frames[0]["id"], context="watch")["body"]
    eq((r["result"], r["variablesReference"] > 0), ("List of 1", True), "watch")
    r = c.evaluate("stop('nope')", frames[0]["id"], ok=False)
    eq(r["success"], False, "an evaluation error")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def watchpoints(c):
    c.start(fx("watch.R"), {fx("watch.R"): [7]})
    _, frames = c.stopped()
    info = c.request("dataBreakpointInfo", name="counter", frameId=frames[0]["id"])["body"]
    v = c.request("dataBreakpointInfo", name="v", frameId=frames[0]["id"])["body"]
    r = c.request("setDataBreakpoints", breakpoints=[{"dataId": info["dataId"]}, {"dataId": v["dataId"]}])
    eq([b["verified"] for b in r["body"]["breakpoints"]], [True, True], "set")
    c.request("setBreakpoints", source={"path": fx("watch.R")}, breakpoints=[])
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq((body["reason"], body.get("text")), ("data breakpoint", "counter changed (line 3): 0 -> 1"), "the change")
    eq(where(frames[0]), ("bump()", "watch.R", 4), "where it stops: the next statement")
    c.request("continue", threadId=1)
    body, _ = c.stopped()
    eq(body.get("text"), "counter changed (line 3): 1 -> 2", "again")
    later = c.request("dataBreakpointInfo", name="not_yet", frameId=frames[-1]["id"])["body"]  # (the script's)
    eq(later["dataId"] is not None, True, "a variable not assigned yet")
    c.request("setDataBreakpoints", breakpoints=[{"dataId": v["dataId"]}, {"dataId": later["dataId"]}])
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq(body.get("text"), "v changed (line 8): num [1:3] 1 2 3 -> num [1:3] 1 20 3", "a change in place")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def set_variables(c):
    c.start(fx("vars.R"), {fx("vars.R"): [6]})
    _, frames = c.stopped()
    scopes = c.request("scopes", frameId=frames[0]["id"])["body"]["scopes"]
    g = scopes[0]["variablesReference"]
    r = c.request("setVariable", variablesReference=g, name="long", value="1:3 * 2")["body"]
    eq(r["value"], "num [1:3] 2 4 6", "a variable")
    l = c.children(g)["l"]["variablesReference"]
    eq(c.request("setVariable", variablesReference=l, name="n", value="99")["body"]["value"], "99", "a list element")
    eq(c.evaluate("l$n", frames[0]["id"])["body"]["result"], "[1] 99", "really set")
    c.request("setExpression", expression="l$s", value="toupper(l$s)", frameId=frames[0]["id"])
    eq(c.evaluate("l$s", frames[0]["id"])["body"]["result"], '[1] "TWO"', "an expression")
    r = c.request("setVariable", ok=False, variablesReference=g, name="long", value="stop('no')")
    eq(r["success"], False, "a failing value")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def completions(c):
    c.start(fx("vars.R"), {fx("vars.R"): [6]})
    _, frames = c.stopped()
    def labels(text):
        r = c.request("completions", text=text, column=len(text) + 1, frameId=frames[0]["id"])["body"]
        return [t["label"] for t in r["targets"]]
    eq(labels("l$"), ["n", "s", "nested"], "a list's names")
    if "long" not in labels("lo"):
        raise AssertionError("a variable: " + str(labels("lo")))
    if "mean" not in labels("mea"):
        raise AssertionError("a function: " + str(labels("mea")))
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def function_breakpoints(c):
    res = c.start(fx("basic.R"), functions=["add"])
    eq(res["functions"][0]["verified"], False, "a function not defined yet: pending")
    body, frames = c.stopped()
    eq((body["reason"], where(frames[0])), ("breakpoint", ("add()", "basic.R", 2)), "at its first statement")
    c.request("setFunctionBreakpoints", breakpoints=[])
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def package_function_breakpoint(c):
    res = c.start(fx("pkg.R"), functions=["stats::sd"])
    b = res["functions"][0]
    eq((b["verified"], b["source"]["name"]), (True, "stats::sd"), "placed in the package's code")
    body, frames = c.stopped()
    eq(frames[0]["name"], "sd()", "stopped in it")
    ref = frames[0]["source"]["sourceReference"]
    content = c.request("source", sourceReference=ref)["body"]["content"]
    if "sqrt(var(" not in content:
        raise AssertionError("its source: " + content)
    eq(c.evaluate("x", frames[0]["id"])["body"]["result"], "[1] 1 5 9", "its arguments")
    eq(where(frames[1]), ("pkg.R", "pkg.R", 2), "its caller")
    c.request("stepOut", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("pkg.R", "pkg.R", 3), "out of it")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    if "sd: 4" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def memory(c):
    import base64, struct
    c.start(fx("vars.R"), {fx("vars.R"): [6]})
    _, frames = c.stopped()
    fid = frames[0]["id"]
    addr = c.evaluate("(void *)&(long)", fid, context="watch")["body"]["result"]
    eq(c.evaluate("sizeof(long)", fid, context="watch")["body"]["result"], "1000", "250 integers")
    r = c.request("readMemory", memoryReference=addr, count=12)["body"]
    eq(struct.unpack("<3i", base64.b64decode(r["data"])), (1, 2, 3), "their bytes")
    g = c.locals(fid, "Global")
    eq(g["long"].get("memoryReference"), addr, "the variable's memoryReference")
    r = c.request("readMemory", memoryReference="0x10", count=16)["body"]
    eq(r["unreadableBytes"], 16, "an unmapped address: no crash")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def step_into_package_code(c):
    c.start(fx("stepin.R"), {fx("stepin.R"): [3]})
    _, frames = c.stopped()
    c.request("stepIn", threadId=1)
    _, frames = c.stopped()
    eq((frames[0]["name"], frames[0]["source"]["name"], frames[0]["line"]), ("sd()", "stats::sd", 3),
       "into a package's function, at its statement")
    eq(where(frames[1]), ("stepin.R", "stepin.R", 3), "called from")
    c.request("stepIn", threadId=1)  # sd's statement calls var() (stats) and is.data.frame() (base)
    _, frames = c.stopped()
    eq(frames[0]["name"], "var()", "into the next one (not into base R)")
    # a breakpoint further down var()'s source, as nvim-dap sends it (run to cursor does this)
    ref = frames[0]["source"]["sourceReference"]
    content = c.request("source", sourceReference=ref)["body"]["content"].split("\n")
    line = next(i + 1 for i, l in enumerate(content) if ".Call(C_cov" in l)
    r = c.request("setBreakpoints", source={"path": f"dap-src://1/{ref}/", "name": ""}, breakpoints=[{"line": line}])
    eq(r["body"]["breakpoints"][0]["verified"], True, "a breakpoint in a source without a file")
    c.request("continue", threadId=1)
    _, frames = c.stopped()
    eq((frames[0]["name"], frames[0]["line"]), ("var()", line), "stopped there")
    c.request("setBreakpoints", source={"path": f"dap-src://1/{ref}/"}, breakpoints=[])
    c.request("stepOut", threadId=1)  # (sd() has nothing after var(): out of both)
    _, frames = c.stopped()
    eq(where(frames[0]), ("stepin.R", "stepin.R", 4), "back")
    c.request("stepIn", threadId=1)  # format(<rdap_thing>): format() is base's, its method isn't
    _, frames = c.stopped()
    eq(where(frames[0]), ("stepin.R", "stepin.R", 5), "base R: stepped over")
    c.request("setBreakpoints", source={"path": fx("stepin.R")}, breakpoints=[{"line": 7}])
    c.request("continue", threadId=1)
    c.stopped()
    c.request("stepIn", threadId=1)  # print(d): base's generic, stats' method
    _, frames = c.stopped()
    eq((frames[0]["name"], frames[0]["source"]["name"]), ("print.density()", "stats::print.density"), "an S3 method")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    if "sd: 4 1 2" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def watchpoint_scope_and_promises(c):
    c.start(fx("basic.R"), {fx("basic.R"): [2]})
    _, frames = c.stopped()
    # a: an argument not evaluated yet (evaluating it is no change); s: not assigned yet
    ids = [c.request("dataBreakpointInfo", name=n, frameId=frames[0]["id"])["body"]["dataId"] for n in ("a", "s")]
    c.request("setDataBreakpoints", breakpoints=[{"dataId": i} for i in ids])
    c.request("setBreakpoints", source={"path": fx("basic.R")}, breakpoints=[])
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq((body.get("text"), where(frames[0])), ("s changed (line 2): <unbound> -> 2", ("add()", "basic.R", 3)), "only s")
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq(body.get("text"), "a went out of scope (add() returned): no longer watched; "
                         "s went out of scope (add() returned): no longer watched", "the frame returned")
    eq(where(frames[0]), ("compute()", "basic.R", 9), "stopped in the caller")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "no more stops")


@test
def watchpoint_last_statement(c):
    c.start(fx("last.R"), stopOnEntry=True)
    _, frames = c.stopped()
    info = c.request("dataBreakpointInfo", name="x", frameId=frames[0]["id"])["body"]
    c.request("setDataBreakpoints", breakpoints=[{"dataId": info["dataId"]}])
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq((body.get("text"), frames[0]["line"]), ("x changed (line 1): <unbound> -> 1", 2), "first")
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq((body.get("text"), where(frames[0])), ("x changed (line 2): 1 -> 2", ("last.R", "last.R", 2)),
       "the last statement's change")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def step_past_the_last_line(c):
    c.start(fx("end.R"), {fx("end.R"): [5]})
    _, frames = c.stopped()
    eq(c.locals(frames[0]["id"], "Global")["y"]["value"], "4", "before the last line")
    c.request("next", threadId=1)
    body, frames = c.stopped()
    eq((body.get("description"), where(frames[0])), ("End of the script", ("end.R", "end.R", 5)), "a stop at the end")
    eq(c.locals(frames[0]["id"], "Global")["y"]["value"], "100", "the last line's effect")
    c.request("next", threadId=1)
    eq(c.finish(), 0, "then it ends")


@test
def continue_past_the_last_line(c):
    c.start(fx("end.R"), {fx("end.R"): [5]})
    c.stopped()
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "continue: no stop at the end")


@test
def forked_workers(c):
    c.start(fx("fork.R"), {fx("fork.R"): [2, 7]})  # 2: in the workers (not debugged); 7: after
    body, frames = c.stopped()
    eq(where(frames[0]), ("fork.R", "fork.R", 7), "only the main process stops")
    eq(c.evaluate("total", frames[0]["id"])["body"]["result"], "[1] 6", "the workers' results")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    out = c.output("stdout")
    if "worker 1" not in out or "worker 2" not in out:
        raise AssertionError("the workers' output: " + out)


@test
def crash(c):
    c.start(fx("crash.R"))
    eq(c.finish(), 128 + 11, "exit code")
    err = c.output("stderr")
    if "R crashed (segmentation fault)" not in err or "crash.R:2" not in err:
        raise AssertionError("the report: " + err)


@test
def program_input(c):
    c.terminal = True
    c.start(fx("input.R"))
    c.term_expect("Name: ")
    c.term_type("Rdap\n")
    c.term_expect("Hi Rdap")
    c.term_expect("Number: ")
    c.term_type("21\n")
    c.term_expect("Twice: 42")
    eq(c.finish(), 0, "exit code")
    c.term_expect("[the program ended]")


@test
def handled_errors(c):
    c.start(fx("handled.R"), filters=("uncaught", "handled"))
    body, frames = c.stopped()
    eq((body["reason"], body.get("text"), where(frames[0])), ("exception", "too big: 5", ("risky()", "handled.R", 2)),
       "an error try() handles")
    info = c.request("exceptionInfo", threadId=1)["body"]
    eq((info["exceptionId"], info["breakMode"], info["details"]["typeName"]),
       ("simpleError", "always", "simpleError, error, condition"), "exceptionInfo")
    if "risky() at handled.R:2" not in info["details"]["stackTrace"]:
        raise AssertionError(info["details"]["stackTrace"])
    exc = c.locals(frames[0]["id"], "Exception")
    eq(exc["message"]["value"], '"too big: 5"', "the Exception scope")
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq(body.get("text"), "too big: 7", "one tryCatch() handles")
    c.request("setExceptionBreakpoints", filters=["uncaught"])  # (off: tryCatch() as it was)
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    if "done -1 1" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def handled_errors_off(c):
    c.start(fx("handled.R"))
    eq(c.finish(), 0, "no stops")
    eq(len([e for e in c.events if e["event"] == "stopped"]), 0, "stops")


@test
def step_in_targets(c):
    c.start(fx("targets.R"), {fx("targets.R"): [7]})
    _, frames = c.stopped()
    targets = c.request("stepInTargets", frameId=frames[0]["id"])["body"]["targets"]
    eq([t["label"] for t in targets], ["outer()", "inner()"], "the calls on the line")
    # into inner(), though outer() is called first (inner(1) is its argument, evaluated in it)
    c.request("stepIn", threadId=1, targetId=next(t["id"] for t in targets if t["label"] == "inner()"))
    _, frames = c.stopped()
    eq(where(frames[0]), ("inner()", "targets.R", 2), "into the chosen one")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def variable_views(c):
    c.start(fx("views.R"), {fx("views.R"): [4]})
    _, frames = c.stopped()
    g = c.locals(frames[0]["id"], "Global")
    m = c.children(g["m"]["variablesReference"])
    eq((list(m), m["r2"]["evaluateName"]), (["r1", "r2"], "m[2, ]"), "a matrix's rows")
    eq(c.children(m["r2"]["variablesReference"])["c"]["value"], "6", "a cell")
    f = c.children(g["f"]["variablesReference"])
    eq((f["[2]"]["value"], f["levels"]["value"]), ('"hi"', 'chr [1:2] "lo" "hi"'), "a factor")
    u = c.children(g["u"]["variablesReference"])
    eq(c.children(u["attributes"]["variablesReference"])["units"]["value"], '"cm"', "attributes")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def watch_expressions(c):
    c.start(fx("exprwatch.R"), {fx("exprwatch.R"): [2]})
    _, frames = c.stopped()
    fid = frames[0]["id"]
    a = c.request("dataBreakpointInfo", name="l$a", frameId=fid)["body"]
    eq(a["description"], "l$a", "an expression")
    g = c.locals(fid, "Global")
    b = c.request("dataBreakpointInfo", name="b", variablesReference=g["l"]["variablesReference"])["body"]
    eq(b["description"], "l$b", "a member, from the variables tree")
    bad = c.request("dataBreakpointInfo", name="f(l)", frameId=fid)["body"]
    eq(bad["dataId"], None, "an expression that would run code")
    c.request("setDataBreakpoints", breakpoints=[{"dataId": a["dataId"], "condition": "l$a > 5"}])
    c.request("setBreakpoints", source={"path": fx("exprwatch.R")}, breakpoints=[])
    c.request("continue", threadId=1)
    body, frames = c.stopped()
    eq((body.get("text"), frames[0]["line"]), ("l$a changed (line 3): 4 -> 7", 4), "with a condition")
    c.request("setDataBreakpoints", breakpoints=[{"dataId": b["dataId"], "hitCondition": "2"}])
    c.request("continue", threadId=1)
    body, _ = c.stopped()
    eq(body.get("text"), "l$b changed (line 4): 16 -> 32", "with a hit count (its 2nd change from here)")
    c.request("setDataBreakpoints", breakpoints=[])
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


PKG = os.path.join(FIX, "pkgdev")


@test
def testthat_file(c):
    add = os.path.join(PKG, "R", "add.R")
    test = os.path.join(PKG, "tests", "testthat", "test-add.R")
    res = c.start(test, {add: [6], test: [2]})
    eq(res[add][0]["verified"], False, "in the package: pending until it's loaded")
    _, frames = c.stopped(timeout=30)
    eq(where(frames[0]), ("test_that()", "test-add.R", 2), "in the test")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(c.locals(frames[0]["id"])["r"]["value"], "2", "the test's variables")
    c.request("continue", threadId=1)
    eq(c.finish(30), 0, "exit code")
    if "Test passed" not in c.output():
        raise AssertionError(c.output())


@test
def load_all_in_a_script(c):
    add = os.path.join(PKG, "R", "add.R")
    c.start(os.path.join(PKG, "dev.R"), {add: [2]})
    _, frames = c.stopped(timeout=30)
    eq([where(f) for f in frames], [("add_one()", "add.R", 2), ("dev.R", "dev.R", 2)], "a breakpoint in R/add.R")
    c.request("continue", threadId=1)
    eq(c.finish(30), 0, "exit code")
    if "z: 6" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def step_into_package_files(c):
    test = os.path.join(PKG, "tests", "testthat", "test-add.R")
    c.start(test, {test: [2]})
    c.stopped(timeout=30)
    c.request("stepIn", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("add_one()", "add.R", 2), "into the package's function, in its own file")
    c.request("continue", threadId=1)
    eq(c.finish(30), 0, "exit code")


@test
def rnw_document(c):
    doc, child = fx("doc.Rnw"), fx("child.Rnw")
    c.start(doc, {doc: [12], child: [3]})
    _, frames = c.stopped()
    eq([where(f) for f in frames], [("doc.Rnw", "doc.Rnw", 12)], "in a chunk, in the document's lines")
    eq(c.evaluate("y", frames[0]["id"])["body"]["result"], "[1] 2", "the chunks before it ran (not eval=FALSE's)")
    c.request("continue", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("doc.Rnw", "doc.Rnw", 12), "again: <<calc>> used in another chunk")
    c.request("continue", threadId=1)
    _, frames = c.stopped()
    eq([where(f) for f in frames], [("child.Rnw", "child.Rnw", 3), ("doc.Rnw", "doc.Rnw", 18)], "a child document")
    c.request("next", threadId=1)
    _, frames = c.stopped()
    eq(where(frames[0]), ("doc.Rnw", "doc.Rnw", 21), "back in the main document")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    if "w: 5 v: 42" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def rmd_document(c):
    doc = fx("doc.Rmd")
    c.start(doc, {doc: [8]})
    _, frames = c.stopped()
    eq(where(frames[0]), ("doc.Rmd", "doc.Rmd", 8), "in a chunk")
    eq(c.evaluate("params$n", frames[0]["id"])["body"]["result"], "[1] 3", "the YAML params")
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")
    out = c.output("stdout")
    if "a: 6" not in out or "not R" in out:
        raise AssertionError(out)


@test
def knitrmini_defaults(c):
    c.start(fx("km.Rnw"))
    eq(c.finish(30), 0, "exit code (chunk a not run)")
    if "ran b" not in c.output("stdout"):
        raise AssertionError(c.output())


@test
def breakpoint_placement(c):
    path = fx("loop.R")  # 1 for (i in 1:10) {  2 sq <- i^2  3 }  4 cat("done\n")
    c.start(path, stopOnEntry=True)
    c.stopped()
    res = c.request("setBreakpoints", source={"path": path}, breakpoints=[{"line": 3}])["body"]["breakpoints"][0]
    eq((res["verified"], res.get("message")), (False, "No R code on this line (nor after it, in its block)"),
       "a loop's closing brace: not moved out of the loop")
    res = c.request("setBreakpoints", source={"path": path}, breakpoints=[{"line": 2, "condition": "i == 7"}])
    eq(res["body"]["breakpoints"][0]["line"], 2, "on the loop's statement")
    c.request("continue", threadId=1)
    _, frames = c.stopped()
    eq(c.evaluate("i", frames[0]["id"])["body"]["result"], "[1] 7", "the condition")
    c.request("setBreakpoints", source={"path": path}, breakpoints=[])
    c.request("continue", threadId=1)
    eq(c.finish(), 0, "exit code")


@test
def endless_evaluation(c):
    c.start(fx("vars.R"), {fx("vars.R"): [6]})
    _, frames = c.stopped()
    seq = c.send("evaluate", expression="repeat {}", frameId=frames[0]["id"], context="repl")
    time.sleep(0.5)
    c.request("pause", threadId=1)
    r = c.response(seq)
    eq((r["success"], "interrupt" in r.get("message", "").lower()), (False, True), "interrupted")
    eq(c.evaluate("1 + 1", frames[0]["id"])["body"]["result"], "[1] 2", "and the session goes on")
    seq = c.send("evaluate", expression="while (TRUE) NULL", frameId=frames[0]["id"], context="repl")
    time.sleep(0.3)
    c.request("terminate")
    c.event("terminated")
    c.request("disconnect")
    eq(c.proc.wait(timeout=5), 0, "terminate gets through")


# ----------------------------------------------------------------------------------------------


def main():
    names = sys.argv[1:]
    log_dir = os.environ.get("RDAP_TEST_LOG")
    failed = 0
    for t in TESTS:
        if names and t.__name__ not in names:
            continue
        c = Client(log=os.path.join(log_dir, t.__name__ + ".log") if log_dir else None)
        start = time.time()
        try:
            t(c)
            print(f"ok    {t.__name__} ({time.time() - start:.1f}s)")
        except Exception as e:
            failed += 1
            print(f"FAIL  {t.__name__}: {e}")
            if os.environ.get("RDAP_TEST_TRACE"):
                traceback.print_exc()
        finally:
            c.close()
    print("all passed" if not failed else f"{failed} failed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
