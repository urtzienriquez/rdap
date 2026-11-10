"""A small Debug Adapter Protocol client, to drive rdap in tests."""

import json
import os
import pty
import select
import queue
import subprocess
import threading
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RDAP = os.environ.get("RDAP", os.path.join(ROOT, "build", "rdap"))


class DapError(Exception):
    pass


class Client:
    def __init__(self, log=None, terminal=False):
        args = [RDAP] + (["--log", log] if log else [])
        self.proc = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.seq = 1
        self.msgs = queue.Queue()
        self.events = []  # every event, in order
        self.pending = {}  # responses not yet taken, by request_seq
        self.stops_taken = 0
        self.terminal = terminal  # answer runInTerminal (with a pseudo-terminal)
        self.term = None  # its master side
        self.term_out = b""
        threading.Thread(target=self._read, daemon=True).start()

    def _read(self):
        f = self.proc.stdout
        while True:
            header = b""
            while not header.endswith(b"\r\n\r\n"):
                c = f.read(1)
                if not c:
                    self.msgs.put(None)
                    return
                header += c
            n = int(header.split(b"Content-Length:")[1].split(b"\r\n")[0])
            self.msgs.put(json.loads(f.read(n)))

    def _pump(self, timeout):
        m = self.msgs.get(timeout=timeout)
        if m is None:
            raise DapError("rdap exited")
        if m["type"] == "event":
            self.events.append(m)
        elif m["type"] == "request":
            self._reverse(m)
        elif m["type"] == "response":
            self.pending[m["request_seq"]] = m
        return m

    def _reverse(self, m):
        """a request from rdap: runInTerminal"""
        body, ok = {}, False
        if m["command"] == "runInTerminal" and self.terminal:
            a = m["arguments"]
            master, slave = pty.openpty()
            proc = subprocess.Popen(a["args"], cwd=a.get("cwd"), stdin=slave, stdout=slave, stderr=slave,
                                    start_new_session=True)
            os.close(slave)
            self.term, self.term_proc = master, proc
            body, ok = {"processId": proc.pid}, True
        out = json.dumps({"seq": self.seq, "type": "response", "request_seq": m["seq"], "success": ok,
                          "command": m["command"], "body": body}).encode()
        self.seq += 1
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(out) + out)
        self.proc.stdin.flush()

    def term_expect(self, text, timeout=10):
        """waits until the terminal has shown `text`"""
        end = time.time() + timeout
        while text.encode() not in self.term_out:
            try:  # (keep the protocol going too)
                self._pump(0.01)
            except queue.Empty:
                pass
            if self.term is not None and select.select([self.term], [], [], 0.05)[0]:
                try:
                    self.term_out += os.read(self.term, 4096)
                except OSError:
                    pass
            if time.time() > end:
                raise DapError(f"the terminal never showed {text!r}: {self.term_out!r}")

    def term_type(self, text):
        os.write(self.term, text.encode())

    def send(self, command, **arguments):
        seq = self.seq
        self.seq += 1
        body = json.dumps({"seq": seq, "type": "request", "command": command, "arguments": arguments}).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.proc.stdin.flush()
        return seq

    def response(self, seq, timeout=10):
        end = time.time() + timeout
        while seq not in self.pending:
            self._pump(max(0.01, end - time.time()))
        return self.pending.pop(seq)

    def request(self, command, ok=True, timeout=10, **arguments):
        r = self.response(self.send(command, **arguments), timeout)
        if ok and not r["success"]:
            raise DapError(f"{command} failed: {r.get('message')}")
        return r

    def event(self, name, timeout=10, after=0):
        """The first `name` event after the first `after` events (waits for it)."""
        end = time.time() + timeout
        while True:
            for i, e in enumerate(self.events[after:], after):
                if e["event"] == name:
                    return e
            try:
                self._pump(max(0.01, end - time.time()))
            except queue.Empty:
                raise DapError(f"no '{name}' event within {timeout}s; events: {[e['event'] for e in self.events]}")

    def output(self, category=None):
        return "".join(
            e["body"]["output"]
            for e in self.events
            if e["event"] == "output" and (category is None or e["body"]["category"] == category)
        )

    # -- a session -----------------------------------------------------------------------------

    def start(self, program, breakpoints=None, filters=("uncaught",), functions=None, **launch_args):
        """initialize, launch, set breakpoints ({path: [line or dict]}) and function breakpoints
        ([name]), configurationDone"""
        self.request("initialize", adapterID="rdap", linesStartAt1=True, columnsStartAt1=True,
                     supportsRunInTerminalRequest=self.terminal)
        launch = self.send("launch", program=program, **launch_args)
        self.event("initialized")
        r = self.response(launch)
        if not r["success"]:
            raise DapError("launch failed: " + str(r.get("message")))
        results = {}
        for path, lines in (breakpoints or {}).items():
            bps = [b if isinstance(b, dict) else {"line": b} for b in lines]
            results[path] = self.request("setBreakpoints", source={"path": path}, breakpoints=bps)["body"]["breakpoints"]
        if functions:
            results["functions"] = self.request(
                "setFunctionBreakpoints", breakpoints=[{"name": f} for f in functions])["body"]["breakpoints"]
        self.request("setExceptionBreakpoints", filters=list(filters))
        self.request("configurationDone")
        return results

    def stopped(self, timeout=10):
        """Waits for the next stop (each is returned once); returns (body, frames)."""
        end = time.time() + timeout
        while True:
            stops = [e for e in self.events if e["event"] == "stopped"]
            if len(stops) > self.stops_taken:
                e = stops[self.stops_taken]
                self.stops_taken += 1
                break
            try:
                self._pump(max(0.01, end - time.time()))
            except queue.Empty:
                raise DapError(f"no stop within {timeout}s; events: {[e['event'] for e in self.events]}")
        frames = self.request("stackTrace", threadId=1)["body"]["stackFrames"]
        return e["body"], frames

    def locals(self, frame_id, scope="Locals"):
        scopes = self.request("scopes", frameId=frame_id)["body"]["scopes"]
        ref = next(s["variablesReference"] for s in scopes if s["name"] == scope)
        return {v["name"]: v for v in self.request("variables", variablesReference=ref)["body"]["variables"]}

    def children(self, ref):
        return {v["name"]: v for v in self.request("variables", variablesReference=ref)["body"]["variables"]}

    def evaluate(self, expression, frame_id=None, context="repl", ok=True):
        args = {"expression": expression, "context": context}
        if frame_id:
            args["frameId"] = frame_id
        return self.request("evaluate", ok=ok, **args)

    def finish(self, timeout=10):
        """Waits for the program's end; returns its exit code."""
        code = self.event("exited", timeout)["body"]["exitCode"]
        self.event("terminated", timeout)
        return code

    def close(self):
        try:
            if self.proc.poll() is None:
                self.send("disconnect")
            self.proc.wait(timeout=5)
        except Exception:
            self.proc.kill()
        return self.proc.returncode
