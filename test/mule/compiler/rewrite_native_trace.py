"""Observe selected native calls throughout real root-frame lifetimes.

GDB embeds host Python only. It runs the unchanged generated executable and
uses ordinary entry/finish breakpoints, including calls hidden in descendants.
Counts cover the requested entrypoints, not unobserved inlined operations.
"""

import json
import os
import re
import shlex
import signal
import subprocess
import tempfile
import traceback
from pathlib import Path


def trace_native(case, *, roots, watched, stdin="", args=()):
    """Trace resolved symbol regexes on the inferior's original main thread.

    Every root must execute and return normally. Expected language errors must
    return through that root, not terminate or unwind past its finish breakpoint.
    Unresolved watches fail even when the expected event count is zero.
    """
    assert roots and watched, "Native tracing requires roots and watched symbols"
    directory = Path(tempfile.mkdtemp(prefix="trace-", dir=case.source_path.parent))
    request = directory / "request.json"
    result = directory / "result.json"
    input_path = directory / "stdin"
    output_path = directory / "stdout"
    error_path = directory / "stderr"
    input_path.write_text(stdin)
    request.write_text(
        json.dumps(
            {
                "roots": roots,
                "watched": watched,
                "args": [str(arg) for arg in args],
                "stdin": str(input_path),
                "stdout": str(output_path),
                "stderr": str(error_path),
                "result": str(result),
            }
        )
    )
    script = directory / "trace.gdb"
    root = Path(__file__).resolve().parents[3]
    script.write_text(
        "set pagination off\nset confirm off\nset debuginfod enabled off\n"
        "set print demangle off\nset disable-randomization off\n"
        "python\nimport sys\n"
        f"sys.path.insert(0, {str(root)!r})\n"
        "from test.mule.compiler.rewrite_native_trace import _trace_inferior\n"
        f"_trace_inferior({str(request)!r})\nend\n"
    )
    command = [
        "/usr/bin/gdb",
        "--batch",
        "--nx",
        "-iex",
        "set auto-load off",
        "-x",
        str(script),
        str(case.executable),
    ]
    environment = os.environ.copy()
    environment.pop("PYTHONPATH", None)
    environment["LC_ALL"] = "C.UTF-8"
    with subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        env=environment,
        start_new_session=True,
    ) as process:
        try:
            output, _ = process.communicate(timeout=60)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            output, _ = process.communicate(timeout=10)
            raise AssertionError(
                f"Native trace exceeded 60s for {case.executable}\n{output}"
            ) from None
        assert process.returncode == 0, (
            f"Native trace failed for {case.executable}\n{output}"
        )
    observations = json.loads(result.read_text())
    native_stdout = output_path.read_text()
    native_stderr = error_path.read_text()
    for observation in observations.values():
        observation["stdout"] = native_stdout
        observation["stderr"] = native_stderr
    return observations


def _trace_inferior(request_path):
    # Only GDB's embedded interpreter provides this module. Never import it in
    # the compiler host or the generated executable.
    import gdb

    request = json.loads(Path(request_path).read_text())
    arguments = shlex.join(request["args"])
    redirects = (
        f" < {shlex.quote(request['stdin'])}"
        f" > {shlex.quote(request['stdout'])}"
        f" 2> {shlex.quote(request['stderr'])}"
    )
    gdb.execute("set args " + arguments + redirects)
    gdb.execute("start", to_string=True)
    main_thread = gdb.selected_thread().global_num
    # GDB 18.1 exposes relocated minimal symbols even when a loaded library also
    # has full debug information. Exclude PLT stubs to avoid duplicate events.
    symbol_table = gdb.execute("maint print msymbols", to_string=True)
    symbols = {}
    for match in re.finditer(
        r"^\[\d+\]\s+[tTiI]\s+0x([0-9a-fA-F]+)\s+(\S+)\s+section\s+(\S+)",
        symbol_table,
        re.MULTILINE,
    ):
        address, name, section = match.groups()
        if section.startswith(".text"):
            symbols.setdefault(name, set()).add(int(address, 16))

    def resolve(patterns):
        resolved = {}
        for label, pattern in patterns.items():
            matches = {
                name: addresses
                for name, addresses in symbols.items()
                if re.search(pattern, name)
            }
            assert matches, f"No native symbol matched {label!r}: {pattern!r}"
            resolved[label] = matches
        return resolved

    root_symbols = resolve(request["roots"])
    watched_symbols = resolve(request["watched"])
    observations = {
        label: {
            "entries": 0,
            "exits": 0,
            "watched": {watch: 0 for watch in watched_symbols},
            "symbols": sorted(matches),
            "watch_symbols": {
                watch: sorted(names) for watch, names in watched_symbols.items()
            },
            "thread": main_thread,
        }
        for label, matches in root_symbols.items()
    }
    active = {label: [] for label in root_symbols}
    failures = []
    finishes = []
    exited = []

    def on_exit(event):
        exited.append(event.exit_code)

    gdb.events.exited.connect(on_exit)

    class RootFinish(gdb.FinishBreakpoint):
        def __init__(self, labels):
            super().__init__(gdb.newest_frame(), internal=True)
            self.labels = labels
            for label in labels:
                active[label].append(self)
                observations[label]["entries"] += 1

        def stop(self):
            try:
                assert gdb.selected_thread().global_num == main_thread
                for label in self.labels:
                    assert active[label] and active[label][-1] is self, (
                        f"Unbalanced return from {label!r}"
                    )
                    active[label].pop()
                    observations[label]["exits"] += 1
            except Exception:  # noqa: BLE001 - GDB otherwise reports and ignores callback errors.
                failures.append(traceback.format_exc())
                return True
            return False

        def out_of_scope(self):
            failures.append(
                f"Root frame disappeared without returning: {self.labels!r}"
            )

    class Entry(gdb.Breakpoint):
        def __init__(self, address, root_labels, watch_labels):
            super().__init__(f"*{address:#x}", internal=True)
            self.root_labels = root_labels
            self.watch_labels = watch_labels

        def stop(self):
            try:
                if gdb.selected_thread().global_num != main_thread:
                    return False
                if self.root_labels:
                    finishes.append(RootFinish(self.root_labels))
                for label, frames in active.items():
                    if frames:
                        for watch in self.watch_labels:
                            observations[label]["watched"][watch] += 1
            except Exception:  # noqa: BLE001 - GDB otherwise reports and ignores callback errors.
                failures.append(traceback.format_exc())
                return True
            return False

    addresses = {}
    for category, patterns in (("roots", root_symbols), ("watched", watched_symbols)):
        for label, matches in patterns.items():
            for locations in matches.values():
                for address in locations:
                    addresses.setdefault(address, {"roots": set(), "watched": set()})[
                        category
                    ].add(label)
    entries = [
        Entry(address, sorted(labels["roots"]), sorted(labels["watched"]))
        for address, labels in addresses.items()
    ]
    assert entries
    gdb.execute("continue", to_string=True)
    assert not failures, "\n".join(failures)
    assert exited == [0], f"Inferior did not exit normally: {exited!r}\n" + (
        gdb.execute("bt", to_string=True) if not exited else ""
    )
    for label, observation in observations.items():
        assert observation["entries"] > 0, f"Root {label!r} never executed"
        assert observation["entries"] == observation["exits"] and not active[label], (
            f"Root {label!r} did not return normally: {observation!r}"
        )
        observation["exit_code"] = 0
    Path(request["result"]).write_text(json.dumps(observations))
