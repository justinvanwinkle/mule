"""Compare complete native tracebacks with Python 3.14, without normalization."""

import os
import re
import subprocess

import pytest

from mule.compiler import build, emit_llvm
from mule.compiler.driver import Toolchain
from test.python_host import python_host


def test_traceback_identity_allows_return_value_promotion(tmp_path):
    source = tmp_path / "add.ll"
    source.write_text(emit_llvm("def add(x, y): return x + y\n"))
    optimized = subprocess.run(
        [Toolchain().llvm_tool("opt"), "-passes=mem2reg", "-S", source],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    ).stdout
    # Isolate promotion from inlining and other LLVM transformations. Both
    # entries must return an SSA merge of the success value and error sentinel.
    for entry in ("body", "spec.0"):
        body = re.search(
            rf'^define[^\n]*@"?mule_fn_add\.{re.escape(entry)}"?\([^\n]*\)[^\n]*\{{\n(.*?)^\}}',
            optimized,
            re.MULTILINE | re.DOTALL,
        )
        assert body is not None
        returned = re.search(r"ret ptr (%[\w.]+)", body[1])
        assert returned is not None
        assert re.search(rf"{re.escape(returned[1])} = phi ptr ", body[1])


PROGRAMS = {
    "builtin_error.py": """def inner():
    return missing

def outer():
    return inner()

outer()
""",
    "finally_error.py": """def inner():
    raise ValueError('original failure')

def outer():
    try:
        try:
            inner()
        finally:
            pass
    finally:
        pass

outer()
""",
    "reraise_error.py": """def inner():
    raise LookupError('original failure')

def middle():
    try:
        inner()
    except LookupError:
        raise

def outer():
    try:
        middle()
    except LookupError as error:
        raise error

outer()
""",
}


@pytest.mark.parametrize("filename", PROGRAMS)
@pytest.mark.parametrize("debug_ranges", [True, False])
def test_tracebacks_match_python_314_verbatim(tmp_path, filename, debug_ranges):
    for name, source in PROGRAMS.items():
        (tmp_path / name).write_text(source)
    path = tmp_path / filename
    environment = {**os.environ, "PYTHON_COLORS": "0"}
    environment.pop("PYTHONNODEBUGRANGES", None)
    if not debug_ranges:
        environment["PYTHONNODEBUGRANGES"] = "1"
    oracle = subprocess.run(
        [python_host(), str(path)],
        cwd=tmp_path,
        env=environment,
        capture_output=True,
        timeout=10,
        check=False,
    )
    (tmp_path / f"{filename}.python.stderr").write_bytes(oracle.stderr)
    assert oracle.returncode == 1
    assert oracle.stderr.startswith(b"Traceback (most recent call last):\n")
    executable = build(path, tmp_path / path.stem, optimize=False, workers=1)
    native = subprocess.run(
        [executable],
        cwd=tmp_path,
        env=environment,
        capture_output=True,
        timeout=10,
        check=False,
    )
    (tmp_path / f"{filename}.mule.stderr").write_bytes(native.stderr)
    assert native.returncode == oracle.returncode
    assert native.stdout == oracle.stdout
    assert native.stderr == oracle.stderr
