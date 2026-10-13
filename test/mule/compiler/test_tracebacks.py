"""Compare complete native tracebacks with Python 3.14, without normalization."""

import os
import subprocess
import sys

import pytest

from mule.compiler import build


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
def test_tracebacks_match_python_314_verbatim(tmp_path, filename):
    assert sys.version_info[:2] == (3, 14), "traceback oracle must be Python 3.14"
    for name, source in PROGRAMS.items():
        (tmp_path / name).write_text(source)
    path = tmp_path / filename
    environment = {**os.environ, "PYTHON_COLORS": "0"}
    oracle = subprocess.run(
        [sys.executable, str(path)],
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
