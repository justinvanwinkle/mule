"""Builtin protocol helpers do not insert artificial Python traceback frames."""

import os
import subprocess

import pytest

from mule.compiler import build
from test.python_host import python_host


@pytest.mark.parametrize(
    "source",
    [
        "import math\nmath.factorial(-1)\n",
        "import posix\nposix.fstat(-1)\n",
        "import _csv\n_csv.reader(None)\n",
        "float.hex('1')\n",
        "float('nan').as_integer_ratio()\n",
        "float('inf').as_integer_ratio()\n",
        """def stream():
    raise LookupError('original')
    yield None

list(stream())
""",
        """def inner():
    return missing

def passthrough():
    yield

wrapped = passthrough()
next(wrapped)
try:
    inner()
except NameError as error:
    wrapped.throw(error)
""",
    ],
)
def test_builtin_tracebacks_match_python_verbatim(tmp_path, source):
    path = tmp_path / "scenario.py"
    path.write_text(source)
    environment = {**os.environ, "PYTHON_COLORS": "0"}
    oracle = subprocess.run(
        [python_host(), path],
        env=environment,
        capture_output=True,
        timeout=10,
        check=False,
    )
    executable = build(path, tmp_path / "program", optimize=False, workers=1)
    native = subprocess.run(
        [executable], env=environment, capture_output=True, timeout=10, check=False
    )
    (tmp_path / "python.stderr").write_bytes(oracle.stderr)
    (tmp_path / "mule.stderr").write_bytes(native.stderr)
    assert native.returncode == oracle.returncode == 1
    assert native.stdout == oracle.stdout
    assert native.stderr == oracle.stderr
