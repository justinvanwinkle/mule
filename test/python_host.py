"""Select the external Python 3.14 oracle even when tests run under Mule."""

import shutil
import subprocess
import sys
from functools import cache


@cache
def python_host():
    executable = (
        sys.executable
        if sys.implementation.name == "cpython"
        else shutil.which("python3")
    )
    if executable is None:
        raise RuntimeError("Python 3.14 oracle executable was not found")
    result = subprocess.run(
        [
            executable,
            "-I",
            "-S",
            "-c",
            (
                "import sys; print(sys.implementation.name); "
                "print(sys.version_info.major, sys.version_info.minor)"
            ),
        ],
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    if result.returncode or result.stdout != "cpython\n3 14\n":
        raise RuntimeError(
            f"Expected a CPython 3.14 oracle at {executable!r}; "
            f"got {result.stdout!r}, {result.stderr!r}"
        )
    return executable
