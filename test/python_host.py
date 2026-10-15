"""Select the external CPython 3.14 oracle, the release of the vendored
standard library, whether tests run under a newer host Python or under Mule."""

import shutil
import subprocess
import sys
from functools import cache

ORACLE_VERSION = (3, 14)


def _candidates():
    if sys.implementation.name == "cpython" and sys.version_info[:2] == ORACLE_VERSION:
        yield sys.executable
    for name in ("python3.14t", "python3.14"):
        found = shutil.which(name)
        if found is not None:
            yield found
    located = subprocess.run(
        ["uv", "python", "find", "3.14"], capture_output=True, text=True, check=False
    )
    if located.returncode == 0 and located.stdout.strip():
        yield located.stdout.strip()


@cache
def python_host():
    executable = next(_candidates(), None)
    if executable is None:
        raise RuntimeError("CPython 3.14 oracle executable was not found")
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
