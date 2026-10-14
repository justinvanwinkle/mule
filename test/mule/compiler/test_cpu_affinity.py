"""Linux CPU affinity uses real masks and Python's index protocol."""

import os
import subprocess

from mule.compiler import build
from test.mule.compiler.test_exception_groups import compare
from test.python_host import python_host


def test_affinity_mask_queries_updates_and_iterable_protocol_match_python(tmp_path):
    compare(
        tmp_path,
        """
import os

original = os.sched_getaffinity(0)
print(type(original).__name__, bool(original))
print(os.sched_getaffinity(os.getpid()) == original)
class Process:
    def __index__(self): return 0
print(os.sched_getaffinity(Process()) == original)
selected = min(original)
class CPU:
    def __index__(self): return selected
class Integer(int):
    def __index__(self): raise AssertionError('index override')
    def __int__(self): raise AssertionError('integer override')
try:
    print(os.sched_setaffinity(0, (CPU() for _ in range(2))))
    print(os.sched_getaffinity(0) == {selected}, os.process_cpu_count())
    print(os.sched_setaffinity(Process(), [Integer(selected)]))
    print(os.sched_getaffinity(0) == {selected})
finally:
    os.sched_setaffinity(0, original)
print(os.sched_getaffinity(0) == original)
""",
    )


def test_affinity_validation_and_kernel_errors_match_python(tmp_path):
    compare(
        tmp_path,
        """
import os

def report(function, *args):
    try:
        function(*args)
    except Exception as error:
        print(type(error).__name__, str(error), getattr(error, 'errno', None))
for pid in (-1, (1 << 31), -(1 << 31) - 1, (1 << 100), 1.0, 'pid', None):
    report(os.sched_getaffinity, pid)
    report(os.sched_setaffinity, pid, [])
class Broken:
    def __index__(self): raise TypeError('broken index')
class BadResult:
    def __index__(self): return 'cpu'
for mask in ([], [-1], [1 << 31], [1 << 100], [-(1 << 100)],
             [1.0], ['cpu'], [object()], [Broken()], [BadResult()], 0, None):
    report(os.sched_setaffinity, 0, mask)
report(os.sched_getaffinity, Broken())
report(os.sched_getaffinity, BadResult())
""",
    )


def test_cpu_count_environment_is_snapshotted_and_invalid_settings_raise(tmp_path):
    path = tmp_path / "counts.py"
    path.write_text(
        "import os\n"
        "print(os.cpu_count(), os.process_cpu_count())\n"
        "os.environ['PYTHON_CPU_COUNT'] = '7'\n"
        "print(os.cpu_count(), os.process_cpu_count())\n"
    )
    executable = build(path, tmp_path / "program", optimize=False, workers=1)
    for value in ("default", "3", "+3", "2147483647"):
        environment = {**os.environ, "PYTHON_CPU_COUNT": value}
        oracle = subprocess.run(
            [python_host(), path],
            env=environment,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        native = subprocess.run(
            [executable],
            env=environment,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        assert native.returncode == oracle.returncode == 0, native.stderr
        assert native.stdout == oracle.stdout
    for value in ("0", "-1", "2147483648", " 3 ", "x"):
        native = subprocess.run(
            [executable],
            env={**os.environ, "PYTHON_CPU_COUNT": value},
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        assert native.returncode == 1
        assert native.stdout == ""
        assert native.stderr.startswith("Traceback (most recent call last):\n")
        assert "ValueError: PYTHON_CPU_COUNT" in native.stderr
