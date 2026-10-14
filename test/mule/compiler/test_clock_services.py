"""Kernel clock selection and integer nanosecond conversion match Python."""

import struct

import pytest

from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_native import run_native


@pytest.mark.integration
def test_clock_resolution_selection_index_protocol_and_errors_match_python(tmp_path):
    compare(
        tmp_path,
        """
import time
class Clock:
    def __index__(self): return 1
class Integer(int):
    def __index__(self): raise AssertionError('index override')
class Broken:
    def __index__(self): raise RuntimeError('broken')
class Bad:
    def __index__(self): return 'clock'
for clock in (0, 1, 2, 3, 4, 5, 6, 7, 11, Clock(), Integer(1)):
    print(time.clock_getres(clock))
    seconds = time.clock_gettime(clock)
    nanoseconds = time.clock_gettime_ns(clock)
    print(type(seconds).__name__, type(nanoseconds).__name__,
          abs(nanoseconds - seconds * 1_000_000_000) < 100_000_000)
for function in (time.clock_gettime, time.clock_gettime_ns, time.clock_getres):
    for clock in (-1, 1000, 1 << 31, -(1 << 31) - 1, 1 << 100,
                  1.0, 'clock', None, Broken(), Bad()):
        try:
            function(clock)
        except Exception as error:
            print(type(error).__name__, str(error), getattr(error, 'errno', None))
time.clock_gettime_ns = lambda clock: 'replacement'
for function in (time.time_ns, time.monotonic_ns, time.perf_counter_ns,
                 time.process_time_ns, time.thread_time_ns):
    print(isinstance(function(), int))
""",
    )


@pytest.mark.integration
def test_named_nanosecond_clocks_preserve_large_integer_records(tmp_path):
    result = run_native(
        tmp_path,
        """
import time
seen = []
def parts(clock):
    seen.append(clock)
    return 0, 2_000_000_000, 123456789
time._clock_parts = parts
expected = 2_000_000_000 * 1_000_000_000 + 123456789
for function in (time.time_ns, time.monotonic_ns, time.perf_counter_ns,
                 time.process_time_ns, time.thread_time_ns):
    print(function() == expected)
print(time.clock_gettime_ns(4) == expected)
print(seen)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\n" * 6 + "[0, 1, 1, 2, 3, 4]\n"


@pytest.mark.integration
def test_nanosecond_range_edges_and_float_conversion_formula(tmp_path):
    result = run_native(
        tmp_path,
        """
import time
import struct
record = (0, 0, 123456789)
def parts(clock): return record
time._clock_parts = parts
time._resolution_parts = parts
print(struct.pack('>d', time.clock_gettime(0)).hex(),
      struct.pack('>d', time.clock_getres(0)).hex())
for seconds, nanos in ((9223372036, 854775807), (-9223372037, 145224192),
                       (9223372036, 854775808), (-9223372037, 145224191)):
    record = 0, seconds, nanos
    try:
        print(time.clock_gettime_ns(0))
    except OverflowError as error:
        print(type(error).__name__, str(error))
""",
    )
    expected = struct.pack(">d", 123456789 * 1e-9).hex()
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        f"{expected} {expected}\n"
        "9223372036854775807\n"
        "-9223372036854775808\n"
        "OverflowError timestamp out of range for platform time_t\n"
        "OverflowError timestamp out of range for platform time_t\n"
    )


@pytest.mark.integration
@pytest.mark.parametrize(
    "operation",
    (
        "time.clock_gettime('clock')",
        "time.clock_getres(-1)",
        "time.clock_getres(Broken())",
    ),
)
def test_clock_builtin_tracebacks_match_python_verbatim(tmp_path, operation):
    from test.mule.compiler.test_builtin_tracebacks import (
        test_builtin_tracebacks_match_python_verbatim,
    )

    test_builtin_tracebacks_match_python_verbatim(
        tmp_path,
        """import time
class Broken:
    def __index__(self):
        raise RuntimeError('index failed')
"""
        + operation
        + "\n",
    )
