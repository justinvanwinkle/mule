"""Exact-integer string conversion through numeric log construction."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_LOG_SOURCE = """
def numeric_log(record, previous):
    count = len(record) - len(previous)
    return "count=" + str(count)
"""


def test_numeric_log_flow_proven_integer_uses_decimal_protocol():
    compilation = compile_source(_LOG_SOURCE)
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "numeric_log"
    )
    assert list(known_calls(function, "_decimal_protocol"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "source",
    [
        'def numeric_log(record):\n return "flag=" + str(bool(record))\n',
        'def numeric_log(value):\n return "value=" + str(value)\n',
        'def numeric_log(record, str):\n return "count=" + str(len(record))\n',
        (
            'def str(value):\n return "custom"\n'
            'def numeric_log(record):\n return "count=" + str(len(record))\n'
        ),
    ],
    ids=["bool", "unknown", "parameter-shadow", "global-shadow"],
)
def test_numeric_log_non_exact_integer_or_shadow_keeps_fallback(source):
    compilation = compile_source(source)
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "numeric_log"
    )
    assert not list(known_calls(function, "_decimal_protocol"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_numeric_log_decimal_loop_executes_native_digit_work(tmp_path):
    source = (
        _LOG_SOURCE
        + """
import sys
record = sys.argv[1]
previous = sys.argv[2]
saved = numeric_log(record, previous)
print(saved)
print(numeric_log(previous, record))
print(saved)

# Runtime dependence prevents the boundary conversions from folding to text.
n = len(record)
print("nonnegative=" + str(n))
print("tagged-max=" + str(n + 4611686018427387902))
print("tagged-min=" + str(-n - 4611686018427387903))
print("i64-max=" + str(n + 9223372036854775806))
print("i64-min=" + str(-n - 9223372036854775807))
print("big=" + str(n + 1267650600228229401496703205375))
print("negative-big=" + str(-n - 1267650600228229401496703205375))
"""
    )
    case = build_case(tmp_path, source)
    for record, previous in [("", ""), ("x", ""), ("", "long"), ("abc", "x")]:
        case.compare(args=(record, previous))

    caller = case.function_ir(r"mule_fn_numeric_log\.body$")
    assert "mule_format_decimal_integer" in caller
    assert "mule_format_string" not in caller

    # Conversion runs in the separately compiled builtins unit. Merely selecting
    # decimal_integer in the caller would miss a generic renderer inside it.
    decimal = case.function_ir(r"mule_format_decimal_integer.*body$", unit="builtins")
    assert re.search(r"\b[su]div i64 [^\n]*, 10\b", decimal), decimal
    assert re.search(r"\b[su]rem i64 [^\n]*, 10\b", decimal), decimal
    assert "mule_text_character" in decimal
    assert " phi " in decimal  # The runtime-dependent digit loop remains.
    assert "mule_format_string" not in decimal
    assert "mule_render_string" not in decimal
    assert "mule_resolve_method" not in decimal


@pytest.mark.integration
def test_numeric_log_fallbacks_preserve_conversion_effects_and_errors(tmp_path):
    source = """
import sys

events = []

class Field:
    def __init__(self, text):
        self.text = text

    def __str__(self):
        events.append("convert")
        return self.text

class Broken:
    def __str__(self):
        events.append("broken")
        return 42

class Raised:
    def __str__(self):
        events.append("raised")
        raise ValueError("conversion")

def numeric_log(value):
    return "value=" + str(value)

def shadowed(record, str):
    return "count=" + str(len(record))

def custom(value):
    events.append("custom")
    return "replacement"

record = sys.argv[1]
print(numeric_log(bool(record)))
print(numeric_log(Field(record)))
print(numeric_log(None))
print(shadowed(record, custom))
for value in (Broken(), Raised()):
    try:
        print(numeric_log(value))
    except TypeError:
        print("TypeError")
    except ValueError:
        print("ValueError")
print(events)
"""
    case = build_case(tmp_path, source)
    case.compare(args=("",))
    case.compare(args=("runtime-field",))
