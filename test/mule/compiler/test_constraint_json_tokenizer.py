"""Tests for per-variable loop widening, max/min/abs builtin type derivation,
and direct truthiness branch narrowing in constraint propagation."""

import pytest

from mule.compiler.annotation_types import AnnotationLedger
from mule.compiler.constraint import propagate
from mule.compiler.ctype import (
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    FLOAT,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    OptionalIntegerType,
)
from mule.compiler.main import compile_source
from mule.compiler.node import Set


def lower(source: str):
    return compile_source(source)


def function(compilation, index: int):
    return compilation.lambdas[index]


def nodes(lambda_):
    return [node for block in lambda_.component.blocks() for node in block.nodes()]


def stored_value(lambda_, name: str):
    return next(
        node.value
        for node in nodes(lambda_)
        if isinstance(node, Set) and node.var.name == name
    )


def fact(compilation, lambda_, node):
    return compilation.facts[lambda_].types[node]


def test_per_variable_loop_widening_preserves_bounded_flag_in_scan_payload():
    compilation = lower(
        "def scan_payload(raw):\n"
        " text = str(raw)\n"
        " n = len(text)\n"
        " i = 0\n"
        " active = 0\n"
        " count = 0\n"
        " while i < n:\n"
        "  ch = text[i]\n"
        "  if ch == ' ':\n"
        "   i = i + 1\n"
        "  elif ch == ',':\n"
        "   i = i + 1\n"
        "  elif ch == ':':\n"
        "   i = i + 1\n"
        "  elif ch == 't' and text[i : i + 4] == 'true':\n"
        "   active = 1\n"
        "   i = i + 4\n"
        "  else:\n"
        "   count = count + active\n"
        "   i = i + 1\n"
        " result = active\n"
        " return count + result\n"
    )
    fn = function(compilation, 1)
    active_type = fact(compilation, fn, stored_value(fn, "result").use())
    assert active_type == IntegerType(0, 1)
    assert active_type.is_fixnum is True


@pytest.mark.parametrize(
    ("expr", "expected", "fixnum"),
    [
        ("max(1, x)", IntegerType(1, None), False),
        ("max(1, x & 255)", IntegerType(1, 255), True),
        ("min(100, x)", IntegerType(None, 100), False),
        ("min(100, max(0, x))", IntegerType(0, 100), True),
        ("abs(x)", IntegerType(0, None), False),
        ("abs((x & 15) - 10)", IntegerType(0, 10), True),
        ("abs((x & 7) + 3)", IntegerType(3, 10), True),
        ("abs(-((x & 7) + 3))", IntegerType(3, 10), True),
    ],
)
def test_max_min_abs_integer_interval_derivation(expr, expected, fixnum):
    compilation = lower(f"def f(x: int):\n out = {expr}\n return out\n")
    fn = function(compilation, 1)
    result = fact(compilation, fn, stored_value(fn, "out").use())
    assert result == expected
    assert result.is_fixnum is fixnum


def test_max_min_abs_float_and_text_derivation():
    compilation = lower(
        "def f(x, y):\n"
        " s1 = str(x)\n"
        " s2 = str(y)\n"
        " a = 1.5\n"
        " b = -2.5\n"
        " f_abs = abs(b)\n"
        " f_max = max(a, b)\n"
        " f_min = min(a, b)\n"
        " s_max = max(s1, s2)\n"
        " s_min = min(s1, s2)\n"
        " return f_abs, f_max, f_min, s_max, s_min\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "f_abs").use()) is FLOAT
    assert fact(compilation, fn, stored_value(fn, "f_max").use()) is FLOAT
    assert fact(compilation, fn, stored_value(fn, "f_min").use()) is FLOAT
    assert fact(compilation, fn, stored_value(fn, "s_max").use()) is TEXT_TYPE
    assert fact(compilation, fn, stored_value(fn, "s_min").use()) is TEXT_TYPE


def test_direct_truthiness_branch_narrowing_on_int():
    compilation = lower(
        "def f(n: int):\n"
        " x = n & 7\n"
        " y = -(n & 7)\n"
        " if x:\n"
        "  pos = x\n"
        " else:\n"
        "  zero_x = x\n"
        " if y:\n"
        "  neg = y\n"
        " else:\n"
        "  zero_y = y\n"
        " return 0\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "pos").use()) == IntegerType(1, 7)
    assert fact(compilation, fn, stored_value(fn, "zero_x").use()) == IntegerType(0, 0)
    assert fact(compilation, fn, stored_value(fn, "neg").use()) == IntegerType(-7, -1)
    assert fact(compilation, fn, stored_value(fn, "zero_y").use()) == IntegerType(0, 0)


def test_direct_truthiness_branch_narrowing_on_bool():
    compilation = lower(
        "def f(a: int, b: int):\n"
        " flag = a < b\n"
        " if flag:\n"
        "  t = flag\n"
        " else:\n"
        "  f_val = flag\n"
        " return 0\n"
    )
    fn = function(compilation, 1)
    assert fact(compilation, fn, stored_value(fn, "t").use()) == BOOLEAN_TRUE
    assert fact(compilation, fn, stored_value(fn, "f_val").use()) == BOOLEAN_FALSE


def test_direct_truthiness_branch_narrowing_on_optional_int():
    compilation = lower(
        "def f(x):\n if x:\n  present = x\n  return present\n return 0\n"
    )
    fn = function(compilation, 1)
    body = propagate(
        fn,
        {},
        [OptionalIntegerType(IntegerType(1, 10))],
        lambda *_args: UNKNOWN,
        AnnotationLedger(),
    )
    present = body.types()[stored_value(fn, "present").use()]
    assert present == IntegerType(1, 10)
    assert present.is_fixnum is True
