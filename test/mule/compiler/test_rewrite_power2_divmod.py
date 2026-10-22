"""Signed bucket calculations must reduce division without truncating negatives."""

import re

import pytest

from mule.compiler.ir1opt import constant_lvar_value
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("divisor", [2, 8, 1024, 1 << 70])
def test_bucket_divmod_positive_power_rewrites_runtime_integer(divisor):
    compilation = compile_source(
        "def bucket(raw):\n"
        " value = int(raw)\n"
        f" return value // {divisor}, value % {divisor}\n"
    )
    bucket = next(fn for fn in compilation.lambdas if fn.source_name == "bucket")
    assert not list(known_calls(bucket, "%floor_divide"))
    assert not list(known_calls(bucket, "%modulo"))
    shifts = list(known_calls(bucket, "%right_shift"))
    masks = list(known_calls(bucket, "%bit_and"))
    assert shifts and masks
    assert constant_lvar_value(shifts[0].args[0]) is None
    assert constant_lvar_value(masks[0].args[0]) is None
    assert constant_lvar_value(shifts[0].args[1]).value == divisor.bit_length() - 1
    assert constant_lvar_value(masks[0].args[1]).value == divisor - 1


@pytest.mark.parametrize("divisor", [0, 3, 6, -8])
def test_bucket_divmod_nonpositive_or_nonpower_keeps_language_operations(divisor):
    compilation = compile_source(
        "def bucket(raw):\n"
        " value = int(raw)\n"
        f" return value // {divisor}, value % {divisor}\n"
    )
    bucket = next(fn for fn in compilation.lambdas if fn.source_name == "bucket")
    assert list(known_calls(bucket, "%floor_divide"))
    assert list(known_calls(bucket, "%modulo"))
    assert not list(known_calls(bucket, "%right_shift"))
    assert not list(known_calls(bucket, "%bit_and"))


@pytest.mark.integration
def test_signed_bucket_native_shift_and_mask_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "import mule\n"
        "def signed_bucket(value: mule.int64) -> mule.int64:\n"
        " return value // 8\n"
        "def bucket_offset(value: mule.int64) -> mule.int64:\n"
        " return value % 8\n"
        "value = int(input())\n"
        "print(signed_bucket(value), bucket_offset(value))\n",
    )
    for value in [-9223372036854775808, -65, -64, -9, -8, -1, 0, 1, 7, 8, 65]:
        case.compare(stdin=f"{value}\n")

    # Inspect the actual callable bodies, including their inlined arithmetic.
    # A selected helper alone must not satisfy this efficiency assertion.
    quotient = case.function_ir(r"mule_fn_signed_bucket\.")
    remainder = case.function_ir(r"mule_fn_bucket_offset\.")
    assert re.search(r"\bashr i64 [^\n]+, (?:3|4)\b", quotient), quotient
    assert re.search(r"\band i64 [^\n]+, (?:7|14)\b", remainder), remainder
    for body in (quotient, remainder):
        assert not re.search(r"\b(?:sdiv|srem|udiv|urem)\b", body), body
        assert not re.search(r"mule\.(?:fast|fixnum)\.binary\.[45]\b", body), body
        assert not re.search(r"@mule_binary(?:_reuse)?\(i32 [45]\b", body), body


@pytest.mark.integration
def test_bucket_divmod_bigints_and_divisor_boundaries_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "def buckets(value):\n"
        " return (value // 8, value % 8, value // 1, value % 1,\n"
        "         value // 6, value % 6, value // -8, value % -8,\n"
        "         value // 1180591620717411303424,\n"
        "         value % 1180591620717411303424)\n"
        "value = int(input())\n"
        "print(buckets(value))\n"
        "try:\n"
        " print(value // 0)\n"
        "except ZeroDivisionError:\n"
        " print('floor zero')\n"
        "try:\n"
        " print(value % 0)\n"
        "except ZeroDivisionError:\n"
        " print('modulo zero')\n",
    )
    for value in [-((1 << 140) + 9), -(1 << 70), -9, -8, -1, 0, 1, 8, (1 << 140) + 9]:
        case.compare(stdin=f"{value}\n")


@pytest.mark.integration
def test_bucket_divmod_evaluation_and_overridden_operators_match_python(tmp_path):
    case = build_case(
        tmp_path,
        "class Coordinate:\n"
        " def __init__(self, value):\n"
        "  self.value = value\n"
        " def __floordiv__(self, divisor):\n"
        "  print('floor', divisor)\n"
        "  return self.value + divisor\n"
        " def __mod__(self, divisor):\n"
        "  print('modulo', divisor)\n"
        "  return self.value - divisor\n"
        "def source(value):\n"
        " print('source', value)\n"
        " return value\n"
        "def bucket(value):\n"
        " return value // 8, value % 8\n"
        "value = int(input())\n"
        "print(bucket(Coordinate(value)))\n"
        "print(source(value) // 8, source(value) % 8)\n"
        "print(source(value) // 1, source(value) % 1)\n"
        "try:\n"
        " print(source(value) // source(0))\n"
        "except ZeroDivisionError:\n"
        " print('zero after operands')\n"
        "print(bucket(True))\n"
        "print(bucket(-9.5))\n",
    )
    case.compare(stdin="-9\n")
