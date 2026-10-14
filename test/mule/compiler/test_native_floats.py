"""Explicit float64 contracts select unboxed binary64 storage in native entries."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native

HEADER = "from mule import float64, int64, optimize\n@optimize(speed=3, safety=3)\n"
SCALE = (
    HEADER + "def scale(x: float64, y: float64) -> float64:\n"
    " total: float64 = x * 2.5 + y\n"
    " if total < 0.0:\n  total = -total\n"
    " return total / 2.0\n"
)


def native_bodies(llvm):
    return re.findall(
        r"^define internal ([^\n]+@mule\.native\.\d+\([^\n]+) \{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )


def test_float_parameters_use_double_arithmetic_without_dispatch():
    bodies = native_bodies(emit_llvm(SCALE + "print(scale(1.0, 2.0))\n"))
    assert bodies, "scale must have a native entry"
    signature, body = bodies[0]
    assert signature.startswith("double ")
    assert "double %parameter.0, double %parameter.1" in signature
    for opcode in ("fmul", "fadd", "fneg", "fdiv", "fcmp olt"):
        assert re.search(rf"\b{opcode} (?:double|olt double)?", body), opcode
    for helper in ("mule_binary", "mule_unary", "mule_compare", "mule_assert_float"):
        assert not re.search(rf"\bcall\b[^\n]*@{helper}\w*\(", body)


@pytest.mark.parametrize(
    ("call", "expected"),
    [
        ("scale(1.0, 2.0)", "2.25\n"),
        ("scale(-4.0, 1.0)", "4.5\n"),
        ("scale(0.1, 0.2)", "0.225\n"),
        ("scale(1e308, 1e308)", "inf\n"),
    ],
)
def test_float_native_entry_matches_python_arithmetic(tmp_path, call, expected):
    result = run_native(tmp_path, SCALE + f"print({call})\n")
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_mixed_integer_and_float_operands_convert_before_operating(tmp_path):
    source = (
        HEADER + "def mix(count: int64, weight: float64) -> float64:\n"
        " if count > weight:\n  return count * weight - 1\n"
        " return weight + count\n"
        "print(mix(3, 0.5), mix(2, 7.25), mix(-9007199254740993, 1.5))\n"
    )
    llvm = emit_llvm(source)
    assert any("sitofp i64" in body for _, body in native_bodies(llvm))
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "0.5 9.25 -9007199254740990.0\n"


def test_float_division_by_zero_raises_in_native_entry(tmp_path):
    result = run_native(
        tmp_path,
        HEADER + "def ratio(a: float64, b: float64) -> float64:\n return a / b\n"
        "print(ratio(1.0, 4.0))\nprint(ratio(1.0, 0.0))\n",
    )
    assert result.returncode == 1
    assert result.stdout == "0.25\n"
    assert "ZeroDivisionError" in result.stderr


def test_float_entry_rejects_integer_argument_with_type_error(tmp_path):
    result = run_native(
        tmp_path,
        HEADER + "def half(x: float64) -> float64:\n return x / 2\n"
        "print(half(3.0))\nprint(half(3))\n",
    )
    assert result.returncode == 1
    assert result.stdout == "1.5\n"
    assert "float" in result.stderr


def test_narrow_float_widths_keep_boxed_bodies():
    source = (
        "from mule import float32, optimize\n@optimize(speed=3, safety=3)\n"
        "def half(x: float32) -> float32:\n return x / 2\nprint(half(3.0))\n"
    )
    assert not native_bodies(emit_llvm(source))
