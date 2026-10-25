"""Ordinary int annotations constrain values without choosing machine widths."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native

PREFIX = "from mule_types import int64, optimize\n@optimize(speed=3, safety=3)\n"


def native_bodies(source):
    return re.findall(
        r"^define internal (i64|i1) @mule\.native\.\d+\([^\n]+\) \{\n(.*?)^\}",
        emit_llvm(source),
        re.MULTILINE | re.DOTALL,
    )


@pytest.mark.parametrize(
    ("body", "expected"),
    (
        (" value: int = 40\n value += 2\n return value\n", "42\n"),
        (
            " index: int = 0\n while index < 100:\n  index += 1\n return index\n",
            "100\n",
        ),
    ),
)
def test_builtin_int_locals_use_raw_storage_when_range_is_proven(
    tmp_path, body, expected
):
    source = PREFIX + "def calculate() -> int:\n" + body + "print(calculate())\n"
    bodies = native_bodies(source)
    assert bodies
    assert all(kind == "i64" for kind, _ in bodies)
    assert all("@mule_assert_integer" not in body for _, body in bodies)
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_boolean_int_local_preserves_boolean_storage_and_identity(tmp_path):
    source = (
        PREFIX + "def calculate() -> int:\n value: int = True\n return value\n"
        "print(calculate(), calculate() is True)\n"
    )
    bodies = native_bodies(source)
    assert bodies and all(kind == "i1" for kind, _ in bodies)
    assert all("alloca i64" not in body for _, body in bodies)
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\n"


@pytest.mark.parametrize(
    ("function", "call", "expected"),
    (
        (
            "def calculate(value: int) -> int:\n return value\n",
            str(2**100),
            f"{2**100}\n",
        ),
        (
            "def calculate(value: int) -> int:\n return value\n",
            "True",
            "True\n",
        ),
        (
            (
                "def calculate() -> int:\n value: int = 9223372036854775807\n"
                " value += 1\n return value\n"
            ),
            "",
            "9223372036854775808\n",
        ),
        (
            (
                "def calculate() -> int:\n value: int = 9223372036854775807\n"
                " result: int = (value + 1) - 1\n return result\n"
            ),
            "",
            "9223372036854775807\n",
        ),
        (
            "def calculate() -> int:\n value: int = True\n value = 2\n return value\n",
            "",
            "2\n",
        ),
    ),
)
def test_unbounded_or_mixed_int_values_keep_general_entries(
    tmp_path, function, call, expected
):
    source = PREFIX + function + f"print(calculate({call}))\n"
    assert not native_bodies(source)
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_dynamic_int64_loop_bound_does_not_assume_int_local_width(tmp_path):
    proven = (
        PREFIX + "def calculate(limit: int64) -> int:\n index: int = 0\n"
        " while index < limit:\n  index += 1\n return index\nprint(calculate(10))\n"
    )
    assert native_bodies(proven)
    result = run_native(tmp_path, proven)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "10\n"

    # An inclusive <= bound allows index == INT64_MAX inside the loop, where
    # index + 1 overflows int64, so preserving Python integer semantics takes
    # precedence over selecting a machine representation.
    unproven = (
        PREFIX + "def calculate(limit: int64) -> int:\n index: int = 0\n"
        " while index <= limit:\n  index += 1\n return index\nprint(calculate(9))\n"
    )
    assert not native_bodies(unproven)
    result = run_native(tmp_path, unproven)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "10\n"
