"""Exercise the independent tagged-integer/GMP arithmetic runtime."""

import re
import subprocess

import pytest

from mule.compiler import emit_llvm
from mule.compiler.driver import Toolchain
from test.mule.compiler.test_native import run_native


def test_tagged_add_sub_keep_encoded_operands(tmp_path):
    source = tmp_path / "arithmetic.ll"
    source.write_text(
        emit_llvm("def add(x, y): return x + y\ndef sub(x, y): return x - y\n")
    )
    optimized = subprocess.run(
        [Toolchain().llvm_tool("opt"), "-passes=mem2reg,instcombine", "-S", source],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    ).stdout
    for prefix in ("fast", "fixnum"):
        for opcode, intrinsic in ((0, "sadd"), (1, "ssub")):
            body = re.search(
                rf"^define[^\n]*@mule\.{prefix}\.binary\.{opcode}\([^\n]*\)[^\n]*\{{\n(.*?)^\}}",
                optimized,
                re.MULTILINE | re.DOTALL,
            )
            assert body is not None
            assert f"@llvm.{intrinsic}.with.overflow.i64" in body[1]
            assert not re.search(r"\b(?:ashr|shl) i64\b", body[1])


def test_tagged_add_sub_preserve_fixnum_boundaries_and_bigints(tmp_path):
    values = [
        -(2**62) - 1,
        -(2**62),
        -(2**62) + 1,
        -1,
        0,
        1,
        2**62 - 2,
        2**62 - 1,
        2**62,
    ]
    result = run_native(
        tmp_path,
        f"""def add(x, y): return x + y
def sub(x, y): return x - y
values = {values!r}
for left in values:
    for right in values:
        print(add(left, right), sub(left, right))
print(add(True, False), sub(True, False))
print(add('a', 'b'))
""",
    )
    assert result.returncode == 0, result.stderr
    expected = "".join(
        f"{left + right} {left - right}\n" for left in values for right in values
    )
    assert result.stdout == expected + "1 1\nab\n"


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        ("(2 ** 62 - 1) + 1", "4611686018427387904"),
        ("-(2 ** 62) - 1", "-4611686018427387905"),
        ("(2 ** 40) * (2 ** 40)", "1208925819614629174706176"),
        ("(2 ** 80 + 7) - 2 ** 80", "7"),
        ("-(2 ** 62) // -1", "4611686018427387904"),
        ("-7 // 3", "-3"),
        ("7 // -3", "-3"),
        ("-7 // -3", "2"),
        ("-7 % 3", "2"),
        ("7 % -3", "-2"),
        ("-7 % -3", "-1"),
        ("(2 ** 100 + 5) // (2 ** 100)", "1"),
        ("(-2 ** 100 - 5) % (2 ** 100)", "1267650600228229401496703205371"),
        ("~(2 ** 100)", "-1267650600228229401496703205377"),
        ("~(-2 ** 100)", "1267650600228229401496703205375"),
        ("-5 >> 1", "-3"),
        ("(-2 ** 100) >> 99", "-2"),
        ("-7 >> (2 ** 100)", "-1"),
        ("7 >> (2 ** 100)", "0"),
        ("3 << 80", "3626777458843887524118528"),
        ("(-2 ** 100) & 7", "0"),
        ("(-2 ** 100) | 7", "-1267650600228229401496703205369"),
        ("(-2 ** 100) ^ 7", "-1267650600228229401496703205369"),
        ("(-3) ** 3", "-27"),
        ("2 ** 130", "1361129467683753853853498429727072845824"),
        ("(-1) ** (2 ** 100 + 1)", "-1"),
        ("0 ** 0", "1"),
        ("True & False", "False"),
        ("True | False", "True"),
        ("True ^ True", "False"),
        ("True + True", "2"),
        ("+True", "1"),
        ("-True", "-1"),
        ("~True", "-2"),
        ("((2 ** 2000) / (2 ** 2000)) == 1.0", "True"),
        ("(1 / (2 ** 2000)) == 0.0", "True"),
        ("(9007199254740993 / 9007199254740992) == 1.0", "True"),
        ("(9007199254740995 / 9007199254740992) == 1.0000000000000004", "True"),
        ("(1 / (2 ** 1074)) == 5e-324", "True"),
        ("(1 / (2 ** 1075)) == 0.0", "True"),
        ("(3 / (2 ** 1075)) == 1e-323", "True"),
        ("((2 ** 1024 - 2 ** 971) / 1) == 1.7976931348623157e308", "True"),
        ("9007199254740993 > 9007199254740992.0", "True"),
        ("9007199254740993 == 9007199254740992.0", "False"),
        ("(2 ** 2000) > 1e308", "True"),
        ("(2 ** 2000) < 1e309", "True"),
        ("(1e309 - 1e309) == 0.0", "False"),
        ("(1e309 - 1e309) != 0.0", "True"),
        ("(1e309 - 1e309) < 0.0", "False"),
        ("(1e309 - 1e309) >= 0.0", "False"),
        ("(-5.5 % 2.0) == 0.5", "True"),
        ("(5.5 % -2.0) == -0.5", "True"),
        ("(5.0 // 0.1) == 49.0", "True"),
        ("(-5.5 // 2.0) == -3.0", "True"),
        ("(2 ** -3) == 0.125", "True"),
        ("((-2) ** -3) == -0.125", "True"),
        ("((-1e309) ** 0.5) == 1e309", "True"),
    ],
)
def test_native_numeric_values(tmp_path, expression, expected):
    result = run_native(tmp_path, f"print({expression})\n")
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected + "\n"


@pytest.mark.parametrize(
    ("expression", "exception"),
    [
        ("1 // 0", "ZeroDivisionError"),
        ("1 % 0", "ZeroDivisionError"),
        ("1 / 0", "ZeroDivisionError"),
        ("1.0 // 0.0", "ZeroDivisionError"),
        ("1.0 % 0.0", "ZeroDivisionError"),
        ("0 ** -1", "ZeroDivisionError"),
        ("1 << -1", "ValueError"),
        ("1 >> -1", "ValueError"),
        ("1.0 & 1", "TypeError"),
        ("~1.0", "TypeError"),
        ("(2 ** 2000) / 1", "OverflowError"),
    ],
)
def test_native_numeric_errors(tmp_path, expression, exception):
    result = run_native(tmp_path, f"print({expression})\n")
    assert result.returncode != 0
    assert exception in result.stderr


def test_complex_literals_preserve_components_and_ignore_constructor_rebinding(
    tmp_path,
):
    result = run_native(
        tmp_path,
        "complex_type = type(1j)\n"
        "complex = None\n"
        "def values(complex):\n"
        " return [0j, 1j, 2.5J, 1e3j, 1e-999j, 1e999j, -0j, -2j]\n"
        "items = values(None)\n"
        "print(all(type(value) is complex_type for value in items))\n"
        "print([value.real == 0.0 for value in items])\n"
        "print([value.imag for value in items] == [0.0, 1.0, 2.5, 1000.0, 0.0, float('inf'), -0.0, -2.0])\n"
        "print(str(items[-2].real).startswith('-'), str(items[-2].imag).startswith('-'))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "True\n[True, True, True, True, True, True, True, True]\nTrue\nTrue True\n"
    )
