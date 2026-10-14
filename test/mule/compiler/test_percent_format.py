"""Compare decimal percent formatting and numeric remainder with Python."""

import subprocess

import pytest

from test.mule.compiler.test_native import run_native
from test.python_host import python_host


def assert_matches_python(tmp_path, source):
    reference = subprocess.run(
        [python_host(), "-c", source],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert reference.returncode == 0, reference.stderr
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == reference.stdout
    assert result.stderr == ""


def test_pidigits_prefix_and_decimal_count_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "print('3141592653', end='')\ni = 10\nprint('\\t:%d' % i)\n",
    )


@pytest.mark.parametrize(
    ("pattern", "operand"),
    [
        ("%d", "0"),
        ("value:%d!", "-42"),
        ("%d", "2 ** 100"),
        ("%d", "-(2 ** 100)"),
        ("%d", "True"),
        ("%d", "False"),
        ("%d", "(42,)"),
        ("%d/%d/%d", "(1, -2, 2 ** 100)"),
        ("[%d] %% complete", "(42,)"),
        ("literal %% text", "()"),
        ("literal text", "()"),
        ("", "()"),
    ],
)
def test_decimal_percent_format_matches_python(tmp_path, pattern, operand):
    assert_matches_python(tmp_path, f"print({pattern!r} % {operand})\n")


def test_dynamic_text_percent_format_matches_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "def render(pattern, value):\n return pattern % value\n"
        "print(render('%d', 42))\nprint(render('count:%d%%', -7))\n",
    )


@pytest.mark.parametrize(
    ("pattern", "operand", "exception"),
    [
        ("%d", "()", "TypeError"),
        ("%d %d", "(1,)", "TypeError"),
        ("%d", "(1, 2)", "TypeError"),
        ("literal", "1", "TypeError"),
        ("%%", "1", "TypeError"),
        ("%d", "'not numeric'", "TypeError"),
        ("%d", "None", "TypeError"),
        ("%", "1", "ValueError"),
        ("%q", "1", "ValueError"),
    ],
)
def test_percent_format_errors_recover_like_python(
    tmp_path, pattern, operand, exception
):
    assert_matches_python(
        tmp_path,
        f"try:\n print({pattern!r} % {operand})\n"
        f"except {exception}:\n print('caught {exception}')\n"
        "print('%d' % 42)\n",
    )


@pytest.mark.parametrize(
    "expression",
    ["-7 % 3", "7 % -3", "-7 % -3", "7.5 % 3.0", "-7.5 % 3.0", "7.5 % -3.0"],
)
def test_numeric_modulo_matches_python(tmp_path, expression):
    assert_matches_python(tmp_path, f"print({expression})\n")


def test_numeric_inplace_modulo_matches_python(tmp_path):
    assert_matches_python(tmp_path, "value = -7\nvalue %= 3\nprint(value)\n")


def test_dynamic_numeric_modulo_and_zero_division_match_python(tmp_path):
    assert_matches_python(
        tmp_path,
        "def remainder(left, right):\n return left % right\n"
        "operation = remainder\n"
        "print(operation(-7, 3), operation(7.5, -3.0))\n"
        "try:\n operation(42, 0)\n"
        "except ZeroDivisionError:\n print('caught')\n",
    )
