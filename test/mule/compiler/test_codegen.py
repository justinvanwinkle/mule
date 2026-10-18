"""Code generation preserves values and ownership through the real runtime."""

import struct

import pytest

from mule import ast
from mule.compiler import emit_llvm
from mule.compiler.codegen import _ConstantKey
from test.mule.compiler.test_native import run_native


def test_constant_pool_keys_preserve_float_bits_and_scalar_types():
    def key(value):
        return ast.Tuple([value]).accept(_ConstantKey())

    assert key(ast.FloatLiteral(0.0)) != key(ast.FloatLiteral(-0.0))
    assert key(ast.BooleanLiteral(True)) != key(ast.IntegerLiteral(1))
    assert key(ast.IntegerLiteral(1)) != key(ast.FloatLiteral(1.0))
    first_nan = struct.unpack(">d", bytes.fromhex("7ff8000000000001"))[0]
    second_nan = struct.unpack(">d", bytes.fromhex("7ff8000000000002"))[0]
    assert key(ast.FloatLiteral(first_nan)) == key(ast.FloatLiteral(first_nan))
    assert key(ast.FloatLiteral(first_nan)) != key(ast.FloatLiteral(second_nan))


@pytest.mark.parametrize(
    ("expression", "output"),
    [
        ("None", "None\n"),
        ("True", "True\n"),
        (f"-{10**100}", "-" + "1" + "0" * 100 + "\n"),
        ("-0.0", "-0.0\n"),
        ("'π\\x00雪'", "π\x00雪\n"),
        ("b'\\x00\\xff'", "b'\\x00\\xff'\n"),
        ("(1 << 20000) > 0", "True\n"),
    ],
    ids=[
        "none",
        "bool",
        "huge-integer",
        "negative-zero",
        "unicode-nul",
        "bytes",
        "integer-beyond-host-conversion-limit",
    ],
)
def test_constants_preserve_values(tmp_path, expression, output):
    result = run_native(tmp_path, f"print({expression})\n")
    assert result.returncode == 0, result.stderr
    assert result.stdout == output


def test_text_constants_preserve_lone_surrogates(tmp_path):
    result = run_native(tmp_path, "print(ascii('\\ud800'))\n")
    assert result.returncode == 0, result.stderr
    assert result.stdout == "'\\ud800'\n"


def test_functions_see_current_globals_at_call_time(tmp_path):
    result = run_native(
        tmp_path,
        "offset = 1\ndef add_offset(x):\n return x + offset\n"
        "offset = 40\nprint(add_offset(2))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42\n"


def test_error_cleanup_preserves_unbound_local_exception(tmp_path):
    result = run_native(tmp_path, "def f():\n print(missing)\n missing = 1\nf()\n")
    assert result.returncode == 1
    assert result.stdout == ""
    assert "UnboundLocalError" in result.stderr
    assert "missing" in result.stderr


@pytest.mark.parametrize("truth", [False, True], ids=["false-edge", "true-edge"])
def test_branches_transfer_selected_value_through_local(tmp_path, truth):
    result = run_native(
        tmp_path,
        f"def choose(c):\n if c:\n  merge = 11\n else:\n  merge = 22\n return merge\n"
        f"print(choose({truth}))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == ("11\n" if truth else "22\n")


def test_moved_temporaries_transfer_references_once(tmp_path):
    result = run_native(
        tmp_path,
        "def run():\n product = 7 * 3\n merge = product\n moved = -merge\n"
        " return moved == -21\nprint(run())\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\n"


def test_text_char_index_and_compare_fusion_emits_codepoint_getitem_and_icmp():
    llvm = emit_llvm(
        "def scan(text):\n"
        " i = 0\n"
        " n = len(text)\n"
        " count = 0\n"
        " while i < n:\n"
        "  ch = text[i]\n"
        "  if ch == '\"' or ch == '[' or ch == ',':\n"
        "   count += 1\n"
        "  elif text[i] == ' ':\n"
        "   count += 2\n"
        "  elif '0' <= text[i] <= '9':\n"
        "   count += 3\n"
        "  i += 1\n"
        " return count\n"
        "scan('\"[ 5,')\n",
        "scan.py",
    )
    body = llvm.split('define internal ptr @"mule_fn_scan.spec.0"', 1)[1].split(
        "\n}\n", 1
    )[0]
    assert "@mule_text_codepoint_getitem(" in body
    assert "icmp eq i64" in body
    assert "icmp sle i64" in body
    assert "@mule.fast.getitem(" not in body
    assert "@mule_compare(" not in body


def test_text_char_index_and_compare_fusion_preserves_semantics(tmp_path):
    result = run_native(
        tmp_path,
        "def check(text: str, i: int):\n"
        " ch = text[i]\n"
        " is_quote = ch == '\"' or ch == '['\n"
        " is_space = text[i] == ' '\n"
        " is_digit = '0' <= text[i] <= '9'\n"
        " is_greek = 'α' <= text[i] <= 'ω'\n"
        " return is_quote, is_space, is_digit, is_greek\n"
        "print(check('\" 5β', 0))\n"
        "print(check('\" 5β', 1))\n"
        "print(check('\" 5β', -2))\n"
        "print(check('\" 5β', -1))\n"
        "for bad in (4, -5):\n"
        " try:\n"
        "  check('\" 5β', bad)\n"
        " except IndexError as exc:\n"
        "  print(type(exc).__name__, exc)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "(True, False, False, False)\n"
        "(False, True, False, False)\n"
        "(False, False, True, False)\n"
        "(False, False, False, True)\n"
        "IndexError string index out of range\n"
        "IndexError string index out of range\n"
    )
