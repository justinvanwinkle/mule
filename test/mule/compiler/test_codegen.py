"""Code generation preserves values and ownership through the real runtime."""

import pytest

from test.mule.compiler.test_native import run_native


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
