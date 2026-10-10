"""Specialized native operations retain Python value and evaluation semantics."""

import re
import subprocess

import pytest
from test_native import run_native
from test_runtime import runtime_probe as runtime_probe  # noqa: PLC0414 - pytest fixture export

from mule.compiler import build


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        (
            (
                "x = 4611686018427387903\ny = -4611686018427387904\n"
                "print(x + 1, y - 1, (x + 1) - 1, (y - 1) + 1)\n"
            ),
            "4611686018427387904 -4611686018427387905 4611686018427387903 -4611686018427387904\n",
        ),
        (
            "x = 4611686018427387903\nprint(x * 2, (x * 2) // 2, x * x)\n",
            "9223372036854775806 4611686018427387903 21267647932558653957237540927630737409\n",
        ),
        (
            "x = -4611686018427387904\nprint(-x, x // -1, x % -1, ~x)\n",
            "4611686018427387904 4611686018427387904 0 4611686018427387903\n",
        ),
        (
            "print(-7 // 3, -7 % 3, 7 // -3, 7 % -3, -7 // -3, -7 % -3)\n",
            "-3 2 -3 -2 2 -1\n",
        ),
        (
            (
                "x = -4611686018427387904\ny = 4611686018427387903\n"
                "print(x >> 100, x >> 1, 1 << 62, y << 1, (1 << 100) >> 99)\n"
            ),
            "-1 -2305843009213693952 4611686018427387904 9223372036854775806 2\n",
        ),
        (
            (
                "print(True & True, False | False, True ^ False)\n"
                "print((True & True) is True, (False | False) is False, (True ^ False) is True)\n"
                "print(True & 1, False | 0, True ^ 0)\n"
                "print((True & 1) is True, (False | 0) is False, (True ^ 0) is True)\n"
            ),
            "True False True\nTrue True True\n1 0 1\nFalse False False\n",
        ),
        (
            (
                "def first(x):\n return x + 1\ndef second(x):\n return x + 100\n"
                "saved = first\nfirst = second\nprint(first(1), saved(1))\n"
            ),
            "101 2\n",
        ),
        (
            (
                "def first(x):\n return x + 1\nsaved = first\nfirst = 12\n"
                "print(saved(41))\n"
            ),
            "42\n",
        ),
        (
            (
                "def current():\n return current\nsaved = current\ncurrent = 42\n"
                "print(saved())\n"
            ),
            "42\n",
        ),
        (
            (
                "def first(x):\n return x + 1\ndef second(x):\n return x + 100\n"
                "def mutate():\n global first\n first = second\n return 1\n"
                "print(first(mutate()))\nprint(first(1))\n"
            ),
            "2\n101\n",
        ),
        (
            (
                "def first(x):\n return x + 1\ndef other(x):\n return x + 100\n"
                "def choose(flag):\n local = first if flag else other\n return local(2)\n"
                "print(choose(True), choose(False))\n"
            ),
            "3 102\n",
        ),
        (
            (
                "def first(x):\n return x + 1\n"
                "def second(x):\n return x + 100\n"
                "def change():\n global first\n first = second\n"
                "def caller():\n change()\n return first(2)\nprint(caller())\n"
            ),
            "102\n",
        ),
        (
            (
                "def first(x):\n return x + 1\n"
                "def alias_call():\n alias = first\n return alias(41)\nprint(alias_call())\n"
            ),
            "42\n",
        ),
    ],
    ids=[
        "overflow-and-demotion",
        "multiplication-promotion",
        "minimum-negation",
        "signed-floor-and-remainder",
        "signed-and-wide-shifts",
        "boolean-bitwise-types",
        "global-rebind-function",
        "global-rebind-integer-retained-alias",
        "alias-body-loads-current-global",
        "callee-before-rebinding-argument",
        "different-target-join",
        "rebind-during-known-caller",
        "local-function-alias",
    ],
)
def test_specialized_native_values(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("source", "expected_stdout", "error"),
    [
        ("print('before')\nprint(1 << -1)\n", "before\n", "ValueError"),
        ("print('before')\nprint(-1 >> -1)\n", "before\n", "ValueError"),
        ("print('before')\nprint(9 // 0)\n", "before\n", "ZeroDivisionError"),
        ("print('before')\nprint(9 % 0)\n", "before\n", "ZeroDivisionError"),
        (
            (
                "def first(x):\n return x + 1\nfirst = 17\n"
                "def argument():\n print('argument')\n return 1\nfirst(argument())\n"
            ),
            "argument\n",
            "TypeError",
        ),
        (
            (
                "def first(x):\n return x + 1\n"
                "def argument():\n print('argument')\n return 1\n"
                "first(argument(), argument())\n"
            ),
            "argument\nargument\n",
            "TypeError",
        ),
        (
            "def first(x):\n return x + 1\nprint('before')\nfirst()\n",
            "before\n",
            "TypeError",
        ),
        (
            "def recurse(n):\n return recurse(n + 1)\nprint('before')\nrecurse(0)\n",
            "before\n",
            "RecursionError",
        ),
    ],
    ids=[
        "negative-left-shift",
        "negative-right-shift",
        "floor-zero",
        "remainder-zero",
        "rebound-noncallable-argument",
        "extra-arguments-evaluated",
        "missing-argument",
        "guarded-direct-recursion",
    ],
)
def test_specialized_native_errors(tmp_path, source, expected_stdout, error):
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert result.stdout == expected_stdout
    assert error in result.stderr


def test_specialized_runtime_abi(runtime_probe):
    result = subprocess.run(
        [runtime_probe, "specialized_abi"], capture_output=True, text=True, check=False
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "abi=ok\n"
    assert result.stderr == ""


def test_known_fixnum_kernel_executes_machine_arithmetic(tmp_path):
    source = tmp_path / "kernel.py"
    source.write_text(
        "a = bool(input())\nb = bool(input())\n"
        "def kernel():\n x = 17 if a else 23\n y = 5 if b else 7\n return x + y\n"
        "print(kernel())\n"
    )
    executable = tmp_path / "kernel"
    build(source, executable)
    assert executable.read_bytes().startswith(b"\x7fELF")
    source.unlink()
    for stdin, expected in [
        ("\n\n", "30\n"),
        ("yes\n\n", "24\n"),
        ("\nyes\n", "28\n"),
        ("yes\nyes\n", "22\n"),
    ]:
        result = subprocess.run(
            [executable],
            input=stdin,
            text=True,
            capture_output=True,
            check=False,
            timeout=10,
        )
        assert result.returncode == 0, result.stderr
        assert result.stdout == expected
        assert result.stderr == ""
    assembly = subprocess.run(
        ["objdump", "-d", "--no-show-raw-insn", "--disassemble=mule.fn.1", executable],
        text=True,
        capture_output=True,
        check=True,
    ).stdout
    assert "<mule.fn.1>:" in assembly
    # LLVM may keep the shared owned-argument body behind a thin ABI wrapper.
    if "<mule.body.1>" in assembly:
        body = subprocess.run(
            [
                "objdump",
                "-d",
                "--no-show-raw-insn",
                "--disassemble=mule.body.1",
                executable,
            ],
            text=True,
            capture_output=True,
            check=True,
        ).stdout
        assert "<mule.body.1>:" in body
        assembly += body
    assert re.search(r"\b(?:add|lea)[a-z]*\s", assembly), assembly
    for dispatcher in ("mule_binary", "mule_unary", "mule_compare"):
        assert f"<{dispatcher}>" not in assembly, assembly
