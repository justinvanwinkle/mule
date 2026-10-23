"""Specialized native operations retain Python value and evaluation semantics."""

import re
import subprocess

import pytest

from mule.compiler import build
from test.mule.compiler.test_native import run_native
from test.mule.compiler.test_runtime import runtime_probe as runtime_probe  # noqa: PLC0414 - pytest fixture export


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


@pytest.mark.integration
@pytest.mark.parametrize(
    ("optimize", "lto"),
    [(1, False), (3, False), (3, True)],
    ids=["O1-no-lto", "O3-no-lto", "O3-lto"],
)
def test_known_fixnum_kernel_executes_machine_arithmetic(tmp_path, optimize, lto):
    source = tmp_path / "kernel.py"
    source.write_text(
        "a = bool(input())\nb = bool(input())\n"
        "def kernel():\n x = 17 if a else 23\n y = 5 if b else 7\n return x + y\n"
        "print(kernel())\n"
    )
    executable = tmp_path / "kernel"
    build(source, executable, optimize=optimize, lto=lto, workers=1)
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
        [
            "objdump",
            "-d",
            "--no-show-raw-insn",
            "--disassemble=mule_fn_kernel.fn",
            executable,
        ],
        text=True,
        capture_output=True,
        check=True,
    ).stdout
    assert "<mule_fn_kernel.fn>:" in assembly
    # LLVM may keep the shared owned-argument body behind a thin ABI wrapper.
    if "<mule_fn_kernel.body>" in assembly:
        body = subprocess.run(
            [
                "objdump",
                "-d",
                "--no-show-raw-insn",
                "--disassemble=mule_fn_kernel.body",
                executable,
            ],
            text=True,
            capture_output=True,
            check=True,
        ).stdout
        assert "<mule_fn_kernel.body>:" in body
        assembly += body
    assert re.search(r"\b(?:add|lea)[a-z]*\s", assembly), assembly
    for dispatcher in ("mule_binary", "mule_unary", "mule_compare"):
        assert f"<{dispatcher}>" not in assembly, assembly


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        (
            (
                "def add(x, y):\n return x + y\n"
                "print(add(3, 4), add(1.5, 2), add(True, 4), add(4, False))\n"
                "print(add(4611686018427387903, 1), add(2 ** 100, 2), add(3, 4))\n"
            ),
            "7 3.5 5 4\n4611686018427387904 1267650600228229401496703205378 7\n",
        ),
        (
            (
                "def bits(x, y):\n return x ^ y\n"
                "print(bits(True, False), bits(True, 0), bits(1, False))\n"
                "print(bits(True, False) is True, bits(True, 0) is True)\n"
            ),
            "True 1 1\nTrue False\n",
        ),
        (
            (
                "def recover(x):\n return (x + 1) - 1\n"
                "print(recover(17), recover(4611686018427387903), recover(2 ** 100))\n"
            ),
            "17 4611686018427387903 1267650600228229401496703205376\n",
        ),
        (
            (
                "def negate(x):\n return -x\n"
                "print(negate(17), negate(-4611686018427387904), negate(1.25), negate(True))\n"
            ),
            "-17 4611686018427387904 -1.25 -1\n",
        ),
        (
            (
                "def grow(x, n):\n if n == 0:\n  return x\n return grow(x * 2, n - 1)\n"
                "print(grow(1152921504606846976, 5))\nprint(grow(3, 3))\n"
            ),
            "36893488147419103232\n24\n",
        ),
        (
            (
                "def descend(x, n):\n if n == 0:\n  return x\n return descend(x // 2, n - 1)\n"
                "print(descend(36893488147419103232, 4))\n"
            ),
            "2305843009213693952\n",
        ),
        (
            (
                "def add(x, y):\n return x + y\n"
                "def caller(x):\n local = add\n return local(x, 3)\n"
                "print(caller(4), caller(1.5), caller(2 ** 100))\n"
            ),
            "7 4.5 1267650600228229401496703205379\n",
        ),
        (
            (
                "def add(x, y):\n return x + y\n"
                "def multiply(x, y):\n return x * y\n"
                "def caller(flag, x):\n local = add if flag else multiply\n return local(x, 3)\n"
                "print(caller(True, 4), caller(False, 4), caller(True, 1.5))\n"
            ),
            "7 12 4.5\n",
        ),
        (
            (
                "def compute(x, y):\n return x + y\n"
                "def replacement(x, y):\n return x * y\n"
                "def first():\n global compute\n print('first')\n compute = replacement\n return 4\n"
                "def second():\n print('second')\n return 3\n"
                "print(compute(first(), second()))\nprint(compute(4, 3))\n"
            ),
            "first\nsecond\n7\n12\n",
        ),
        (
            (
                "def calculate(x):\n y = x + 1\n print('calculated', y)\n return y - 1\n"
                "print(calculate(4611686018427387903))\n"
            ),
            "calculated 4611686018427387904\n4611686018427387903\n",
        ),
        (
            (
                "def choose(flag):\n if flag:\n  x = 4\n else:\n  x = 9\n return x\n"
                "def transform(x):\n return x * 3 - 2\n"
                "print(transform(choose(True)), transform(choose(False)))\n"
            ),
            "10 25\n",
        ),
        (
            (
                "def give(x):\n return x + 1\n"
                "def wrapper(x):\n return give(x)\n"
                "def give_bool(x):\n return True\n"
                "print(wrapper(2))\ngive = give_bool\nprint(wrapper(2) is True)\n"
            ),
            "3\nTrue\n",
        ),
        (
            (
                "def add(x, y):\n return x + y\n"
                "def invoke(callable, x, y):\n return callable(x, y)\n"
                "print(invoke(add, 3, 4), invoke(add, 1.5, 2))\n"
            ),
            "7 3.5\n",
        ),
        (
            (
                "def shrink(xs):\n"
                " if len(xs) <= 1:\n"
                "  return list(xs)\n"
                " return [xs[0]] + shrink(list(xs[1:]))\n"
                "print(shrink('abc'))\n"
            ),
            "['a', 'b', 'c']\n",
        ),
    ],
    ids=[
        "parameter-numeric-kinds",
        "parameter-bool-bitwise-types",
        "parameter-overflow-demotion",
        "parameter-negation-minimum",
        "recursive-promotion",
        "recursive-demotion",
        "local-call-numeric-kinds",
        "local-call-different-targets",
        "parameter-callee-before-arguments",
        "promotion-before-visible-side-effect",
        "return-type-through-local-call",
        "global-rebind-changes-return-representation",
        "callable-parameter-general-path",
        "recursive-mixed-sequence-domains",
    ],
)
def test_specialized_parameter_behavior(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("source", "expected_stdout", "error"),
    [
        (
            "def add(x, y):\n return x + y\nprint(add(3, 4))\nprint(add('bad', 4))\n",
            "7\n",
            "TypeError",
        ),
        (
            "def divide(x, y):\n return x // y\nprint(divide(9, 2))\nprint(divide(9, 0))\n",
            "4\n",
            "ZeroDivisionError",
        ),
        (
            (
                "def selected(flag, x):\n if flag:\n  y = x + 1\n return y\n"
                "print(selected(True, 3))\nprint(selected(False, 3))\n"
            ),
            "4\n",
            "UnboundLocalError",
        ),
        (
            "def recurse(n):\n return recurse(n + 1)\nprint('before')\nrecurse(0)\n",
            "before\n",
            "RecursionError",
        ),
    ],
    ids=[
        "parameter-nonnumeric-fallback",
        "parameter-zero-divisor",
        "parameter-conditional-unbound",
        "parameter-recursion-guard",
    ],
)
def test_specialized_parameter_errors(tmp_path, source, expected_stdout, error):
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert result.stdout == expected_stdout
    assert error in result.stderr


@pytest.mark.integration
def test_bounded_parameter_body_uses_primitive_arithmetic(tmp_path):
    source = tmp_path / "parameter_kernel.py"
    source.write_text(
        "def chain(x, y):\n return (x + y) * 3 - x\n"
        "def choose(a, b):\n x = 17 if a else 23\n y = 5 if b else 7\n return chain(x, y)\n"
        "print(choose(bool(input()), bool(input())))\n"
        "print(chain(int(input()), int(input())))\n"
    )
    executable = tmp_path / "parameter_kernel"
    llvm = tmp_path / "parameter_kernel.ll"
    build(source, executable, llvm_output=llvm)
    assert executable.read_bytes().startswith(b"\x7fELF")
    source.unlink()
    for stdin, expected in [
        ("\n\n4\n3\n", "67\n17\n"),
        ("yes\n\n4611686018427387903\n1\n", "55\n9223372036854775809\n"),
        ("\nyes\n-4611686018427387904\n-1\n", "61\n-9223372036854775811\n"),
        (
            "yes\nyes\n1267650600228229401496703205376\n1\n",
            "49\n2535301200456458802993406410755\n",
        ),
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
    bodies = re.findall(
        r'define[^\n]*@"(mule_fn_chain\.spec\.\d+)"\([^\n]*\)[^\n]*\{\n(.*?)\n\}',
        llvm.read_text(),
        re.DOTALL,
    )
    assert bodies, "parameter calls must have a guarded specialized body"
    # The bounded variant proves all intermediate values fit. The full fixnum
    # variant may retain overflow recovery, while the public body stays generic.
    bounded = [
        (symbol, body)
        for symbol, body in bodies
        if not any(
            dispatcher in body
            for dispatcher in (
                "@mule_binary(",
                "@mule_binary_reuse(",
                "@mule.fast.binary.",
                "@mule.fixnum.binary.",
            )
        )
    ]
    assert bounded, "bounded parameter arithmetic must avoid operation dispatch"
    assert any(
        re.search(r"\badd i64\b", body)
        and re.search(r"\bmul i64\b", body)
        and re.search(r"\bsub i64\b", body)
        for _, body in bounded
    ), bounded
    for symbol, body in bounded:
        # Primitive operations use the proven parameter representation directly.
        # Ownership cleanup may still inspect tags of values in generic slots.
        assert "@mule.fast.binary." not in body, body
        assembly = subprocess.run(
            [
                "objdump",
                "-d",
                "--no-show-raw-insn",
                f"--disassemble={symbol}",
                executable,
            ],
            capture_output=True,
            text=True,
            check=True,
        ).stdout
        # An optimized body may inline entirely; the LLVM proof still applies.
        # When retained, inspect its actual arithmetic and helper-free kernel.
        if f"<{symbol}>:" in assembly:
            assert re.search(r"\b(?:add|lea)[a-z]*\s", assembly), assembly
            assert re.search(r"\bsub[a-z]*\s", assembly), assembly
            for dispatcher in ("mule_binary", "mule.fast.binary", "mule.fixnum.binary"):
                assert dispatcher not in assembly, assembly
