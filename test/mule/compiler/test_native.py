"""Run real native artifacts, independently of the compiler process."""

import os
import subprocess

import pytest

from mule.compiler import build, emit_llvm


def run_native(tmp_path, source):
    source_path = tmp_path / "program.py"
    source_path.write_text(source)
    executable = tmp_path / "program"
    build(source_path, executable, optimize=False, workers=1)
    assert executable.read_bytes().startswith(b"\x7fELF")
    source_path.unlink()
    return subprocess.run(
        [executable],
        check=False,
        cwd=tmp_path,
        env={"PATH": "/usr/bin:/bin", "LC_ALL": "C.UTF-8"},
        capture_output=True,
        text=True,
        timeout=10,
    )


def test_global_reads_follow_rebinding_shadowing_and_deletion(tmp_path):
    # Each read site caches its name's cells after first use; every later
    # binding change must still be visible through that site.
    source = """def show():
    return len("abc"), counter
counter = 1
print(show())
counter = 2
print(show())
def len(value):
    return -1
print(show())
del len
print(show())
del counter
try:
    show()
except NameError as error:
    print(error)
counter = 3
print(show())
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "(3, 1)\n(3, 2)\n(-1, 2)\n(3, 2)\nname 'counter' is not defined\n(3, 3)\n"
    )


@pytest.mark.parametrize(("safety", "expected"), [(3, "3\n99\n"), (2, "3\n3\n")])
def test_builtin_rebinding_is_visible_only_at_safety_three(tmp_path, safety, expected):
    # Safety 2 assumes builtins are never rebound, so the module keeps the
    # builtin it started with; safety 3 is exact Python.
    source = (
        f"__safety__ = {safety}\nimport builtins\n"
        "def measure():\n    return len('abc')\n"
        "print(measure())\nbuiltins.len = lambda value: 99\nprint(measure())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_unchecked_unbound_global_raises_when_used(tmp_path):
    # Safety 0 omits the unbound check: the read succeeds, and using the
    # value raises NameError.
    source = (
        "__safety__ = 0\n"
        "def probe():\n    value = missing\n    print('read')\n    return value\n"
        "try:\n    probe()()\nexcept NameError:\n    print('raised')\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "read\nraised\n"


def test_tracebacks_record_each_frame_an_exception_passes_through(tmp_path):
    source = (
        "def inner(value):\n    return 10 // value\n"
        "def outer(value):\n    return 1 + inner(value)\n"
        "try:\n    outer(0)\nexcept ZeroDivisionError as error:\n"
        "    entry = error.__traceback__\n"
        "    while entry is not None:\n"
        "        print(entry.tb_frame.f_code.co_name, entry.tb_lineno)\n"
        "        entry = entry.tb_next\n"
        "try:\n    raise TypeError\nexcept TypeError as exc:\n"
        "    print(type(exc.__traceback__).__name__,"
        " type(exc.__traceback__.tb_frame).__name__)\n"
        "outer(0)\n"
    )
    result = run_native(tmp_path, source)
    assert result.stdout == ("<module> 6\nouter 4\ninner 2\ntraceback frame\n")
    assert result.returncode != 0
    entries = [
        line for line in result.stderr.splitlines() if line.startswith("  File ")
    ]
    assert [entry.rsplit(", ", 2)[1:] for entry in entries] == [
        ["line 16", "in <module>"],
        ["line 4", "in outer"],
        ["line 2", "in inner"],
    ]
    assert result.stderr.startswith("Traceback (most recent call last):\n")


def test_function_type_descriptors_allow_types_import(tmp_path):
    # types.py reads FunctionType.__globals__ through the class to name the
    # descriptor type; instances must still see their real globals.
    source = (
        "import types\n"
        "marker = 42\n"
        "def probe():\n    return marker\n"
        "print(probe.__globals__.marker, probe.__closure__)\n"
        "print(types.FunctionType.__globals__ is types.FunctionType.__globals__)\n"
        "print(types.FunctionType.__closure__ is types.FunctionType.__closure__)\n"
        "print(globals()['marker'])\n"
        "def current():\n    return globals()['marker']\n"
        "print(current())\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42 None\nTrue\nTrue\n42\n42\n"


def test_bytearray_constructs_iterates_and_mutates(tmp_path):
    source = (
        "ba = bytearray(b'abc')\n"
        "print(list(ba), len(ba), ba[0], ba[-1])\n"
        "ba[0] = 65\nba.append(100)\n"
        "print(bytes(ba), ba == b'Abcd', ba.decode())\n"
        "print(bytearray(2), bytearray([1, 255]), bytearray('xy', 'utf-8'))\n"
        "print(ba[1:3])\n"
        "del ba[0]\nprint(ba.pop(), list(ba))\n"
        "try:\n    bytearray([256])\nexcept ValueError as error:\n    print(error)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "[97, 98, 99] 3 97 99\n"
        "b'Abcd' True Abcd\n"
        "bytearray(b'\\x00\\x00') bytearray(b'\\x01\\xff') bytearray(b'xy')\n"
        "bytearray(b'bc')\n"
        "100 [98, 99]\n"
        "byte must be in range(0, 256)\n"
    )


def test_module_policy_assignment_must_precede_module_code():
    with pytest.raises(SyntaxError, match="must precede the module's code"):
        emit_llvm("print(1)\n__safety__ = 0\n")


@pytest.mark.parametrize(
    ("source", "expected"),
    [
        ("print(40 + 2)\n", "42\n"),
        ("print(2 ** 100)\n", "1267650600228229401496703205376\n"),
        ("print('hello ' + 'world', True, None)\n", "hello world True None\n"),
        ("print(-7 // 3, -7 % 3, 7 / 2)\n", "-3 2 3.5\n"),
        ("x = 3\nif x > 2:\n print('yes')\nelse:\n print('no')\n", "yes\n"),
        ("x = 3\nwhile x:\n print(x)\n x -= 1\n", "3\n2\n1\n"),
        ("def twice(x):\n return x * 2\nprint(twice(21))\n", "42\n"),
        (
            "def fact(n):\n if n <= 1:\n  return 1\n return n * fact(n - 1)\nprint(fact(10))\n",
            "3628800\n",
        ),
        ("print(0 and print('bad'), 3 or print('bad'))\n", "0 3\n"),
        ("print('yes' if True else 1 / 0)\n", "yes\n"),
        ("print(1 < 2 < 3, 3 < 2 < 1)\n", "True False\n"),
        ("a = b = 12\nprint(a, b)\n", "12 12\n"),
        (
            "x = 0\nwhile x < 5:\n x += 1\n if x == 2:\n  continue\n if x == 4:\n  break\n print(x)\nelse:\n print('bad')\nprint('done')\n",
            "1\n3\ndone\n",
        ),
        ("x = 0\nwhile x < 2:\n x += 1\nelse:\n print(x)\n", "2\n"),
        ("def f():\n pass\nprint(f())\n", "None\n"),
        (
            "def f():\n return f\nold = f\nf = 23\nprint(old())\n",
            "23\n",
        ),
        ("x = 1\ndef change():\n global x\n x = 2\nchange()\nprint(x)\n", "2\n"),
        ("print = abs\nresult = print(-3)\n", ""),
        ("print('🙂', b'abc', 1.5 * 2)\n", "🙂 b'abc' 3.0\n"),
        ("print((1 if True else 2) if False else 3)\n", "3\n"),
        ("print(1 and (2 or 3), 0 or (4 and 5))\n", "2 5\n"),
        (
            "def middle():\n print('middle')\n return 2\nprint(1 < middle() < 3)\n",
            "middle\nTrue\n",
        ),
        ("print(3 < 2 < print('bad'))\n", "False\n"),
        ("print((x := 42), x)\n", "42 42\n"),
        ("print(0x" + "f" * 5000 + " > 0)\n", "True\n"),
    ],
)
def test_native_execution(tmp_path, source, expected):
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected
    assert result.stderr == ""


@pytest.mark.parametrize(
    ("source", "exception"),
    [
        ("print(1 / 0)\n", "ZeroDivisionError"),
        ("print(missing)\n", "NameError"),
        ("def f():\n print(x)\n x = 1\nf()\n", "UnboundLocalError"),
        ("def f(x):\n return x\nf()\n", "TypeError"),
        ("def f(x):\n return x\nf(1, 2)\n", "TypeError"),
        ("x = 1\nx()\n", "TypeError"),
        ("print('x' - 1)\n", "TypeError"),
        ("def recurse():\n return recurse()\nrecurse()\n", "RecursionError"),
    ],
)
def test_runtime_failure_returns_nonzero(tmp_path, source, exception):
    result = run_native(tmp_path, source)
    assert result.returncode != 0
    assert exception in result.stderr


@pytest.mark.parametrize(
    ("truth", "statement", "expected"),
    [
        (False, "if x and True:\n print('bad')", "1\n"),
        (True, "if x or False:\n print('yes')", "yes\n1\n"),
        (False, "if not (x and True):\n print('yes')", "yes\n1\n"),
        (True, "print(not (x or False))", "False\n2\n"),
        (False, "print(not not x)", "False\n1\n"),
        (False, "while x and True:\n print('bad')", "1\n"),
        (False, "print('yes' if x and True else 'no')", "no\n1\n"),
        (False, "if (x and True) if True else False:\n print('bad')", "1\n"),
        (False, "if x < x < x:\n print('bad')", "1\n"),
        (False, "print((x and True) or 7)", "7\n1\n"),
        (True, "print((x or False) and 7)", "7\n1\n"),
        (False, "print((x < x < x) or 7)", "7\n2\n"),
        (False, "result = x and True\nprint(result is x)", "True\n1\n"),
        (True, "result = x or False\nprint(result is x)", "True\n1\n"),
        (False, "result = x < x < x\nprint(result is x)", "True\n1\n"),
    ],
    ids=[
        "false-and-condition",
        "true-or-condition",
        "not-and-condition",
        "not-or-value",
        "double-not",
        "while-condition",
        "conditional-test",
        "conditional-in-condition",
        "comparison-condition",
        "nested-and-or-value",
        "nested-or-and-value",
        "comparison-in-or",
        "and-operand-value",
        "or-operand-value",
        "comparison-operand-value",
    ],
)
def test_native_truth_testing_preserves_side_effects(
    tmp_path, truth, statement, expected
):
    source = (
        "count = 0\n"
        "def truth():\n global count\n count += 1\n"
        f" return {truth}\n"
        "def less(other):\n return x\n"
        "C = type('C', tuple(), dict())\n"
        "setattr(C, '__bool__', staticmethod(truth))\n"
        "setattr(C, '__lt__', staticmethod(less))\n"
        "x = C()\n"
        f"{statement}\nprint(count)\n"
    )
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected


def test_cli_builds_executable_and_llvm_ir(tmp_path):
    source = tmp_path / "hello.py"
    source.write_text("print(42)\n")
    executable = tmp_path / "hello"
    llvm = tmp_path / "hello.ll"
    result = subprocess.run(
        [
            "uv",
            "run",
            "python",
            "-m",
            "mule.compiler",
            str(source),
            "-O0",
            "-o",
            str(executable),
            "--emit-llvm",
            str(llvm),
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr
    assert os.access(executable, os.X_OK)
    assert "define i32 @main" in llvm.read_text()
    run = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert run.returncode == 0, run.stderr
    assert run.stdout == "42\n"


def test_executable_runs_without_python_runtime_dependencies(tmp_path):
    source = tmp_path / "independent.py"
    source.write_text("print(2 ** 100, 40 + 2)\n")
    executable = tmp_path / "independent"
    build(source, executable, optimize=False, workers=1)
    source.unlink()
    dynamic = subprocess.run(
        ["readelf", "-d", executable],
        check=True,
        capture_output=True,
        text=True,
    )
    imports = subprocess.run(
        ["nm", "-D", "--undefined-only", executable],
        check=True,
        capture_output=True,
        text=True,
    )
    sections = subprocess.run(
        ["readelf", "-SW", executable],
        check=True,
        capture_output=True,
        text=True,
    )
    assert ".debug_info" not in sections.stdout
    assert ".debug_line" not in sections.stdout
    assert "libpython" not in dynamic.stdout.lower()
    assert "libgmp" in dynamic.stdout
    symbols = [line.split()[-1] for line in imports.stdout.splitlines() if line.split()]
    assert not any(symbol.startswith(("Py", "_Py")) for symbol in symbols)
    result = subprocess.run(
        [executable],
        check=True,
        capture_output=True,
        text=True,
        cwd=tmp_path,
        env={
            "PATH": "/usr/bin:/bin",
            "PYTHONHOME": "/nonexistent-python-installation",
            "PYTHONPATH": "/nonexistent-python-modules",
        },
        timeout=10,
    )
    assert result.stdout == "1267650600228229401496703205376 42\n"
