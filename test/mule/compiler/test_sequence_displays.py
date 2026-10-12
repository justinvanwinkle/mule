"""Native construction of lists and tuples from evaluated expressions."""

import shutil
import subprocess
from pathlib import Path

from mule.compiler.driver import Toolchain, build_runtime, builtins_object, emit_llvm
from test.mule.compiler.test_modules import execute


def test_sequence_displays_preserve_order_identity_and_fresh_allocations(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def item(value):\n print(value)\n return value\n"
        "values = [item(1), item(2), (item(3),)]\n"
        "print(values, len(values), (), [])\n"
        "a = [42]\nb = [42]\nprint(a is b)\n"
        "print((values, values))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "1\n2\n3\n[1, 2, (3,)] 3 () []\nFalse\n([1, 2, (3,)], [1, 2, (3,)])\n"
    )


def test_sequence_element_failure_stops_later_evaluation(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def before():\n print('before')\n return 'owned'\n"
        "def after():\n print('after')\n return 3\n"
        "print([before(), missing, after()])\n",
    )
    assert result.returncode != 0
    assert result.stdout == "before\n"
    assert "NameError" in result.stderr


def test_immutable_tuple_literals_are_shared_without_merging_scalar_types(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def leaf():
    return None, None
def record():
    return (True, 1, 1.25, 'text', b'bytes', 1208925819614629174706176, (None, 2))
first = record()
second = record()
print(leaf() is leaf(), first is second, first[6] is second[6])
print(first[0], first[1], first[2], first[3], first[4], first[5], first[6][1])
def boolean():
    return (True,)
def integer():
    return (1,)
def floating():
    return (1.0,)
print(boolean() is integer(), integer() is floating())
def mutable():
    return ([1], None)
a = mutable()
b = mutable()
a[0][0] = 7
print(a is b, b[0][0])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "True True True\n"
        "True 1 1.25 text b'bytes' 1208925819614629174706176 2\n"
        "False False\nFalse 1\n"
    )


def test_tuple_constants_survive_defaults_imports_and_caught_errors(tmp_path):
    result = execute(
        tmp_path,
        {"constants.py": "def value():\n return ('imported', (None, 2))\n"},
        """
import constants
def default(value=(1, 2)):
    return value
try:
    raise ValueError(('caught', (None, 2)))
except ValueError as error:
    print(error.args[0][0], error.args[0][1][1])
print(default() is default(), constants.value() is constants.value())
print(constants.value()[0], default()[1])
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "caught 2\nTrue True\nimported 2\n"


def test_tuple_pool_allocation_failure_releases_partial_initialization(tmp_path):
    # Keep the generated main's startup and cleanup, but call it from a Rust
    # allocator probe which fails after creating a child and owned scalar.
    llvm = emit_llvm("root = ('owned text', (1, 2))\nprint('body', root)\n")
    llvm = llvm.replace("define i32 @main(", "define i32 @mule_entry(", 1)
    module = tmp_path / "module.ll"
    module.write_text(llvm)
    object_file = tmp_path / "module.o"
    subprocess.run(
        ["clang", "-O3", "-Wno-override-module", "-c", module, "-o", object_file],
        check=True,
        capture_output=True,
        text=True,
    )
    runtime = build_runtime(tmp_path, crate_type="rlib")
    builtins = builtins_object(Toolchain(optimize=False))
    rustc = shutil.which("rustc")
    assert rustc is not None, "native runtime tests require rustc"
    probe = tmp_path / "probe"
    subprocess.run(
        [
            rustc,
            "--edition=2024",
            "-Copt-level=3",
            "-Cpanic=abort",
            "--extern",
            f"mule_runtime={runtime}",
            "-C",
            f"link-arg={object_file}",
            "-C",
            f"link-arg={builtins}",
            Path(__file__).with_name("pool_failure_probe.rs"),
            "-o",
            probe,
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    result = subprocess.run(
        [probe], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "partial pool cleanup verified\n"
    assert "MemoryError: sequence allocation failed" in result.stderr
