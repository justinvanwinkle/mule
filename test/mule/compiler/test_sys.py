"""Compiled sys state comes from the native process, not the compiler host."""

import subprocess

import pytest

from mule.compiler import build


@pytest.fixture(scope="module")
def argv_program(tmp_path_factory):
    directory = tmp_path_factory.mktemp("sys-argv")
    source = directory / "main.py"
    source.write_text(
        "import sys\n"
        "from sys import argv\n"
        "import sys as again\n"
        "print(sys.__name__, sys is again, argv is again.argv)\n"
        "print(len(argv))\n"
        "for argument in argv:\n"
        " print(argument)\n"
        "argv.append('added')\n"
        "print(again.argv[len(again.argv) - 1])\n"
        "sys.argv = ['replaced']\n"
        "print(again.argv[0], argv[len(argv) - 1])\n"
    )
    # Built-in modules precede a source-local file, including private OS bindings.
    (directory / "sys.py").write_text("raise RuntimeError('local sys loaded')\n")
    (directory / "_mule_native.py").write_text(
        "raise RuntimeError('local native module loaded')\n"
    )
    (directory / "_mule_codecs.py").write_text(
        "raise RuntimeError('local codec module loaded')\n"
    )
    executable = build(source, directory / "program", optimize=False)
    for path in directory.glob("*.py"):
        path.unlink()
    return executable


@pytest.mark.parametrize("arguments", [[], ["10"], ["", "two words", "café", "🐍"]])
def test_argv_contains_native_arguments_and_shared_mutable_module_state(
    argv_program, arguments
):
    result = subprocess.run(
        [argv_program, *arguments],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "sys True True\n"
        f"{len(arguments) + 1}\n"
        + "\n".join([str(argv_program), *arguments])
        + "\nadded\nreplaced added\n"
    )


def test_argv_preserves_undecodable_posix_bytes(argv_program):
    result = subprocess.run(
        [bytes(argv_program), b"\xff"], check=False, capture_output=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        b"sys True True\n2\n" + bytes(argv_program) + b"\n\xff\nadded\nreplaced added\n"
    )
    assert result.stderr == b""


def test_native_operations_require_an_explicit_import(tmp_path):
    source = tmp_path / "main.py"
    source.write_text("print(read_pointer(None, 0))\n")
    executable = build(source, tmp_path / "program", optimize=False)
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode != 0
    assert "NameError" in result.stderr


def test_compiled_sys_argv_supplies_a_workload_size(tmp_path):
    source = tmp_path / "main.py"
    source.write_text(
        "from sys import argv\n"
        "n = int(argv[1])\n"
        "total = 0\n"
        "for value in range(n):\n"
        " total += value\n"
        "print(total)\n"
    )
    executable = build(source, tmp_path / "program", optimize=False)
    source.unlink()
    result = subprocess.run(
        [executable, "10"], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "45\n"
