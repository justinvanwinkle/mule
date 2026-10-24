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


def test_stdin_reads_text_and_binary_streams(tmp_path):
    source = tmp_path / "main.py"
    source.write_text(
        "import sys\n"
        "print(sys.stdin.readable(), sys.stdout.writable())\n"
        "print(repr(sys.stdin.readline()))\n"
        "print(repr(sys.stdin.read(4)))\n"
        "print(repr(sys.stdin.read()))\n"
    )
    executable = build(source, tmp_path / "program", optimize=False, workers=1)
    result = subprocess.run(
        [executable],
        input="first line\ncafé rest\n",
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True True\n'first line\\n'\n'café'\n' rest\\n'\n"


def test_sys_intern_and_compile_time_literal_interning(tmp_path):
    (tmp_path / "helper.py").write_text(
        "def get_literal():\n"
        " return 'shared_compile_time_literal'\n"
        "def get_other():\n"
        " return 'second_module_literal'\n"
    )
    source = tmp_path / "main.py"
    source.write_text(
        "import sys\n"
        "import helper\n"
        "\n"
        "# Compile-time literals are interned across functions and modules.\n"
        "local_lit = 'shared_compile_time_literal'\n"
        "assert helper.get_literal() is local_lit\n"
        "assert sys._is_interned(local_lit)\n"
        "assert sys._is_interned(helper.get_other())\n"
        "assert sys._is_interned('')\n"
        "assert sys._is_interned('x')\n"
        "assert sys._is_interned('é')\n"
        "\n"
        "# Dynamically constructed string matching a literal resolves to that literal.\n"
        "parts = ['shared', '_compile', '_time', '_literal']\n"
        "dyn = ''.join(parts)\n"
        "assert dyn == local_lit\n"
        "assert dyn is not local_lit\n"
        "assert not sys._is_interned(dyn)\n"
        "assert sys.intern(dyn) is local_lit\n"
        "assert sys._is_interned(sys.intern(dyn))\n"
        "\n"
        "# Fresh dynamic string is interned in place and reused by subsequent sys.intern.\n"
        "fresh1 = ''.join(['dynamic', '_only', '_string'])\n"
        "assert not sys._is_interned(fresh1)\n"
        "pinned = sys.intern(fresh1)\n"
        "assert pinned is fresh1\n"
        "assert sys._is_interned(fresh1)\n"
        "fresh2 = ''.join(['dynamic', '_only', '_string'])\n"
        "assert fresh2 is not fresh1\n"
        "assert sys.intern(fresh2) is fresh1\n"
        "\n"
        "# Lone surrogates are supported by sys.intern.\n"
        "surr = '\\ud800' + '_tail'\n"
        "assert sys.intern(surr) is surr\n"
        "assert sys._is_interned(surr)\n"
        "\n"
        "# str subclasses are rejected by sys.intern and report False in sys._is_interned.\n"
        "class SubStr(str):\n"
        " pass\n"
        "sub = SubStr('shared_compile_time_literal')\n"
        "assert not sys._is_interned(sub)\n"
        "try:\n"
        " sys.intern(sub)\n"
        " raise AssertionError('expected TypeError for str subclass')\n"
        "except TypeError:\n"
        " pass\n"
        "\n"
        "for invalid in (123, None, b'bytes'):\n"
        " try:\n"
        "  sys.intern(invalid)\n"
        "  raise AssertionError('expected TypeError from sys.intern')\n"
        " except TypeError:\n"
        "  pass\n"
        " try:\n"
        "  sys._is_interned(invalid)\n"
        "  raise AssertionError('expected TypeError from sys._is_interned')\n"
        " except TypeError:\n"
        "  pass\n"
        "print('intern ok')\n"
    )
    executable = build(source, tmp_path / "intern_prog", optimize=False, workers=1)
    result = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "intern ok\n"
