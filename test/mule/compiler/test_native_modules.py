"""Native exports are ordinary imported values, not compiler-spelling intrinsics."""

import subprocess

import pytest

from mule.compiler import build
from test.mule.compiler.test_modules import execute


@pytest.fixture(scope="module")
def memory_program(tmp_path_factory):
    directory = tmp_path_factory.mktemp("native-memory")
    source = directory / "main.py"
    source.write_text(
        """
import _mule_native as native
from _mule_native import read_pointer as load
from _mule_native import read_c_string, byte_length, read_byte
def invoke(function, vector, index):
    return function(vector, index)
data = read_c_string(invoke(load, native.argv, 1))
print(native.__name__, native.argc, repr(data), byte_length(data))
print(load(native.argv, native.argc) is None)
def replacement(vector, index):
    return 'rebound'
native.read_pointer = replacement
print(native.read_pointer(native.argv, 1))
print(read_c_string(load(native.argv, 1)) == data)
for index in [-1, native.argc + 1]:
    try:
        load(native.argv, index)
    except IndexError:
        print('pointer bounds')
try:
    read_c_string(0)
except TypeError:
    print('opaque pointer required')
try:
    load(data, 0)
except TypeError:
    print('pointer vector required')
try:
    read_byte(data, byte_length(data))
except IndexError:
    print('byte bounds')
try:
    byte_length('')
except TypeError:
    print('byte storage required')
try:
    native.__file__
except AttributeError:
    print('no source file')
"""
    )
    # Neither a source-local module nor package may impersonate the native module.
    (directory / "_mule_native.py").write_text("raise RuntimeError('local module')\n")
    package = directory / "_mule_native"
    package.mkdir()
    (package / "__init__.py").write_text("raise RuntimeError('local package')\n")
    executable = build(source, directory / "program", optimize=False)
    for path in directory.rglob("*.py"):
        path.unlink()
    return executable


@pytest.mark.parametrize("argument", [b"", b"hello", b"caf\xc3\xa9", b"\xff"])
def test_native_callable_imports_aliases_rebinding_and_foreign_memory_guards(
    memory_program, argument
):
    result = subprocess.run(
        [bytes(memory_program), argument], check=False, capture_output=True, timeout=10
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.decode() == (
        f"_mule_native 2 {argument!r} {len(argument)}\n"
        "True\nrebound\nTrue\n"
        "pointer bounds\npointer bounds\nopaque pointer required\n"
        "pointer vector required\nbyte bounds\nbyte storage required\nno source file\n"
    )


def test_compiled_utf8_decoder_matches_python_for_valid_storage(tmp_path):
    samples = [
        b"",
        b"\x00ASCII\x7f",
        "café 🐍".encode(),
        chr(0x80).encode(),
        chr(0x7FF).encode(),
        chr(0x800).encode(),
        chr(0xD7FF).encode(),
        chr(0xE000).encode(),
        chr(0xFFFF).encode(),
        chr(0x10000).encode(),
        chr(0x10FFFF).encode(),
        b"a" * 8192,
    ]
    result = execute(
        tmp_path,
        {},
        "from _mule_codecs import utf8_decode\n"
        f"for data in {samples!r}:\n"
        " value = utf8_decode(data)\n"
        " print(len(value), repr(value))\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "".join(
        f"{len(value)} {value!r}\n" for value in (data.decode() for data in samples)
    )


def test_compiled_utf8_decoder_rejects_invalid_sequences_and_storage(tmp_path):
    samples = [
        b"\x80",
        b"\xff",
        b"\xc0\x80",
        b"\xc2",
        b"\xc2A",
        b"\xe0\x80\x80",
        b"\xe1\x80",
        b"\xed\xa0\x80",
        b"\xf0\x80\x80\x80",
        b"\xf4\x90\x80\x80",
        b"\xf0\x90\x80",
    ]
    result = execute(
        tmp_path,
        {},
        "from _mule_codecs import utf8_decode\n"
        f"for data in {samples!r}:\n"
        " try:\n"
        "  utf8_decode(data)\n"
        " except UnicodeDecodeError:\n"
        "  print('invalid UTF-8')\n"
        "try:\n"
        " utf8_decode('')\n"
        "except TypeError:\n"
        " print('bytes required')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "invalid UTF-8\n" * len(samples) + "bytes required\n"
