"""Character construction uses compiled Python rules and native text storage."""

import subprocess

import pytest

from mule.compiler import build
from test.mule.compiler.test_modules import execute


@pytest.fixture(scope="module")
def character_program(tmp_path_factory):
    directory = tmp_path_factory.mktemp("characters")
    source = directory / "main.py"
    source.write_text(
        "from sys import argv\n"
        "character = chr(int(argv[1]))\n"
        "print(len(character), repr(character), repr(character[0]))\n"
    )
    executable = build(source, directory / "program")
    source.unlink()
    return executable


@pytest.mark.parametrize(
    "codepoint",
    [0, 48, 127, 128, 0x7FF, 0x800, 0xD800, 0xDFFF, 0xFFFF, 0x10000, 0x10FFFF],
)
def test_chr_constructs_one_character_with_python_codepoint_semantics(
    character_program, codepoint
):
    result = subprocess.run(
        [character_program, str(codepoint)],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    character = chr(codepoint)
    assert result.stdout == f"1 {character!r} {character!r}\n"


def test_chr_uses_index_protocol_and_remains_dynamically_rebindable(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Index:
    def __index__(self):
        print('indexed')
        return 65
print(chr(Index()), repr(chr(False)), repr(chr(True)))
original = chr
def chr(value):
    return 'rebound'
print(chr(65), original(65))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "indexed\nA '\\x00' '\\x01'\nrebound A\n"


def test_chr_rejects_invalid_values_and_argument_binding(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
for value in [-1, 0x110000, 2**100]:
    try:
        chr(value)
    except ValueError:
        print('range error')
for value in [1.0, '65', None]:
    try:
        chr(value)
    except TypeError:
        print('type error')
class InvalidIndex:
    def __index__(self):
        return 1.0
try:
    chr(InvalidIndex())
except TypeError:
    print('invalid index result')
try:
    chr()
except TypeError:
    print('missing argument')
try:
    chr(65, 66)
except TypeError:
    print('extra argument')
try:
    chr(value=65)
except TypeError:
    print('keyword argument')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "range error\n" * 3
        + "type error\n" * 3
        + "invalid index result\nmissing argument\nextra argument\nkeyword argument\n"
    )
