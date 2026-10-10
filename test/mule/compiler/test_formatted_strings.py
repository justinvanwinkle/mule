"""Compiled interpolation respects conversion protocols and evaluation order."""

import pytest

from mule.compiler import UnsupportedSyntax
from mule.compiler.main import compile_source as lower
from test.mule.compiler.test_modules import execute


def test_basic_fstrings_and_debug_fields_ignore_shadowed_builtin_names(tmp_path):
    result = execute(
        tmp_path,
        {},
        "str = 42\nrepr = 7\nvalue = 'café'\n"
        "print(f'hello {value}, {42}, {True}, {None}')\n"
        "print(f'{value!r} {value!s} {value=}')\n"
        "print(f'{{{value}}}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "hello café, 42, True, None\n'café' café value='café'\n{café}\n"
    )


def test_fstrings_dispatch_to_class_protocols_before_next_expression(tmp_path):
    result = execute(
        tmp_path,
        {},
        "class Value:\n"
        " def __format__(self, spec):\n  print('format', spec)\n  return 'formatted'\n"
        " def __str__(self):\n  print('str')\n  return 'string'\n"
        " def __repr__(self):\n  print('repr')\n  return 'representation'\n"
        "def later():\n print('later')\n return 'last'\n"
        "value = Value()\nprint(f'{value} {later()}')\n"
        "print(f'{value!s} {value!r}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "format \nlater\nformatted last\nstr\nrepr\nstring representation\n"
    )


@pytest.mark.parametrize("method", ["__format__", "__str__", "__repr__"])
def test_invalid_formatting_protocol_result_is_an_error(tmp_path, method):
    parameters = "self, spec" if method == "__format__" else "self"
    conversion = {"__format__": "", "__str__": "!s", "__repr__": "!r"}[method]
    result = execute(
        tmp_path,
        {},
        f"class Broken:\n def {method}({parameters}):\n  return 42\n"
        f"print(f'{{Broken(){conversion}}}')\n",
    )
    assert result.returncode != 0
    assert "TypeError" in result.stderr


@pytest.mark.parametrize("source", ["f'{42!a}'"])
def test_unsupported_format_features_remain_explicit_errors(source):
    with pytest.raises(UnsupportedSyntax):
        lower(source)


@pytest.mark.parametrize(
    ("specification", "expected"),
    [
        ("9", "café     "),
        (">9", "     café"),
        ("^9", "  café   "),
        ("*^8", "**café**"),
        ("💥<6", "café💥💥"),
        ("<2", "café"),
        (">", "café"),
    ],
)
def test_string_alignment_and_unicode_fill(tmp_path, specification, expected):
    result = execute(
        tmp_path,
        {},
        f"value = 'café'\nprint(f'{{value:{specification}}}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == expected + "\n"


def test_nested_widths_and_custom_format_specifications(tmp_path):
    result = execute(
        tmp_path,
        {},
        "class Custom:\n def __format__(self, specification):\n  return specification\n"
        "width = 8\nvalue = 'hi'\nprint(f'{value:^{width}}')\n"
        "print(f'{Custom():arbitrary-specification}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "   hi   \narbitrary-specification\n"


def test_formatting_can_mutate_globals_seen_by_later_fields(tmp_path):
    result = execute(
        tmp_path,
        {},
        "state = 1\nclass Mutator:\n def __format__(self, spec):\n  global state\n  state = 2\n  return 'changed'\n"
        "print(f'{Mutator()} {state}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "changed 2\n"


def test_formatting_uses_class_method_not_instance_field(tmp_path):
    result = execute(
        tmp_path,
        {},
        "class Custom:\n def __init__(self):\n  self.__format__ = 42\n"
        " def __repr__(self):\n  return 'repr fallback'\n"
        "print(f'{Custom()}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "repr fallback\n"


def test_private_primitive_facts_do_not_come_from_user_global_functions(tmp_path):
    result = execute(
        tmp_path,
        {},
        "def _text_length(value):\n return 0\n"
        "def _text_character(value, index):\n return 0\n"
        "def _render_string(value):\n return 0\n"
        "def _format_method(value):\n return 0\n"
        "print(f'{42!s} {42!r}')\nprint(f'{\"hi\":^8}')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "42 42\n   hi   \n"
