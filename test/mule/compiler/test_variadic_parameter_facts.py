"""Argument binding supplies exact collecting-container facts to IR1."""

import pytest

from mule.compiler.ctype import DICT_TYPE, TEXT_TYPE, UNKNOWN, IntegerType, TupleType
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source


@pytest.mark.parametrize(
    ("parameters", "returned", "expected"),
    [
        ("*values", "values", TupleType()),
        ("**named", "named", DICT_TYPE),
        ("first, *values, marker=1, **named", "values", TupleType()),
        ("first, *values, marker=1, **named", "named", DICT_TYPE),
        ("*values: int", "values", TupleType()),
        ("**named: int", "named", DICT_TYPE),
        ("values, named", "values", UNKNOWN),
        ("values, named", "named", UNKNOWN),
    ],
)
def test_collecting_parameter_general_entry_has_binding_proven_type(
    parameters, returned, expected
):
    compilation = compile_source(f"def collect({parameters}):\n return {returned}\n")
    function = compilation.lambdas[1]

    assert compilation.facts is not None
    assert compilation.facts[function].return_type == expected


def test_variadic_parameter_reassignment_replaces_binding_fact():
    compilation = compile_source(
        "def collect(*values, **named):\n"
        " values = [1]\n"
        " named = 'rebound'\n"
        " return values, named\n"
    )
    function = compilation.lambdas[1]
    assert compilation.facts is not None
    returned = compilation.facts[function].return_type

    assert returned.getitem_type(IntegerType(0, 0)).is_exact_list
    assert returned.getitem_type(IntegerType(1, 1)) == TEXT_TYPE


def test_variadic_parameter_branch_join_does_not_preserve_replaced_container():
    compilation = compile_source(
        "def collect(flag, *values, **named):\n"
        " if flag:\n"
        "  values = 42\n"
        "  named = 'rebound'\n"
        " return values, named\n"
    )
    function = compilation.lambdas[1]
    assert compilation.facts is not None
    returned = compilation.facts[function].return_type

    assert returned.getitem_type(IntegerType(0, 0)) is UNKNOWN
    assert returned.getitem_type(IntegerType(1, 1)) is UNKNOWN


def test_varargs_length_selects_direct_storage_primitive():
    compilation = compile_source("def count(*values):\n return len(values)\n")
    function = compilation.lambdas[1]

    assert list(known_calls(function, "_storage_length"))
