"""Absorbing arithmetic must discard an operation, not evaluation of its input."""

import re
from textwrap import dedent

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_lvar_value
from mule.compiler.ir1util import combinations, known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_RULES = (
    ("x * 0", "%multiply"),
    ("0 * x", "%multiply"),
    ("x & 0", "%bit_and"),
    ("0 & x", "%bit_and"),
    ("x % 1", "%modulo"),
    ("x % -1", "%modulo"),
)


@pytest.mark.parametrize(("expression", "operation"), _RULES)
@pytest.mark.parametrize("producer", ["len(record)", "len(record) << 100"])
def test_absorbing_bound_record_integer_removes_ir1_operation(
    expression, operation, producer
):
    compilation = compile_source(
        f"def record_zero(record):\n x = {producer}\n return x, {expression}\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "record_zero")
    assert not list(known_calls(fn, operation))
    # Builtin len remains a full/guarded call here, not a known operation.
    # Its result remains live because the returned first field owns it.
    assert any(
        isinstance(call.source, ast.Call)
        and isinstance(call.source.function, ast.Name)
        and call.source.function.identifier == "len"
        and call.lvar is not None
        for call in combinations(fn)
    )
    result = next(known_calls(fn, "%build_tuple"))
    zero = constant_lvar_value(result.args[1])
    assert type(zero) is ast.IntegerLiteral
    assert zero.value == 0
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(("expression", "operation"), _RULES)
def test_absorbing_call_operand_remains_in_ir1(expression, operation):
    compilation = compile_source(
        "import mule\n"
        "def read_record(record) -> mule.int64:\n"
        " print(record)\n"
        " return len(record)\n"
        "def record_zero(record):\n"
        f" return {expression.replace('x', 'read_record(record)')}\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "record_zero")
    # These rules accept only pure bound references, not even an exact-int call.
    assert list(known_calls(fn, operation))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_absorbing_record_kernels_remove_arithmetic_and_preserve_records(tmp_path):
    kernels = "\n".join(
        f"def record_zero_{index}(record):\n"
        " x = len(record)\n"
        " print('record', record)\n"
        f" return x, {expression}\n"
        for index, (expression, _) in enumerate(_RULES)
    )
    calls = "\n".join(
        f" print(record_zero_{index}(record))" for index in range(len(_RULES))
    )
    source = kernels + "\nfor record in input().split('|'):\n" + calls + "\n"
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="alpha||βeta|record with spaces\n")
    for index in range(len(_RULES)):
        body = case.function_ir(rf"mule_fn_record_zero_{index}\.(?:body|spec\.)")
        # Check actual optimized bodies, not the presence of a selected helper.
        # The only numeric work in these functions is the discarded operation.
        assert not re.search(r"\b(?:mul|srem|urem) i\d+\b", body)
        assert "mule.fast.binary" not in body
        assert "@mule_binary_reuse" not in body
        assert "@mule_format_modulo" not in body


@pytest.mark.integration
def test_absorbing_boundaries_match_python_effects_errors_and_result_types(tmp_path):
    operations = "\n".join(
        f"def operation_{index}(x):\n return {expression}\n"
        for index, (expression, _) in enumerate(_RULES)
    )
    calls = "\n".join(
        f"  result = operation_{index}(value)\n"
        "  print(result, type(result) is int, type(result) is bool)"
        for index in range(len(_RULES))
    )
    effect_calls = "\n".join(
        " try:\n"
        f"  print({expression.replace('x', 'read_value(record)')})\n"
        " except ValueError:\n"
        "  print('invalid record')"
        for expression, _ in _RULES
    )
    unbound = "\n".join(
        f"def unbound_{index}(record, assigned):\n"
        " if assigned:\n"
        "  x = len(record)\n"
        f" return {expression}\n"
        for index, (expression, _) in enumerate(_RULES)
    )
    unbound_calls = "\n".join(
        "try:\n"
        f" print(unbound_{index}(record, False))\n"
        "except UnboundLocalError:\n"
        " print('unbound')"
        for index in range(len(_RULES))
    )
    source = (
        operations
        + unbound
        + dedent("""
        def read_value(record):
            print('read', record)
            return int(record)

        record = input()
        for value in (int(record), int(record) << 100, -(int(record) << 100), True, False):
        """).rstrip()
        + "\n"
        + calls
        + "\n"
    )
    source += "for record in (record, 'not-an-integer'):\n" + effect_calls + "\n"
    source += unbound_calls + "\n"
    source += dedent("""
        class Arithmetic:
            def __mul__(self, other):
                print('mul', other)
                return self
            def __rmul__(self, other):
                print('rmul', other)
                return self
            def __and__(self, other):
                print('and', other)
                return self
            def __rand__(self, other):
                print('rand', other)
                return self
            def __mod__(self, other):
                print('mod', other)
                return self
        custom = Arithmetic()
        print(operation_0(custom) is custom, operation_1(custom) is custom)
        print(operation_2(custom) is custom, operation_3(custom) is custom)
        print(operation_4(custom) is custom, operation_5(custom) is custom)
        print(operation_0([int(input())]), operation_1('payload'))
        print(operation_0(-1.5), operation_4(1.5), operation_5(1.5))
    """)
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="17\n23\n")
