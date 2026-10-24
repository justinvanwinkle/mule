"""Consecutive shifts in bit-field extraction and packing, not isolated rules."""

import re
from textwrap import dedent

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_SSA_VALUE = r"%[-a-zA-Z$._0-9]+"


def _returned_input_operations(case, body, input_parameters):
    """Follow returned SSA values, including actual outlined native callees."""
    definitions = dict(re.findall(rf"^\s*({_SSA_VALUE}) = (.+)$", body, re.MULTILINE))

    def depends_on_input(value, visiting=frozenset()):
        if value in input_parameters:
            return True
        if value in visiting or value not in definitions:
            return False
        return any(
            depends_on_input(operand, visiting | {value})
            for operand in re.findall(_SSA_VALUE, definitions[value])
        )

    pending = re.findall(rf"\bret (?:ptr|i\d+) ({_SSA_VALUE})", body)
    visited = set()
    operations = []
    while pending:
        value = pending.pop()
        if value in visited or value not in definitions:
            continue
        visited.add(value)
        statement = definitions[value]
        pending.extend(re.findall(_SSA_VALUE, statement))
        if not depends_on_input(value):
            continue
        operations.append(statement)
        # Obtain the callee from the actual call instruction, not a convention
        # relating a Python function identifier to a generated native symbol.
        native_call = re.search(
            r'\bcall\b[^@]*@"?(mule\.native\.[^"(\s]+)"?\((.*)\)', statement
        )
        if native_call is not None:
            symbol, arguments = native_call.groups()
            callee = case.function_ir(rf"^{re.escape(symbol)}$")
            parameters = re.findall(_SSA_VALUE, callee.splitlines()[0])
            inputs = {
                parameter
                for parameter, argument in zip(
                    parameters, arguments.split(","), strict=True
                )
                if any(
                    depends_on_input(operand)
                    for operand in re.findall(_SSA_VALUE, argument)
                )
            }
            operations.extend(_returned_input_operations(case, callee, inputs))
    return operations


@pytest.mark.parametrize("operator", ["<<", ">>"])
@pytest.mark.parametrize("counts", [(2, 3), (0, 5), (512, 512)])
def test_shift_field_normal_pipeline_combines_nonnegative_counts(operator, counts):
    inner, outer = counts
    compilation = compile_source(
        "import mule_types\n"
        "def shift_field(value: mule_types.int32):\n"
        f" return (value {operator} {inner}) {operator} {outer}\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "shift_field")
    operation = "%left_shift" if operator == "<<" else "%right_shift"
    calls = list(known_calls(function, operation))
    assert len(calls) == 1
    count = constant_literal(calls[0].args[1])
    assert isinstance(count, ast.IntegerLiteral)
    assert count.value == inner + outer
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("operator", ["<<", ">>"])
@pytest.mark.parametrize("counts", [(-1, 2), (2, -1), (512, 513)])
def test_shift_field_normal_pipeline_keeps_invalid_or_excessive_counts(
    operator, counts
):
    inner, outer = counts
    compilation = compile_source(
        "import mule_types\n"
        "def shift_field(value: mule_types.int32):\n"
        f" return (value {operator} {inner}) {operator} {outer}\n"
    )
    function = next(fn for fn in compilation.lambdas if fn.source_name == "shift_field")
    operation = "%left_shift" if operator == "<<" else "%right_shift"
    assert len(list(known_calls(function, operation))) == 2
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize(
    "expression",
    ["((value >> 2) >> 3) & 2047", "((value & 65535) << 2) << 3"],
    ids=["extract", "pack"],
)
def test_shift_field_optimized_path_has_combined_native_work(tmp_path, expression):
    source = (
        "import mule_types\n"
        "def shift_field(value: mule_types.int32) -> mule_types.int32:\n"
        f" return {expression}\n"
        "print(shift_field(int(input())))\n"
    )
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for value in (0, 1, -1, 65535, -65537, 2147483647, -2147483648):
        case.compare(f"{value}\n")

    # The argument-binding entry delegates the computation to the body. Inspect
    # that computation, including the native entry when it remains outlined.
    entry = case.function_ir(r"^mule_fn_shift_field\.fn$")
    assert "@mule_fn_shift_field.body(" in entry, entry
    body = case.function_ir(r"^mule_fn_shift_field\.body$")
    parameters = re.findall(_SSA_VALUE, body.splitlines()[0])
    operations = "\n".join(_returned_input_operations(case, body, {parameters[-1]}))
    # LLVM can combine the shift with the tagged representation's one-bit
    # encoding/decoding. Require the combined work on the input-to-return path,
    # not an instruction elsewhere in a wrapper, guard, or cleanup branch.
    shift = r"\b(?:ashr|lshr|shl) (?:nuw |nsw |exact )*i(?:32|64) [^,\n]+, "
    assert re.search(shift + r"(?:4|5|6)\b", operations), operations
    assert not re.search(shift + r"(?:2|3)\b", operations), operations
    assert not re.search(
        r"\bcall\b[^\n]*@(?:\"?mule\.(?:fast|fixnum)\.binary|mule_binary\b)",
        operations,
    ), operations


_BOUNDARY_PROGRAM = dedent(
    """
    def fields(value):
        print('signed', (value >> 2) >> 3, (value << 2) << 3)
        print('zero', (value >> 0) >> 0, (value << 0) << 0)
        print('limit', (value >> 512) >> 512, (value << 512) << 512)
        print('over', (value >> 512) >> 513, (value << 512) << 513)
        intermediate = value << 2
        packed = intermediate << 3
        print('shared', intermediate, packed, intermediate)
        print('opposite', (value << 2) >> 3, (value >> 2) << 3)
        for count in (-1, 0, 4):
            try:
                print('dynamic', count, (value >> count) >> 3)
            except ValueError:
                print('dynamic-negative')
        try:
            print((value << -1) << 2)
        except ValueError:
            print('left-inner-negative')
        try:
            print((value << 2) << -1)
        except ValueError:
            print('left-outer-negative')
        try:
            print((value >> -1) >> 2)
        except ValueError:
            print('right-inner-negative')
        try:
            print((value >> 2) >> -1)
        except ValueError:
            print('right-outer-negative')

    class Traced:
        def __init__(self, value):
            self.value = value
        def __lshift__(self, count):
            print('left', self.value, count)
            if count < 0:
                raise ValueError('negative')
            return Traced(self.value << count)
        def __rshift__(self, count):
            print('right', self.value, count)
            if count < 0:
                raise ValueError('negative')
            return Traced(self.value >> count)

    def outer_count():
        print('outer-count')
        return 3

    value = int(input())
    fields(value)
    tracked = Traced(value)
    print('object-left', ((tracked << 2) << 3).value)
    print('object-right', ((tracked >> 2) >> 3).value)
    try:
        result = (tracked << -1) << outer_count()
        print(result.value)
    except ValueError:
        print('inner-failed-before-outer')
    print('original', tracked.value)
    """
)


@pytest.mark.integration
def test_shift_field_boundaries_effect_order_and_multiple_uses_match_python(tmp_path):
    case = build_case(tmp_path, _BOUNDARY_PROGRAM, optimize=3, lto=False)
    # Runtime parsing prevents constant folding and includes both promotion and
    # negative arithmetic-right-shift rounding beyond machine integer storage.
    for value in (0, -33, 65535, 2**100 + 33, -(2**100) - 33):
        case.compare(f"{value}\n")
