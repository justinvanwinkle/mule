"""Dead displays disappear, but evaluating their operands remains observable."""

import re

import pytest

from mule import ast
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import Constant, Ref
from test.mule.compiler.rewrite_support import build_case

_DEAD_EXPRESSIONS = {
    "list": "[observe(payload, index), observe(payload, -index)]",
    "tuple": "(observe(payload, index), observe(payload, -index))",
    "function": (
        "lambda first=observe(payload, index), second=observe(payload, -index): first"
    ),
}


@pytest.mark.integration
@pytest.mark.parametrize(
    "expression", _DEAD_EXPRESSIONS.values(), ids=_DEAD_EXPRESSIONS
)
def test_unused_construction_removes_allocation_but_preserves_operands(
    tmp_path, expression
):
    kernel_source = f"""
def observe(payload, value):
    print('operand', value)
    payload.append(value)
    return payload

def discard(payload, count):
    for index in range(count):
        {expression}
        971239872398723987239872398723
        'unused-reference-marker'
    return payload
"""
    source = (
        "import sys\n"
        + kernel_source
        + """
payload = []
alias = payload
result = discard(payload, int(sys.argv[1]))
print(result is alias, payload)
payload.append(100)
print(alias, result)
"""
    )
    # Standalone lowering accepts the function fragment, not runtime imports.
    # Native compilation below uses the complete program and its actual imports.
    # Check IR1 here so LLVM deletion alone cannot satisfy the allocation test.
    compilation = compile_source(kernel_source)
    function = next(fn for fn in compilation.lambdas if fn.name == "mule_fn_discard")
    for operation in ("%build_list", "%build_tuple", "%make_function"):
        assert not list(known_calls(function, operation))
    assert function.component is not None
    for block in function.component.blocks():
        for node in block.nodes():
            if isinstance(node, Ref) and isinstance(node.leaf, Constant):
                literal = node.leaf.value
                if isinstance(literal, ast.TextLiteral):
                    assert literal.value != "unused-reference-marker"
                if isinstance(literal, ast.IntegerLiteral):
                    assert literal.value != 971239872398723987239872398723

    case = build_case(tmp_path, source)
    body = case.function_ir(r"^mule_fn_discard(?:\.|$)")
    # The VOPs lower these operations directly to these runtime constructors.
    # A call to observe remains: removing its result cannot remove its effects.
    assert not re.search(r"@mule_sequence_(?:list|tuple_owned)\(", body)
    assert not re.search(r"@mule_function(?:_defaults|_closure)?\(", body)
    assert re.search(r"\bcall\b", body)
    for count in (0, 3, 19):
        case.compare(args=(count,))


@pytest.mark.integration
def test_unused_display_keeps_name_errors_operand_failures_and_live_aliases(tmp_path):
    source = """
import sys

def observe(payload, value):
    print('operand', value)
    payload.append(value)
    return payload

def missing_global(payload):
    [observe(payload, 1), missing_dead_operand, observe(payload, 2)]

def missing_local(payload, bind):
    if bind:
        value = payload
    (observe(payload, 3), value, observe(payload, 4))

def deleted_local(payload):
    value = payload
    del value
    [observe(payload, 5), value, observe(payload, 6)]

def failing_operand(payload, divisor):
    [observe(payload, 7), 12 // divisor, observe(payload, 8)]

def failing_default(payload, divisor):
    lambda first=observe(payload, 9), second=12 // divisor: first

payload = []
alias = payload
mode = int(sys.argv[1])
try:
    if mode == 0:
        missing_global(payload)
    elif mode == 1:
        missing_local(payload, False)
    elif mode == 2:
        deleted_local(payload)
    elif mode == 3:
        failing_operand(payload, int(sys.argv[2]))
    elif mode == 4:
        failing_default(payload, int(sys.argv[2]))
    else:
        missing_local(payload, True)
except UnboundLocalError:
    print('unbound-local')
except NameError:
    print('missing-global')
except ZeroDivisionError:
    print('zero-division')
print(payload is alias, alias)
payload.append(100)
print(alias)
"""
    case = build_case(tmp_path, source)
    body = case.function_ir(
        r"^mule_fn_(?:missing_global|missing_local|deleted_local|failing_operand|failing_default)(?:\.|$)"
    )
    assert not re.search(r"@mule_sequence_(?:list|tuple_owned)\(", body)
    assert not re.search(r"@mule_function(?:_defaults|_closure)?\(", body)
    for mode, divisor in (
        (0, 1),
        (1, 1),
        (2, 1),
        (3, 0),
        (3, 2),
        (4, 0),
        (4, 2),
        (5, 1),
    ):
        case.compare(args=(mode, divisor))


@pytest.mark.integration
def test_unused_arithmetic_keeps_dynamic_operator_effects_and_type_errors(tmp_path):
    source = """
import sys

class Recorder:
    def __init__(self, values):
        self.values = values

    def __add__(self, other):
        print('add', other)
        self.values.append(other)
        return self

def discard_operator(value, operand):
    value + operand
    return value

values = []
recorder = Recorder(values)
result = discard_operator(recorder, int(sys.argv[1]))
print(result is recorder, values)
try:
    discard_operator(None, int(sys.argv[1]))
except TypeError:
    print('type-error')
values.append(100)
print(recorder.values is values, recorder.values)
"""
    case = build_case(tmp_path, source)
    # Unlike display allocation, an unused Python operator is not flushable.
    # Its actual dynamic dispatch must remain, including its exception path.
    body = case.function_ir(r"^mule_fn_discard_operator(?:\.|$)")
    assert re.search(r"\bcall\b[^\n]*@[^\s(]*(?:add|binary)", body)
    for operand in (-7, 0, 123):
        case.compare(args=(operand,))
