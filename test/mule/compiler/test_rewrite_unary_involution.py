"""Integer involutions use inferred facts without erasing operator effects."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import combinations, known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize(
    ("expression", "operation"),
    [("-(-value)", "%unary_negative"), ("~(~value)", "%unary_invert")],
)
@pytest.mark.parametrize("normalization", ["len(raw)", "len(raw) + 2**100"])
def test_inferred_exact_integer_involution_cancels(
    expression, operation, normalization
):
    compilation = compile_source(
        f"def normalize(raw):\n value = {normalization}\n return {expression}\n"
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "normalize"
    )
    assert not list(known_calls(function, operation))
    # len remains a full or guarded builtin call at this graph stage.
    # Its input-dependent computation must survive rather than fold to a literal.
    assert any(
        isinstance(call.source, ast.Call)
        and isinstance(call.source.function, ast.Name)
        and call.source.function.identifier == "len"
        for call in combinations(function)
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expression", "operation"),
    [("-(-value)", "%unary_negative"), ("~(~value)", "%unary_invert")],
)
@pytest.mark.parametrize("normalization", ["raw", "bool(raw)"])
def test_unknown_and_boolean_involution_retains_operators(
    expression, operation, normalization
):
    compilation = compile_source(
        f"def normalize(raw):\n value = {normalization}\n return {expression}\n"
    )
    function = next(
        function
        for function in compilation.lambdas
        if function.source_name == "normalize"
    )
    assert list(known_calls(function, operation))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize("expression", ["-(-value)", "~(~value)"])
def test_runtime_normalization_has_no_optimized_unary_work(tmp_path, expression):
    case = build_case(
        tmp_path,
        f"""
def normalize(raw):
    value = len(raw)
    return {expression}
raw = input()
print(normalize(raw))
""",
    )
    for raw in ("", "x", "runtime-sized integer" * 40):
        case.compare(raw + "\n")
    body = case.function_ir(r"^mule_fn_normalize\.")
    # Inspect every retained entry, not only a selected specialized helper.
    assert not re.search(r"\bcall\b[^\n]*@(?:\"?mule[.]fast[.]unary|mule_unary)", body)
    assert not re.search(r"\bfneg\b", body)
    # Require a real runtime-dependent result with a physical length operation.
    assert re.search(r"\bload\b|@mule_(?:length|storage_length|text_length)", body)


@pytest.mark.integration
def test_involutions_preserve_bigints_bool_widening_and_owned_results(tmp_path):
    case = build_case(
        tmp_path,
        """
raw = int(input())
def normalized(value):
    value = value + 2**100
    alias = value
    negative = -(-value)
    inverted = ~(~value)
    del value
    print(negative == alias, inverted == alias)
    print(negative + inverted, alias)
    del alias
    print(negative, inverted)
normalized(raw)
for value in (False, True):
    result = -(-value)
    print(result, type(result).__name__, result is value)
""",
    )
    for value in (0, -(2**100), -(2**101), 2**130):
        case.compare(f"{value}\n")


@pytest.mark.integration
@pytest.mark.parametrize("value", [0, 1])
def test_boolean_inversion_preserves_widening_and_deprecation_warning(tmp_path, value):
    # Keep warning parity separate so a warning mismatch cannot prevent the
    # bigint ownership and boolean negation cases above from running fully.
    case = build_case(
        tmp_path,
        """
value = bool(int(input()))
result = ~(~value)
print(result, type(result).__name__, result is value)
""",
    )
    case.compare(f"{value}\n")


@pytest.mark.integration
def test_unknown_involutions_preserve_dispatch_order_mutation_and_errors(tmp_path):
    case = build_case(
        tmp_path,
        """
events = []
class Operand:
    def __init__(self, count):
        self.count = count
    def __neg__(self):
        events.append(('neg', self.count))
        self.count += 1
        if self.count == 3:
            raise ValueError('second negation')
        return self
    def __invert__(self):
        events.append(('invert', self.count))
        self.count += 1
        if self.count == 3:
            raise ValueError('second inversion')
        return self
def negative(value):
    return -(-value)
def invert(value):
    return ~(~value)
def acquire(value):
    events.append('acquire')
    return value
count = int(input())
for operation in (negative, invert):
    value = Operand(count)
    try:
        result = operation(acquire(value))
        print(result is value, value.count)
    except ValueError as error:
        print(type(error).__name__, str(error), value.count)
    print(events)
    events.clear()
    for invalid in (object(), 'text'):
        try:
            operation(invalid)
        except TypeError:
            print('TypeError')
class Integer(int):
    def __neg__(self):
        events.append('integer negation')
        return 17
    def __invert__(self):
        events.append('integer inversion')
        return 19
value = Integer(count)
print(negative(value), invert(value), events)
for value in (0.0, -0.0, 1.25):
    print(str(negative(value)))
    try:
        invert(value)
    except TypeError:
        print('TypeError')
""",
    )
    for count in (0, 1, 2):
        case.compare(f"{count}\n")
