"""Same-local bitwise identities through inferred facts and real execution."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import Combination, LambdaVar, Ref, Return
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("operator, operation", [("&", "%bit_and"), ("|", "%bit_or")])
@pytest.mark.parametrize("initial", ["len(raw)", "bool(raw)"])
def test_self_bitwise_inferred_local_returns_original_value(
    operator, operation, initial
):
    compilation = compile_source(
        f"def process(raw):\n value = {initial}\n return value {operator} value\n"
    )
    function = next(f for f in compilation.lambdas if f.source_name == "process")
    assert not list(known_calls(function, operation))
    result = function.return_
    assert isinstance(result, Return)
    producer = result.result.use()
    assert isinstance(producer, Ref)
    # Lambda.vars contains parameters, not this lexical local. Its surviving
    # reference still identifies the runtime-produced value and its assignment.
    local = producer.leaf
    assert isinstance(local, LambdaVar)
    assert local.name == "value" and local.home is function
    assert local.refs == [producer]
    assert len(local.sets) == 1
    initializer = local.sets[0].value.use()
    assert isinstance(initializer, Combination)
    argument = initializer.args[0].use()
    assert isinstance(argument, Ref)
    assert argument.leaf is function.vars[0]
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("operator, operation", [("&", "%bit_and"), ("|", "%bit_or")])
@pytest.mark.parametrize(
    "body",
    [
        "left = len(raw)\nright = len(other)\nreturn left OP right",
        "value = len(raw)\nreturn value OP (value := len(other))",
        "if raw:\n value = len(other)\nreturn value OP value",
        "value = len(raw)\ndel value\nreturn value OP value",
        "return raw OP raw",
    ],
)
def test_self_bitwise_near_misses_retain_operation(operator, operation, body):
    source = "def process(raw, other):\n" + "\n".join(
        " " + line for line in body.replace("OP", operator).splitlines()
    )
    compilation = compile_source(source + "\n")
    function = next(f for f in compilation.lambdas if f.source_name == "process")
    assert list(known_calls(function, operation))
    check_ir1_consistency(compilation.components)


_PROCESSORS = """
def count_and(raw):
    value = len(raw)
    return value & value

def count_or(raw):
    value = len(raw)
    return value | value

def flag_and(raw):
    value = bool(raw)
    return value & value

def flag_or(raw):
    value = bool(raw)
    return value | value

raw = input()
print(count_and(raw), count_or(raw))
print(flag_and(raw), flag_or(raw))
print(type(flag_and(raw)) is bool, type(flag_or(raw)) is bool)
"""


@pytest.mark.integration
def test_self_bitwise_runtime_processing_has_no_bitwise_work(tmp_path):
    case = build_case(tmp_path, _PROCESSORS, optimize=3, lto=False)
    for name in ("count_and", "count_or", "flag_and", "flag_or"):
        # Inspect the actual user bodies, not only the external entry wrappers.
        body = case.function_ir(rf"^mule_fn_{name}\.body(?:\.|$)")
        assert "ret " in body
        assert not re.search(r"@[^\s(]*(?:bit_and|bit_or|binary|resolve_method)", body)
        # The emitted body also rules out a helper containing the removed work.
        original = case.function_ir(rf"^mule_fn_{name}\.body(?:\.|$)", optimized=False)
        assert not re.search(r"@[^\s(]*(?:bit_and|bit_or|binary)", original)
    for raw in ("", "x", "a mask record with Unicode: \u03bb", "x" * 4097):
        case.compare(stdin=raw + "\n")


@pytest.mark.integration
@pytest.mark.parametrize("operator", ["&", "|"])
def test_self_bitwise_mutation_unbound_and_overloads_match_python(tmp_path, operator):
    source = """
def different(raw, other):
    left = len(raw)
    right = len(other)
    return left OP right

def replaced(raw, other):
    value = len(raw)
    result = value OP (value := len(other))
    return result, value

def unbound(raw):
    if raw:
        value = len(raw)
    return value OP value

def deleted(raw):
    value = len(raw)
    del value
    return value OP value

class Mask:
    def __init__(self, label):
        self.label = label
    def __and__(self, other):
        print('and', self.label, self is other)
        return other
    def __or__(self, other):
        print('or', self.label, self is other)
        return other

def overloaded(value):
    return value OP value

raw = input()
other = input()
print(different(raw, other), replaced(raw, other))
for text in ('', raw):
    try:
        print(unbound(text))
    except UnboundLocalError:
        print('unbound')
try:
    deleted(raw)
except UnboundLocalError:
    print('deleted')
value = Mask(raw)
print(overloaded(value) is value)
try:
    print(overloaded(raw))
except TypeError:
    print('unsupported')
""".replace("OP", operator)
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for raw, other in (("abc", "12345"), ("", "abcdefg")):
        case.compare(stdin=raw + "\n" + other + "\n")


@pytest.mark.integration
@pytest.mark.parametrize("operator, operation", [("&", "%bit_and"), ("|", "%bit_or")])
def test_self_bitwise_integer_subclass_preserves_overrides(
    tmp_path, operator, operation
):
    source = """
class Integer(int):
    def __and__(self, other):
        print('integer and', self is other)
        return 73
    def __or__(self, other):
        print('integer or', self is other)
        return 91

def same(value):
    return value OP value

def different(left, right):
    return left OP right

value = Integer(len(input()))
other = Integer(len(input()))
print(same(value))
print(different(value, other))
""".replace("OP", operator)
    # Both forms must reach generic dispatch. The distinct-local control makes
    # a numeric subclass protocol failure distinguishable from idempotence.
    compilation = compile_source(source)
    for name in ("same", "different"):
        function = next(f for f in compilation.lambdas if f.source_name == name)
        assert list(known_calls(function, operation))
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="abc\n12345\n")
