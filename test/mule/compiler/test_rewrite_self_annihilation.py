"""Same-local arithmetic cancellation through source inference and record processing."""

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_lvar_value
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_RECORD_KERNELS = """
def clear_difference(raw):
    value = int(raw)
    return value, value - value

def clear_checksum(raw):
    value = int(raw)
    return value, value ^ value

def clear_flags(raw):
    value = bool(int(raw))
    return value, value ^ value
"""


@pytest.mark.parametrize(
    ("name", "operation", "literal_type", "expected"),
    [
        ("clear_difference", "%subtract", ast.IntegerLiteral, 0),
        ("clear_checksum", "%bit_xor", ast.IntegerLiteral, 0),
        ("clear_flags", "%bit_xor", ast.BooleanLiteral, False),
    ],
)
def test_source_inference_cancels_same_local_and_preserves_record_value(
    name, operation, literal_type, expected
):
    # int()/bool() establish exact types from unknown text parameters. No test
    # mutates LambdaVar.type or invokes a transform outside the normal pipeline.
    compilation = compile_source(_RECORD_KERNELS)
    function = next(fn for fn in compilation.lambdas if fn.source_name == name)
    assert not list(known_calls(function, operation))
    record = next(known_calls(function, "%build_tuple"))
    assert constant_lvar_value(record.args[0]) is None
    zero = constant_lvar_value(record.args[1])
    assert type(zero) is literal_type
    assert zero.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_runtime_records_keep_conversion_and_constant_reset_without_arithmetic(
    tmp_path,
):
    source = (
        _RECORD_KERNELS
        + """
for raw in input().split():
    try:
        print(clear_difference(raw), clear_checksum(raw), clear_flags(raw))
    except ValueError:
        print("invalid", raw)
"""
    )
    case = build_case(tmp_path, source, optimize=3, lto=False)
    # Includes fixnum extremes, heap-backed integers, both Boolean values, and a
    # failed conversion. An optimizer must retain parsing even when zero is known.
    result = case.compare(
        stdin=(
            "0 1 -1 9223372036854775807 -9223372036854775808 "
            "1267650600228229401496703205376 "
            "-1267650600228229401496703205376 invalid\n"
        )
    )
    assert "invalid invalid" in result.stdout
    kernels = case.function_ir(r"^mule_fn_clear_(?:difference|checksum|flags)\.body$")
    # These kernels retain runtime parsing and record construction, but no
    # arithmetic helper remains to conceal subtraction/XOR in another unit.
    assert "mule.fast.binary" not in kernels
    assert "mule_binary" not in kernels


_BOUNDARIES = """
def subtract_same(value):
    return value - value

def xor_same(value):
    return value ^ value

class Operand:
    def __init__(self, value):
        self.value = value

    def __sub__(self, other):
        print("subtract", self is other, self.value)
        return self

    def __xor__(self, other):
        print("xor", self is other, self.value)
        return self

class Cursor:
    def __init__(self, value):
        self.position = value

    @property
    def value(self):
        self.position += 1
        print("read", self.position)
        return self.position

def changed_subtract(raw):
    value = int(raw)
    return value - (value := value + 1), value

def changed_xor(raw):
    value = int(raw)
    return value ^ (value := value + 1), value

def unbound_subtract(raw, assign):
    if assign:
        value = int(raw)
    return value - value

def deleted_xor(raw):
    value = int(raw)
    del value
    return value ^ value

def read_number(raw):
    print("parse", raw)
    return int(raw)

raw = input()
operand = Operand(int(raw))
answer = subtract_same(operand)
print("sub identity", answer is operand)
answer.value += 1
print("sub mutation", operand.value)
answer = xor_same(operand)
print("xor identity", answer is operand)
answer.value += 1
print("xor mutation", operand.value)
print("bool", subtract_same(True), xor_same(True), xor_same(False))
print("changed", changed_subtract(raw), changed_xor(raw))
cursor = Cursor(int(raw))
print("attributes", cursor.value - cursor.value, cursor.value ^ cursor.value)
print("calls", read_number(raw) - read_number(raw),
      read_number(raw) ^ read_number(raw))
print("bound", unbound_subtract(raw, True))
try:
    unbound_subtract(raw, False)
except UnboundLocalError:
    print("unbound")
try:
    deleted_xor(raw)
except UnboundLocalError:
    print("deleted")
try:
    print(read_number("invalid") - read_number(raw))
except ValueError:
    print("first operand failed")
value = float("nan")
difference = subtract_same(value)
print("nan", difference != difference)
try:
    xor_same(value)
except TypeError:
    print("float xor error")
"""


@pytest.mark.integration
def test_same_local_near_misses_preserve_effects_mutation_errors_and_identity(tmp_path):
    case = build_case(tmp_path, _BOUNDARIES, optimize=3, lto=False)
    for raw in ("7", "1267650600228229401496703205376"):
        case.compare(stdin=raw + "\n")


@pytest.mark.parametrize(
    ("name", "operation"),
    [
        ("changed_subtract", "%subtract"),
        ("changed_xor", "%bit_xor"),
        ("unbound_subtract", "%subtract"),
        ("deleted_xor", "%bit_xor"),
        ("subtract_same", "%subtract"),
        ("xor_same", "%bit_xor"),
    ],
)
def test_source_near_misses_do_not_erase_required_arithmetic(name, operation):
    compilation = compile_source(_BOUNDARIES)
    function = next(fn for fn in compilation.lambdas if fn.source_name == name)
    assert list(known_calls(function, operation))
    check_ir1_consistency(compilation.components)
