"""Integer identities through numeric-record parsing, not injected IR1 facts."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import LambdaVar, Ref
from test.mule.compiler.rewrite_support import build_case

# The two orders have different matching branches and different reflected methods.
IDENTITIES = (
    ("add_right", "x + 0", "%add", "__add__"),
    ("add_left", "0 + x", "%add", "__radd__"),
    ("subtract", "x - 0", "%subtract", "__sub__"),
    ("multiply_right", "x * 1", "%multiply", "__mul__"),
    ("multiply_left", "1 * x", "%multiply", "__rmul__"),
    ("floor_divide", "x // 1", "%floor_divide", "__floordiv__"),
    ("power", "x ** 1", "%power", "__pow__"),
    ("or_right", "x | 0", "%bit_or", "__or__"),
    ("or_left", "0 | x", "%bit_or", "__ror__"),
    ("xor_right", "x ^ 0", "%bit_xor", "__xor__"),
    ("xor_left", "0 ^ x", "%bit_xor", "__rxor__"),
    ("and_right", "x & -1", "%bit_and", "__and__"),
    ("and_left", "-1 & x", "%bit_and", "__rand__"),
    ("left_shift", "x << 0", "%left_shift", "__lshift__"),
    ("right_shift", "x >> 0", "%right_shift", "__rshift__"),
    ("positive", "+x", "%unary_positive", "__pos__"),
)


def _normalizers(conversion):
    return "\n".join(
        f"def normalize_{label}(raw):\n x = {conversion}\n return {expression}\n"
        for label, expression, _operation, _method in IDENTITIES
    )


def _record_program(conversion):
    # Each record reaches every normalization stage. Invalid records must still
    # fail during conversion even though the subsequent arithmetic disappears.
    calls = "".join(
        " try:\n"
        f"  print('{label}', normalize_{label}(raw))\n"
        " except ValueError:\n"
        f"  print('{label}', 'invalid record')\n"
        for label, _expression, _operation, _method in IDENTITIES
    )
    return (
        _normalizers(conversion)
        + "\nfor raw in input().split():\n"
        + " print('record', raw)\n"
        + calls
    )


@pytest.mark.parametrize(("label", "expression", "operation", "method"), IDENTITIES)
def test_integer_identity_matches_normal_numeric_record_source(
    label, expression, operation, method
):
    compilation = compile_source(
        f"def normalize_{label}(raw):\n x = int(raw)\n return {expression}\n"
    )
    function = next(
        lam for lam in compilation.lambdas if lam.source_name == f"normalize_{label}"
    )
    assert not list(known_calls(function, operation)), expression
    producer = function.return_.result.use()
    assert isinstance(producer, Ref)
    assert isinstance(producer.leaf, LambdaVar)
    assert producer.leaf.name == "x"
    check_ir1_consistency(compilation.components)


@pytest.fixture(scope="module")
def integer_records(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("integer-identities"), _record_program("int(raw)")
    )


@pytest.mark.integration
def test_integer_identity_runtime_records_preserve_values_and_conversion_errors(
    integer_records,
):
    integer_records.compare(
        "0 1 -1 12345 -98765 4611686018427387904 "
        "-9223372036854775808 1267650600228229401496703205376 invalid\n"
    )


@pytest.mark.integration
@pytest.mark.parametrize(("label", "expression", "operation", "method"), IDENTITIES)
def test_integer_identity_optimized_parser_has_no_arithmetic_dispatch(
    integer_records, label, expression, operation, method
):
    body = integer_records.function_ir(rf"^mule_fn_normalize_{label}(?:\.|$)")
    # The real function must retain conversion of the runtime record. Identity
    # arithmetic must not move to a generic helper, a fixnum helper, or unary dispatch.
    assert re.search(r"\b(?:call|invoke)\b", body), body
    assert "ret ptr" in body
    assert not re.search(
        r"@(?:mule\.(?:fast|fixnum)\.(?:binary|unary)"
        r"|mule_binary|mule_unary|mule_numeric|mule_int_)",
        body,
    ), (expression, body)
    assert not re.search(r"@llvm\.(?:sadd|ssub|smul)\.with\.overflow", body), body


@pytest.fixture(scope="module")
def boolean_records(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("boolean-widening"),
        _record_program("bool(int(raw))"),
    )


@pytest.mark.integration
def test_integer_identity_boolean_records_widen_to_integer(boolean_records):
    # Printing distinguishes True/False from 1/0. Every integer-literal identity
    # here widens bool, including the mixed bool/int bitwise operators.
    boolean_records.compare("0 1 -3 999999999999999999999999999999 invalid\n")


@pytest.mark.parametrize(("label", "expression", "operation", "method"), IDENTITIES)
def test_integer_identity_boolean_and_unknown_sources_keep_operation(
    label, expression, operation, method
):
    for source in (
        f"def normalize_{label}(raw):\n x = bool(int(raw))\n return {expression}\n",
        f"def normalize_{label}(x):\n return {expression}\n",
    ):
        compilation = compile_source(source)
        function = next(
            lam
            for lam in compilation.lambdas
            if lam.source_name == f"normalize_{label}"
        )
        assert list(known_calls(function, operation)), expression
        check_ir1_consistency(compilation.components)


def _overloaded_records():
    source = "class Measurement:\n def __init__(self, value):\n  self.value = value\n"
    for label, _expression, _operation, method in IDENTITIES:
        parameters = "self" if method == "__pos__" else "self, other"
        source += (
            f" def {method}({parameters}):\n"
            f"  print('dispatch', '{label}', self.value)\n"
            "  self.value += 1\n"
            "  if self.value < 0:\n"
            "   raise ValueError('invalid measurement')\n"
            "  return self.value\n"
        )
    for label, expression, _operation, _method in IDENTITIES:
        source += f"def apply_{label}(x):\n return {expression}\n"
    source += "for raw in input().split():\n value = int(raw)\n"
    for label, _expression, _operation, _method in IDENTITIES:
        source += (
            " sample = Measurement(value)\n"
            " try:\n"
            f"  print('{label}', apply_{label}(sample))\n"
            " except ValueError:\n"
            "  print('invalid measurement')\n"
            " print('mutated', sample.value)\n"
            " try:\n"
            f"  apply_{label}(None)\n"
            " except TypeError:\n"
            "  print('unsupported measurement')\n"
        )
    return source


@pytest.mark.integration
def test_integer_identity_unknown_overloads_preserve_dispatch_mutation_and_errors(
    tmp_path,
):
    case = build_case(tmp_path, _overloaded_records())
    case.compare("12 -4 1267650600228229401496703205376\n")
