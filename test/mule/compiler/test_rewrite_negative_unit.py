"""Negative-unit rewrites through ordinary signed-number processing."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

NEGATIVE_UNITS = [
    pytest.param("x * -1", "%multiply", id="right-multiply"),
    pytest.param("-1 * x", "%multiply", id="left-multiply"),
    pytest.param("x // -1", "%floor_divide", id="floor-divide"),
]


@pytest.mark.parametrize(("expression", "operation"), NEGATIVE_UNITS)
def test_negative_unit_normal_pipeline_retargets_integer_operation(
    expression, operation
):
    compilation = compile_source(
        f"import mule_types\ndef reverse_charge(x: mule_types.int64):\n return {expression}\n"
    )
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == "reverse_charge"
    )
    assert not list(known_calls(function, operation))
    assert len(list(known_calls(function, "%unary_negative"))) == 1
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(("expression", "operation"), NEGATIVE_UNITS)
@pytest.mark.parametrize("conversion", ["bool(raw)", "float(raw)", "raw"])
def test_negative_unit_non_exact_integer_keeps_original_protocol(
    expression, operation, conversion
):
    compilation = compile_source(
        f"def reverse_charge(raw):\n x = {conversion}\n return {expression}\n"
    )
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == "reverse_charge"
    )
    assert list(known_calls(function, operation))
    assert not list(known_calls(function, "%unary_negative"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize(("expression", "operation"), NEGATIVE_UNITS)
def test_negative_unit_runtime_integer_processing_uses_real_negation(
    tmp_path, expression, operation
):
    # Parse transaction amounts at runtime, retain their originals, and reverse
    # each amount twice. Bigints must not be mutated or truncated by negation.
    case = build_case(
        tmp_path,
        "def reverse_charge(text):\n"
        " x = int(text)\n"
        f" return x, {expression}\n"
        "for text in input().split(','):\n"
        " original, reversed_amount = reverse_charge(text)\n"
        " print(original, reversed_amount, -reversed_amount == original)\n",
    )
    case.compare(
        stdin="0,1,-1,37,-53,4611686018427387904,-4611686018427387904,"
        "9223372036854775807,-9223372036854775808,"
        "1234567890123456789012345678901234567890,"
        "-1234567890123456789012345678901234567890\n"
    )
    body = case.function_ir(r"mule_fn_reverse_charge\.(body|spec\.)")
    # The alwaysinline tagged-integer unary helper must expose native negation.
    # The minimum tagged integer and heap bigints may take the unary fallback.
    assert re.search(r"\bsub i64 (?:0|1|2),", body), body
    assert "@mule_unary" in body
    assert "@mule_binary" not in body
    assert "mule.fast.binary" not in body
    assert not re.search(r"\b(?:mul|sdiv|udiv) i64\b", body)

    # Follow the linked physical unary implementation, not only its call site.
    # numeric::unary may inline into the ABI entry; GMP supplies heap negation.
    unary = case.function_assembly(r"(?:^mule_unary$|numeric.*unary|gmp.*neg)")
    assert re.search(r"\bneg[qwl]?\b|\bsub[qwl]?\b", unary), unary
    assert "__gmpz_neg" in unary, unary
    assert "__gmpz_mul" not in unary
    assert "__gmpz_fdiv" not in unary


@pytest.mark.integration
def test_negative_unit_dynamic_protocols_preserve_effects_and_result_identity(tmp_path):
    case = build_case(
        tmp_path,
        """
class Amount:
    def __init__(self, result):
        self.result = result
    def __mul__(self, other):
        print("multiply", other)
        return self.result
    def __rmul__(self, other):
        print("reflected", other)
        return self.result
    def __floordiv__(self, other):
        print("divide", other)
        return self.result
    def __neg__(self):
        raise ValueError("must not replace user protocols")

def reverse_charge(x):
    return x * -1, -1 * x, x // -1

def read_amount(result):
    print("read")
    return Amount(result)

result = [input()]
left, right, quotient = reverse_charge(read_amount(result))
print(left is result, right is result, quotient is result)
result.append("retained")
print(left, right, quotient)
for x in [True, False, 1.5, -1.5, 0.0, -0.0, "abc", None]:
    try:
        print(reverse_charge(x))
    except TypeError:
        print("TypeError")
try:
    reverse_charge(int("invalid"))
except ValueError:
    print("ValueError")
""",
    )
    case.compare(stdin="runtime-owned-result\n")
