"""Tests for modular arithmetic and associative integer constant transforms."""

import pytest

from mule import ast
from mule.compiler import modarith, srctran  # noqa: F401
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import Combination, Constant, LambdaVar, Ref, Return


def _compile_fn(source: str):
    compilation = compile_source(source)
    check_ir1_consistency(compilation.components)
    return compilation, compilation.lambdas[1]


def test_redundant_bitmask_nested_bit_and():
    _, fn = _compile_fn(
        "import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return (x & 0xff) & 0xffff\n"
    )
    bit_ands = list(known_calls(fn, "%bit_and"))
    assert len(bit_ands) == 1
    mask = constant_literal(bit_ands[0].args[1])
    assert isinstance(mask, ast.IntegerLiteral) and mask.value == 0xFF


def test_redundant_bitmask_commutative_nested_bit_and():
    _, fn = _compile_fn(
        "import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return 0xffff & (x & 0xff)\n"
    )
    bit_ands = list(known_calls(fn, "%bit_and"))
    assert len(bit_ands) == 1
    mask = constant_literal(bit_ands[0].args[1])
    assert isinstance(mask, ast.IntegerLiteral) and mask.value == 0xFF


def test_redundant_bitmask_ord_codepoint():
    _, fn = _compile_fn("def f(c):\n return ord(c) & 0x1fffff\n")
    assert list(known_calls(fn, "%bit_and")) == []


def test_redundant_bitmask_range_loop_and_narrowed_branch():
    # A `for ... in range(16)` proves fixnum range bounds (where `i` has type
    # IntegerType(0, 15)) and eliminates the generic fallback loop.
    # Redundant bitmask elimination removes the `%bit_and` in the counted loop.
    compilation = compile_source(
        "def f():\n"
        " total = 0\n"
        " for i in range(16):\n"
        "  y = i & 0xff\n"
        "  total = total + y\n"
        " return total\n"
    )
    check_ir1_consistency(compilation.components)
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%bit_and")) == []

    # In a branch-narrowed interval `0 <= x <= 15`, `x & 0xff` is eliminated completely.
    _, fn_branch = _compile_fn(
        "import mule_types\n"
        "def f(x: mule_types.int64) -> mule_types.int64:\n"
        " if x >= 0:\n"
        "  if x < 16:\n"
        "   return x & 0xff\n"
        " return 0\n"
    )
    assert list(known_calls(fn_branch, "%bit_and")) == []


def test_non_redundant_bitmask_preserved():
    # Non-contiguous mask is not a low-bit mask 2**k - 1
    compilation = compile_source(
        "def f():\n"
        " total = 0\n"
        " for i in range(16):\n"
        "  y = i & 0xf0\n"
        "  total = total + y\n"
        " return total\n"
    )
    check_ir1_consistency(compilation.components)
    fn = compilation.lambdas[1]
    assert len(list(known_calls(fn, "%bit_and"))) == 1

    # Negative lower bound may have high bits set in two's complement
    compilation_neg = compile_source(
        "def f():\n"
        " total = 0\n"
        " for i in range(-1, 16):\n"
        "  y = i & 0xff\n"
        "  total = total + y\n"
        " return total\n"
    )
    check_ir1_consistency(compilation_neg.components)
    fn_neg = compilation_neg.lambdas[1]
    assert len(list(known_calls(fn_neg, "%bit_and"))) == 1


@pytest.mark.parametrize(
    ("expr", "expected_op", "expected_const"),
    [
        ("(x + 3) + 5", "%add", 8),
        ("(x - 10) + 3", "%subtract", 7),
        ("(x + 3) - 10", "%subtract", 7),
        ("(x - 3) - 5", "%subtract", 8),
        ("(x + 10) - 3", "%add", 7),
        ("(x & 0xffff) & 0xff", "%bit_and", 0xFF),
        ("(x | 0x0f) | 0xf0", "%bit_or", 0xFF),
        ("(x ^ 0x0f) ^ 0xf0", "%bit_xor", 0xFF),
        ("(x >> 2) >> 3", "%right_shift", 5),
        ("(x << 1) << 4", "%left_shift", 5),
    ],
)
def test_associative_integer_constant_folding(expr, expected_op, expected_const):
    _, fn = _compile_fn(
        f"import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return {expr}\n"
    )
    calls = list(known_calls(fn, expected_op))
    assert len(calls) == 1
    right = constant_literal(calls[0].args[1])
    assert isinstance(right, ast.IntegerLiteral) and right.value == expected_const


@pytest.mark.parametrize(
    "expr",
    [
        "(x + 5) - 5",
        "(x - 5) + 5",
        "(x ^ 0xff) ^ 0xff",
    ],
)
def test_associative_integer_cancels_to_variable(expr):
    _, fn = _compile_fn(
        f"import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return {expr}\n"
    )
    for op in ("%add", "%subtract", "%bit_xor"):
        assert list(known_calls(fn, op)) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert not isinstance(producer, Combination)
    # No dead Constant references remain in the function
    const_refs = [
        node
        for block in fn.component.blocks()
        for node in block.nodes()
        if isinstance(node, Ref) and isinstance(node.leaf, Constant)
    ]
    assert const_refs == []


def test_associative_chained_three_steps():
    _, fn = _compile_fn(
        "import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return ((x + 1) + 2) + 3\n"
    )
    adds = list(known_calls(fn, "%add"))
    assert len(adds) == 1
    right = constant_literal(adds[0].args[1])
    assert isinstance(right, ast.IntegerLiteral) and right.value == 6


def test_associative_guards_preserve_semantics():
    # Unknown parameter type must not fold (x + 3) + 5
    _, fn_unknown = _compile_fn("def f(x):\n return (x + 3) + 5\n")
    assert len(list(known_calls(fn_unknown, "%add"))) == 2

    # Boolean operand must not collapse (b + 5) - 5 to b (since True + 5 - 5 is 1, not True)
    _, fn_bool = _compile_fn("def f(c):\n b = bool(c)\n return (b + 5) - 5\n")
    assert len(list(known_calls(fn_bool, "%add"))) == 1
    assert len(list(known_calls(fn_bool, "%subtract"))) == 1
    ret = fn_bool.return_
    assert isinstance(ret, Return)
    assert not (
        isinstance(ret.result.use(), Ref)
        and isinstance(ret.result.use().leaf, LambdaVar)
    )

    # Negative shift count must not fold
    _, fn_neg_shift = _compile_fn(
        "import mule_types\ndef f(x: mule_types.int64):\n return (x >> 2) >> -1\n"
    )
    assert len(list(known_calls(fn_neg_shift, "%right_shift"))) == 2


@pytest.mark.parametrize(
    ("expr", "op_name", "expected_const"),
    [
        ("(3 + x) + 5", "%add", 8),
        ("(10 + x) - 3", "%add", 7),
        ("5 + (x + 3)", "%add", 8),
        ("3 + (x - 10)", "%subtract", 7),
        ("(0xFF & x) & 0x0F", "%bit_and", 0x0F),
        ("0x0F & (x & 0xFF)", "%bit_and", 0x0F),
        ("(0x10 | x) | 0x01", "%bit_or", 0x11),
        ("0x01 | (x | 0x10)", "%bit_or", 0x11),
        ("(0xFF ^ x) ^ 0x0F", "%bit_xor", 0xF0),
        ("0x0F ^ (x ^ 0xFF)", "%bit_xor", 0xF0),
    ],
)
def test_commutative_associative_constant_combination(expr, op_name, expected_const):
    _, fn = _compile_fn(
        f"import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return {expr}\n"
    )
    calls = list(known_calls(fn, op_name))
    assert len(calls) == 1
    right = constant_literal(calls[0].args[1])
    assert isinstance(right, ast.IntegerLiteral)
    assert right.value == expected_const


@pytest.mark.parametrize("expr", ["(5 + x) - 5", "5 + (x - 5)"])
def test_commutative_associative_zero_offset_collapses_to_x(expr):
    _, fn = _compile_fn(
        f"import mule_types\ndef f(x: mule_types.int64) -> mule_types.int64:\n return {expr}\n"
    )
    assert list(known_calls(fn, "%add")) == []
    assert list(known_calls(fn, "%subtract")) == []
