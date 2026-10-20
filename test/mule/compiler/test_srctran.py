"""Tests for IR1 source transforms in ``mule.compiler.srctran``."""

import pytest

from mule import ast
from mule.compiler.ctype import (
    BOOLEAN,
    BOOLEAN_FALSE,
    BOOLEAN_TRUE,
    BYTES_TYPE,
    FLOAT,
    INTEGER,
    PYTHON_INTEGER,
    REAL,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
    TextType,
)
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import (
    constant_lvar_value,
    ir1_optimize_until_done,
    lvar_type,
)
from mule.compiler.ir1util import known_calls
from mule.compiler.knownfun import KNOWN
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import Constant, Ref, Return

_BEFORE_SRCTRAN = {name: info.transforms for name, info in KNOWN.items()}
from mule.compiler import srctran

_WITH_SRCTRAN = {name: info.transforms for name, info in KNOWN.items()}
for _name, _transforms in _BEFORE_SRCTRAN.items():
    object.__setattr__(KNOWN[_name], "transforms", _transforms)


@pytest.fixture(autouse=True)
def _activate_srctran_transforms():
    saved = {name: info.transforms for name, info in KNOWN.items()}
    for name, transforms in _WITH_SRCTRAN.items():
        object.__setattr__(KNOWN[name], "transforms", transforms)
    try:
        yield
    finally:
        for name, transforms in saved.items():
            object.__setattr__(KNOWN[name], "transforms", transforms)


def _compile_typed(source: str, param_types: dict[str, object]):
    compilation = compile_toplevel(source)
    fn = compilation.lambdas[1]
    for var in fn.vars:
        if var.name in param_types:
            var.type = param_types[var.name]
    assert fn.component is not None
    changed = ir1_optimize_until_done(fn.component)
    check_ir1_consistency(compilation.components)
    return compilation, fn, changed


def test_literal_and_type_helpers():
    assert srctran._is_exact_int_type(INTEGER)
    assert srctran._is_exact_int_type(IntegerType(-5, 10))
    assert not srctran._is_exact_int_type(BOOLEAN)
    assert not srctran._is_exact_int_type(BOOLEAN_TRUE)
    assert not srctran._is_exact_int_type(BOOLEAN_FALSE)
    assert not srctran._is_exact_int_type(PYTHON_INTEGER)
    assert not srctran._is_exact_int_type(UNKNOWN)

    assert srctran._power_of_two_exponent(None) is None
    assert srctran._power_of_two_exponent(-4) is None
    assert srctran._power_of_two_exponent(0) is None
    assert srctran._power_of_two_exponent(1) is None
    assert srctran._power_of_two_exponent(2) == 1
    assert srctran._power_of_two_exponent(3) is None
    assert srctran._power_of_two_exponent(8) == 3
    assert srctran._power_of_two_exponent(16) == 4
    assert srctran._power_of_two_exponent(1024) == 10

    compilation = compile_toplevel(
        "def f():\n return 7, True, False, 'hi', b'by', None\n"
    )
    fn = compilation.lambdas[1]
    tuple_call = next(known_calls(fn, "%build_tuple"))
    int_lvar, true_lvar, false_lvar, text_lvar, bytes_lvar, none_lvar = tuple_call.args

    assert srctran._exact_int_literal(int_lvar) == 7
    assert srctran._exact_int_literal(true_lvar) is None
    assert srctran._exact_int_literal(false_lvar) is None
    assert srctran._exact_int_literal(text_lvar) is None

    assert srctran._exact_bool_literal(true_lvar) is True
    assert srctran._exact_bool_literal(false_lvar) is False
    assert srctran._exact_bool_literal(int_lvar) is None

    assert srctran._exact_text_literal(text_lvar) == "hi"
    assert srctran._exact_text_literal(bytes_lvar) is None
    assert srctran._exact_text_literal(none_lvar) is None

    assert srctran._exact_bytes_literal(bytes_lvar) == b"by"
    assert srctran._exact_bytes_literal(text_lvar) is None


@pytest.mark.parametrize(
    ("expr", "op_name"),
    [
        ("x + 0", "%add"),
        ("0 + x", "%add"),
        ("x - 0", "%subtract"),
        ("x * 1", "%multiply"),
        ("1 * x", "%multiply"),
        ("x // 1", "%floor_divide"),
        ("x ** 1", "%power"),
        ("x | 0", "%bit_or"),
        ("0 | x", "%bit_or"),
        ("x ^ 0", "%bit_xor"),
        ("0 ^ x", "%bit_xor"),
        ("x & -1", "%bit_and"),
        ("-1 & x", "%bit_and"),
        ("x << 0", "%left_shift"),
        ("x >> 0", "%right_shift"),
        ("+x", "%unary_positive"),
    ],
)
def test_integer_identity_reductions(expr: str, op_name: str):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": INTEGER})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and producer.leaf is fn.vars[0]


@pytest.mark.parametrize(
    ("expr", "op_name", "param_type"),
    [
        ("s + ''", "%add", TEXT_TYPE),
        ("'' + s", "%add", TEXT_TYPE),
        ("s + ''", "%add", TextType("abc")),
        ("s * 1", "%multiply", TEXT_TYPE),
        ("1 * s", "%multiply", TEXT_TYPE),
        ("s + b''", "%add", BYTES_TYPE),
        ("b'' + s", "%add", BYTES_TYPE),
        ("s * 1", "%multiply", BYTES_TYPE),
        ("1 * s", "%multiply", BYTES_TYPE),
    ],
)
def test_sequence_identity_reductions(expr: str, op_name: str, param_type: object):
    _, fn, changed = _compile_typed(f"def f(s):\n return {expr}\n", {"s": param_type})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and producer.leaf is fn.vars[0]


@pytest.mark.parametrize(
    ("expr", "op_name"),
    [
        ("b | False", "%bit_or"),
        ("False | b", "%bit_or"),
        ("b ^ False", "%bit_xor"),
        ("False ^ b", "%bit_xor"),
        ("b & True", "%bit_and"),
        ("True & b", "%bit_and"),
    ],
)
def test_boolean_bitwise_identity_reductions(expr: str, op_name: str):
    _, fn, changed = _compile_typed(f"def f(b):\n return {expr}\n", {"b": BOOLEAN})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and producer.leaf is fn.vars[0]


@pytest.mark.parametrize(
    ("expr", "op_name", "param_type"),
    [
        # bool + 0 widens bool to int in Python (True + 0 == 1, not True)
        ("x + 0", "%add", BOOLEAN),
        ("0 + x", "%add", BOOLEAN),
        ("x + 0", "%add", PYTHON_INTEGER),
        ("x + 0", "%add", UNKNOWN),
        # int + False is not a syntactic IntegerLiteral(0)
        ("x + False", "%add", INTEGER),
        ("False + x", "%add", INTEGER),
        ("x * True", "%multiply", INTEGER),
        ("True * x", "%multiply", INTEGER),
        ("x * True", "%multiply", TEXT_TYPE),
        ("x * True", "%multiply", BYTES_TYPE),
        # 0 - x is -x, not x
        ("0 - x", "%subtract", INTEGER),
        # 1 // x and 1 ** x are not x
        ("1 // x", "%floor_divide", INTEGER),
        ("1 ** x", "%power", INTEGER),
        # +b on bool widens to int
        ("+x", "%unary_positive", BOOLEAN),
        # Non-identity constants stay as calls
        ("x + 2", "%add", INTEGER),
        ("x | 1", "%bit_or", INTEGER),
        ("x & 2", "%bit_and", INTEGER),
        ("x | True", "%bit_or", BOOLEAN),
        ("x & False", "%bit_and", BOOLEAN),
    ],
)
def test_non_identity_or_boolean_widening_not_reduced_to_lvar(
    expr: str, op_name: str, param_type: object
):
    _, fn, _ = _compile_typed(f"def f(x):\n return {expr}\n", {"x": param_type})
    assert len(list(known_calls(fn, op_name))) == 1


def test_true_plus_zero_folds_to_int_one_not_bool_true():
    compilation = compile_source("def f():\n return True + 0\n")
    fn = compilation.lambdas[1]
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and isinstance(producer.leaf, Constant)
    assert isinstance(producer.leaf.value, ast.IntegerLiteral)
    assert not isinstance(producer.leaf.value, ast.BooleanLiteral)
    assert producer.leaf.value.value == 1
    check_ir1_consistency(compilation.components)


def test_unary_involution_reductions():
    _, fn_neg, changed_neg = _compile_typed(
        "def f(x):\n return -(-x)\n", {"x": INTEGER}
    )
    assert changed_neg
    assert list(known_calls(fn_neg, "%unary_negative")) == []
    producer_neg = fn_neg.return_.result.use()
    assert isinstance(producer_neg, Ref) and producer_neg.leaf is fn_neg.vars[0]

    _, fn_inv, changed_inv = _compile_typed(
        "def f(x):\n return ~(~x)\n", {"x": INTEGER}
    )
    assert changed_inv
    assert list(known_calls(fn_inv, "%unary_invert")) == []
    producer_inv = fn_inv.return_.result.use()
    assert isinstance(producer_inv, Ref) and producer_inv.leaf is fn_inv.vars[0]

    # Quadruple negation reduces in sequence to x
    _, fn_quad, changed_quad = _compile_typed(
        "def f(x):\n return -(-(-(-x)))\n", {"x": INTEGER}
    )
    assert changed_quad
    assert list(known_calls(fn_quad, "%unary_negative")) == []
    producer_quad = fn_quad.return_.result.use()
    assert isinstance(producer_quad, Ref) and producer_quad.leaf is fn_quad.vars[0]

    # Mixed unary operators do NOT cancel
    _, fn_mixed, _ = _compile_typed("def f(x):\n return -(~x)\n", {"x": INTEGER})
    assert len(list(known_calls(fn_mixed, "%unary_negative"))) == 1
    assert len(list(known_calls(fn_mixed, "%unary_invert"))) == 1

    # Boolean inner operand must NOT reduce -(-b) or ~(~b) to b (since result is int, not bool)
    _, fn_bool_neg, _ = _compile_typed("def f(b):\n return -(-b)\n", {"b": BOOLEAN})
    assert len(list(known_calls(fn_bool_neg, "%unary_negative"))) == 2

    _, fn_bool_inv, _ = _compile_typed("def f(b):\n return ~(~b)\n", {"b": BOOLEAN})
    assert len(list(known_calls(fn_bool_inv, "%unary_invert"))) == 2


def test_power_of_two_floor_divide_and_modulo_strength_reductions():
    _, fn_div, changed_div = _compile_typed(
        "def f(x):\n return x // 8\n", {"x": INTEGER}
    )
    assert changed_div
    assert list(known_calls(fn_div, "%floor_divide")) == []
    rshifts = list(known_calls(fn_div, "%right_shift"))
    assert len(rshifts) == 1
    rshift = rshifts[0]
    rhs_ref = rshift.args[1].use()
    assert isinstance(rhs_ref, Ref) and isinstance(rhs_ref.leaf, Constant)
    assert isinstance(rhs_ref.leaf.value, ast.IntegerLiteral)
    assert rhs_ref.leaf.value.value == 3
    assert lvar_type(rshift.args[1]) == IntegerType(3, 3)

    _, fn_mod, changed_mod = _compile_typed(
        "def f(x):\n return x % 16\n", {"x": INTEGER}
    )
    assert changed_mod
    assert list(known_calls(fn_mod, "%modulo")) == []
    bit_ands = list(known_calls(fn_mod, "%bit_and"))
    assert len(bit_ands) == 1
    bit_and = bit_ands[0]
    mask_ref = bit_and.args[1].use()
    assert isinstance(mask_ref, Ref) and isinstance(mask_ref.leaf, Constant)
    assert isinstance(mask_ref.leaf.value, ast.IntegerLiteral)
    assert mask_ref.leaf.value.value == 15
    assert lvar_type(bit_and.args[1]) == IntegerType(15, 15)
    assert bit_and.lvar is not None
    assert lvar_type(bit_and.lvar) == IntegerType(0, 15)

    # Non-power-of-two or negative divisors stay as %floor_divide / %modulo
    _, fn_non_pow2, _ = _compile_typed(
        "def f(x):\n return (x // 6) + (x % 6) + (x // -4) + (x % -4)\n",
        {"x": INTEGER},
    )
    assert len(list(known_calls(fn_non_pow2, "%floor_divide"))) == 2
    assert len(list(known_calls(fn_non_pow2, "%modulo"))) == 2


def test_srctran_end_to_end_with_compile_source_and_negative_integers():
    source = (
        "import mule\n"
        "def f(x: mule.int64):\n"
        " b = x > 0\n"
        " return (x + 0, x * 1, -(-x), ~(~x), x // 8, x % 16, b | False, b & True)\n"
        "def neg_div_mod():\n"
        " return [(-45) // 8, (-45) % 16, (-16) // 8, (-16) % 16, 45 // 8, 45 % 16]\n"
    )
    compilation = compile_source(source)
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%add")) == []
    assert list(known_calls(fn, "%multiply")) == []
    assert list(known_calls(fn, "%unary_negative")) == []
    assert list(known_calls(fn, "%unary_invert")) == []
    assert list(known_calls(fn, "%floor_divide")) == []
    assert list(known_calls(fn, "%modulo")) == []
    assert list(known_calls(fn, "%bit_or")) == []
    assert len(list(known_calls(fn, "%right_shift"))) == 1
    assert len(list(known_calls(fn, "%bit_and"))) == 1

    neg_fn = next(fn for fn in compilation.lambdas if fn.source_name == "neg_div_mod")
    assert list(known_calls(neg_fn, "%floor_divide")) == []
    assert list(known_calls(neg_fn, "%modulo")) == []
    list_call = next(known_calls(neg_fn, "%build_list"))
    values = [constant_lvar_value(arg) for arg in list_call.args]
    assert [v.value for v in values] == [-6, 3, -2, 0, 5, 13]
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expr", "op_name", "param_type", "expected_cls", "expected_val"),
    [
        ("x - x", "%subtract", INTEGER, ast.IntegerLiteral, 0),
        ("x ^ x", "%bit_xor", INTEGER, ast.IntegerLiteral, 0),
        ("x ^ x", "%bit_xor", BOOLEAN, ast.BooleanLiteral, False),
    ],
)
def test_same_variable_annihilation_to_constant_and_no_dead_refs(
    expr: str,
    op_name: str,
    param_type: object,
    expected_cls: type,
    expected_val: object,
):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": param_type})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    var = fn.vars[0]
    assert len(var.refs) == 0
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and isinstance(producer.leaf, Constant)
    assert type(producer.leaf.value) is expected_cls
    assert producer.leaf.value.value == expected_val


@pytest.mark.parametrize(
    ("expr", "op_name", "param_type"),
    [
        ("x & x", "%bit_and", INTEGER),
        ("x | x", "%bit_or", INTEGER),
        ("x & x", "%bit_and", BOOLEAN),
        ("x | x", "%bit_or", BOOLEAN),
    ],
)
def test_same_variable_idempotent_bitwise_reductions(
    expr: str, op_name: str, param_type: object
):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": param_type})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    var = fn.vars[0]
    assert len(var.refs) == 1
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and producer.leaf is var


@pytest.mark.parametrize(
    "param_type",
    [INTEGER, BOOLEAN, PYTHON_INTEGER, TEXT_TYPE, TextType("hi"), BYTES_TYPE],
)
@pytest.mark.parametrize(
    ("expr", "op_name", "expected"),
    [
        ("x == x", "%compare_equal", True),
        ("x != x", "%compare_not_equal", False),
        ("x < x", "%compare_less", False),
        ("x <= x", "%compare_less_equal", True),
        ("x > x", "%compare_greater", False),
        ("x >= x", "%compare_greater_equal", True),
    ],
)
def test_same_variable_reflexive_comparisons(
    param_type: object, expr: str, op_name: str, expected: bool
):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": param_type})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    var = fn.vars[0]
    assert len(var.refs) == 0
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and isinstance(producer.leaf, Constant)
    assert isinstance(producer.leaf.value, ast.BooleanLiteral)
    assert producer.leaf.value.value is expected


@pytest.mark.parametrize("param_type", [FLOAT, REAL, UNKNOWN])
@pytest.mark.parametrize(
    ("expr", "op_name"),
    [
        ("f == f", "%compare_equal"),
        ("f != f", "%compare_not_equal"),
        ("f < f", "%compare_less"),
        ("f <= f", "%compare_less_equal"),
        ("f > f", "%compare_greater"),
        ("f >= f", "%compare_greater_equal"),
    ],
)
def test_float_and_unknown_comparisons_not_folded(
    param_type: object, expr: str, op_name: str
):
    _, fn, _ = _compile_typed(f"def g(f):\n return {expr}\n", {"f": param_type})
    assert len(list(known_calls(fn, op_name))) == 1
    assert len(fn.vars[0].refs) == 2


@pytest.mark.parametrize("param_type", [UNKNOWN, INTEGER, FLOAT, TEXT_TYPE])
@pytest.mark.parametrize(
    ("expr", "op_name", "expected"),
    [
        ("x is x", "%compare_is", True),
        ("x is not x", "%compare_is_not", False),
    ],
)
def test_same_variable_reflexive_identity(
    param_type: object, expr: str, op_name: str, expected: bool
):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": param_type})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    var = fn.vars[0]
    assert len(var.refs) == 0
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and isinstance(producer.leaf, Constant)
    assert isinstance(producer.leaf.value, ast.BooleanLiteral)
    assert producer.leaf.value.value is expected


def test_same_variable_unannotated_and_negative_cases():
    # Unannotated parameter: x is x -> True, x is not x -> False via compile_source
    compilation = compile_source(
        "def is_same(x):\n return x is x\ndef is_not_same(x):\n return x is not x\n"
    )
    fn_is = next(fn for fn in compilation.lambdas if fn.source_name == "is_same")
    fn_is_not = next(
        fn for fn in compilation.lambdas if fn.source_name == "is_not_same"
    )
    assert list(known_calls(fn_is, "%compare_is")) == []
    assert len(fn_is.vars[0].refs) == 0
    assert fn_is.return_ is not None
    assert constant_lvar_value(fn_is.return_.result).value is True

    assert list(known_calls(fn_is_not, "%compare_is_not")) == []
    assert len(fn_is_not.vars[0].refs) == 0
    assert fn_is_not.return_ is not None
    assert constant_lvar_value(fn_is_not.return_.result).value is False
    check_ir1_consistency(compilation.components)

    # Different locals x - y and x == y are not folded
    _, fn_diff, _ = _compile_typed(
        "def f(x, y):\n return (x - y) + (1 if x == y else 0)\n",
        {"x": INTEGER, "y": INTEGER},
    )
    assert len(list(known_calls(fn_diff, "%subtract"))) == 1
    assert len(list(known_calls(fn_diff, "%compare_equal"))) == 1

    # Unbound local must not fold x - x or x is x
    _, fn_unbound, _ = _compile_typed(
        "def f(x):\n return x - x\n",
        {"x": INTEGER.with_unbound()},
    )
    assert len(list(known_calls(fn_unbound, "%subtract"))) == 1
    assert len(fn_unbound.vars[0].refs) == 2


@pytest.mark.parametrize(
    ("expr", "op_name"),
    [
        ("x * 0", "%multiply"),
        ("0 * x", "%multiply"),
        ("x % 1", "%modulo"),
        ("x % -1", "%modulo"),
        ("x & 0", "%bit_and"),
        ("0 & x", "%bit_and"),
    ],
)
def test_absorbing_zero_reductions_and_local_ref_cleanup(expr: str, op_name: str):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": INTEGER})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    var = fn.vars[0]
    assert len(var.refs) == 0
    ret = fn.return_
    assert isinstance(ret, Return)
    producer = ret.result.use()
    assert isinstance(producer, Ref) and isinstance(producer.leaf, Constant)
    assert type(producer.leaf.value) is ast.IntegerLiteral
    assert producer.leaf.value.value == 0


@pytest.mark.parametrize(
    ("expr", "op_name"),
    [
        ("x * -1", "%multiply"),
        ("-1 * x", "%multiply"),
        ("x // -1", "%floor_divide"),
    ],
)
def test_negative_unit_multiply_and_floor_divide_reductions(expr: str, op_name: str):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": INTEGER})
    assert changed
    assert list(known_calls(fn, op_name)) == []
    neg_calls = list(known_calls(fn, "%unary_negative"))
    assert len(neg_calls) == 1
    neg_call = neg_calls[0]
    assert len(neg_call.args) == 1
    arg_ref = neg_call.args[0].use()
    assert isinstance(arg_ref, Ref) and arg_ref.leaf is fn.vars[0]
    assert len(fn.vars[0].refs) == 1


@pytest.mark.parametrize(
    ("expr", "expected_shift"),
    [
        ("x * 2", 1),
        ("2 * x", 1),
        ("x * 8", 3),
        ("8 * x", 3),
        ("x * 1024", 10),
        ("1024 * x", 10),
    ],
)
def test_power_of_two_multiply_strength_reductions(expr: str, expected_shift: int):
    _, fn, changed = _compile_typed(f"def f(x):\n return {expr}\n", {"x": INTEGER})
    assert changed
    assert list(known_calls(fn, "%multiply")) == []
    lshifts = list(known_calls(fn, "%left_shift"))
    assert len(lshifts) == 1
    lshift = lshifts[0]
    lhs_ref = lshift.args[0].use()
    rhs_ref = lshift.args[1].use()
    assert isinstance(lhs_ref, Ref) and lhs_ref.leaf is fn.vars[0]
    assert isinstance(rhs_ref, Ref) and isinstance(rhs_ref.leaf, Constant)
    assert isinstance(rhs_ref.leaf.value, ast.IntegerLiteral)
    assert rhs_ref.leaf.value.value == expected_shift
    assert lvar_type(lshift.args[1]) == IntegerType(expected_shift, expected_shift)


def test_absorbing_zero_rejects_impure_or_unbound_operands():
    # A call with potential side effects must NOT fold when multiplied/anded/modded by 0/1
    source = (
        "import mule\n"
        "def g(x: mule.int64) -> mule.int64:\n"
        " return x + 1\n"
        "def caller(x: mule.int64):\n"
        " return (g(x) * 0, 0 * g(x), g(x) % 1, g(x) % -1, g(x) & 0, 0 & g(x))\n"
    )
    compilation = compile_source(source)
    caller_fn = next(fn for fn in compilation.lambdas if fn.source_name == "caller")
    assert len(list(known_calls(caller_fn, "%multiply"))) == 2
    assert len(list(known_calls(caller_fn, "%modulo"))) == 2
    assert len(list(known_calls(caller_fn, "%bit_and"))) == 2
    check_ir1_consistency(compilation.components)

    # Possibly unbound local must not fold x * 0, x % 1, or x & 0 to 0
    for expr, op_name in [
        ("x * 0", "%multiply"),
        ("0 * x", "%multiply"),
        ("x % 1", "%modulo"),
        ("x % -1", "%modulo"),
        ("x & 0", "%bit_and"),
        ("0 & x", "%bit_and"),
    ]:
        _, fn_unbound, _ = _compile_typed(
            f"def f(x):\n return {expr}\n",
            {"x": INTEGER.with_unbound()},
        )
        assert len(list(known_calls(fn_unbound, op_name))) == 1
        assert len(fn_unbound.vars[0].refs) == 1

    # End-to-end compile_source with 8 * x, x * 8, x * -1, -1 * x, x // -1, x * 0, 0 * x
    e2e = compile_source(
        "import mule\n"
        "def h(x: mule.int64):\n"
        " return (x * 0, 0 * x, x * -1, -1 * x, x * 8, 8 * x, x // -1, x % 1, x % -1, x & 0, 0 & x)\n"
    )
    h_fn = next(fn for fn in e2e.lambdas if fn.source_name == "h")
    assert list(known_calls(h_fn, "%multiply")) == []
    assert list(known_calls(h_fn, "%floor_divide")) == []
    assert list(known_calls(h_fn, "%modulo")) == []
    assert list(known_calls(h_fn, "%bit_and")) == []
    assert len(list(known_calls(h_fn, "%unary_negative"))) == 3
    assert len(list(known_calls(h_fn, "%left_shift"))) == 2
    check_ir1_consistency(e2e.components)
