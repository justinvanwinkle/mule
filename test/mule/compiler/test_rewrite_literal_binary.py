"""Literal folds must reach a runtime record consumer without arithmetic work."""

import operator
import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_BINARY = (
    ("+", "%add", operator.add),
    ("-", "%subtract", operator.sub),
    ("*", "%multiply", operator.mul),
    ("&", "%bit_and", operator.and_),
    ("|", "%bit_or", operator.or_),
    ("^", "%bit_xor", operator.xor),
)
_COMPARE = (
    ("==", "%compare_equal", operator.eq),
    ("!=", "%compare_not_equal", operator.ne),
    ("<", "%compare_less", operator.lt),
    ("<=", "%compare_less_equal", operator.le),
    (">", "%compare_greater", operator.gt),
    (">=", "%compare_greater_equal", operator.ge),
)


def _record_source(expression, name="kernel"):
    # The input record stays dynamic. Only the slice bound is foldable.
    return f"def {name}(record):\n return record[:({expression})]\n"


def _slice_bound(expression):
    compilation = compile_source(_record_source(expression))
    function = next(fn for fn in compilation.lambdas if fn.name == "mule_fn_kernel")
    slice_call = next(known_calls(function, "%getslice"))
    check_ir1_consistency(compilation.components)
    return function, constant_literal(slice_call.args[2])


@pytest.mark.parametrize("symbol,name,operation", _BINARY)
@pytest.mark.parametrize(
    "left,right",
    [(6, 3), (True, False), (False, True), (True, True), (True, 3), (3, True)],
)
def test_literal_binary_normal_ir_preserves_integer_or_boolean_result(
    symbol, name, operation, left, right
):
    function, bound = _slice_bound(f"{left!r} {symbol} {right!r}")
    expected = operation(left, right)
    expected_literal = (
        ast.BooleanLiteral if type(expected) is bool else ast.IntegerLiteral
    )
    assert isinstance(bound, expected_literal)
    assert bound.value == expected
    assert not list(known_calls(function, name))


@pytest.mark.parametrize("symbol,name,operation", _COMPARE)
@pytest.mark.parametrize(
    "left,right",
    [
        (7, 3),
        (3, 3),
        (True, False),
        (True, 1),
        (1, True),
        ("é", "😀"),
        ("", ""),
        (2**200, 2**201),
    ],
)
def test_literal_comparison_normal_ir_folds_all_six_relations(
    symbol, name, operation, left, right
):
    function, bound = _slice_bound(f"{left!r} {symbol} {right!r}")
    assert isinstance(bound, ast.BooleanLiteral)
    assert bound.value is operation(left, right)
    assert not list(known_calls(function, name))


@pytest.mark.parametrize(
    "expression,name,expected",
    [
        (f"{2**128 - 3} + 2", "%add", 2**128 - 1),
        (f"{2**128 - 1} + 1", "%add", None),
        (f"-({2**127}) - {2**127 - 1}", "%subtract", -(2**128 - 1)),
        (f"-({2**127}) - {2**127}", "%subtract", None),
        (f"{2**64 - 1} * {2**64 + 1}", "%multiply", 2**128 - 1),
        (f"{2**64} * {2**64}", "%multiply", None),
        (f"{2**129 + 7} & 3", "%bit_and", 3),
        (f"{2**128 - 1} & {2**128 - 2}", "%bit_and", 2**128 - 2),
        (f"{2**128 + 7} & {2**128 + 3}", "%bit_and", None),
        (f"{2**128 - 2} | 1", "%bit_or", 2**128 - 1),
        (f"{2**128} | 3", "%bit_or", None),
        (f"{2**128 - 2} ^ 1", "%bit_xor", 2**128 - 1),
        (f"{2**128} ^ 3", "%bit_xor", None),
        (f"{2**129 + 7} ^ {2**129 + 4}", "%bit_xor", 3),
    ],
)
def test_literal_binary_fold_limit_applies_to_result_not_operands(
    expression, name, expected
):
    function, bound = _slice_bound(expression)
    if expected is None:
        assert bound is None
        assert list(known_calls(function, name))
    else:
        assert isinstance(bound, ast.IntegerLiteral)
        assert bound.value == expected
        assert not list(known_calls(function, name))


@pytest.mark.parametrize(
    "expression,name",
    [
        ("'a' + 'b'", "%add"),
        ("'a' * 3", "%multiply"),
        ("3 * 'a'", "%multiply"),
        ("'a' - 'b'", "%subtract"),
        ("True & 'a'", "%bit_and"),
        ("'a' | True", "%bit_or"),
        ("'a' ^ 'b'", "%bit_xor"),
        ("1.5 + 2.5", "%add"),
        ("b'a' + b'b'", "%add"),
        ("1.5 < 2.5", "%compare_less"),
        ("b'a' < b'b'", "%compare_less"),
        ("'a' < 1", "%compare_less"),
    ],
)
def test_literal_binary_ineligible_domains_keep_operation(expression, name):
    function, bound = _slice_bound(expression)
    assert bound is None
    assert list(known_calls(function, name))


@pytest.mark.integration
def test_literal_binary_optimized_record_slicing_has_no_runtime_fold_work(tmp_path):
    cases = [
        (f"{left!r} {symbol} {right!r}", operation(left, right))
        for symbol, _name, operation in _BINARY
        for left, right in [(6, 3), (True, False), (True, 3), (3, True)]
    ]
    cases += [
        (f"{left!r} {symbol} {right!r}", operation(left, right))
        for symbol, _name, operation in _COMPARE
        for left, right in [(3, 7), (3, 3), (True, 1), ("é", "😀")]
    ]
    source = "".join(
        _record_source(expression, f"literal_record_{index}")
        for index, (expression, _expected) in enumerate(cases)
    )
    source += "record = input()\n"
    source += "".join(
        f"print(literal_record_{index}(record))\n" for index in range(len(cases))
    )
    case = build_case(tmp_path, source)
    # Clang keeps this shared slice helper rather than duplicating it in each body.
    # Follow its stop operand to the physical slice call, not just the entry wrapper.
    slice_helper = case.function_ir(r"^mule\.fast\.getslice$")
    sequence, stop = re.findall(r"ptr\s+(%[\w.]+)", slice_helper.splitlines()[0])
    assert re.search(
        r"@mule_fast_getslice\(ptr "
        + re.escape(sequence)
        + r", ptr nonnull @mule\.none, ptr "
        + re.escape(stop)
        + r", ptr nonnull @mule\.none\)",
        slice_helper,
    )
    for index, (_expression, expected) in enumerate(cases):
        body = case.function_ir(rf"^mule_fn_literal_record_{index}\.body$")
        _globals, record = re.findall(r"ptr\s+(%[\w.]+)", body.splitlines()[0])
        bound = (
            ("@mule.true" if expected else "@mule.false")
            if type(expected) is bool
            else f"inttoptr (i64 {(expected << 1) | 1} to ptr)"
        )
        # Require the folded value itself at the runtime-dependent slicing boundary.
        assert re.search(
            r"@mule\.fast\.getslice\(ptr "
            + re.escape(record)
            + r", ptr nonnull "
            + re.escape(bound)
            + r"\)",
            body,
        )
        assert not re.search(
            r"@mule(?:_|\.fast\.)(?:binary|compare|add|subtract|multiply|bit_and|bit_or|bit_xor)",
            body + slice_helper,
        )
    for record in ("abcdefghijk", "é😀records", ""):
        case.compare(stdin=record + "\n")


@pytest.mark.integration
def test_literal_binary_fallback_preserves_results_errors_and_record_effects(tmp_path):
    # __getitem__ observes the folded object's type, evaluation order, and ownership.
    source = """
class Record:
    def __init__(self, data):
        self.data = data
    def __getitem__(self, bound):
        print("bound", type(bound).__name__, bound)
        return self.data
def consume(record):
    print(record[True & False])
    print(record[True + False])
    print(record[True | 2])
    print(record[2 ^ True])
    print(record['é' < '😀'])
    print(record[340282366920938463463374607431768211455 + 1])
    print(record['a' + 'b'])
    print(record[3 * 'xy'])
    print(record[b'a' + b'b'])
    print(record[1.5 + 2.5])
    try:
        print(record['a' < 1])
    except TypeError:
        print("ordering error")
    try:
        print(record[True & 'a'])
    except TypeError:
        print("bitwise error")
consume(Record(input()))
"""
    case = build_case(tmp_path, source)
    case.compare(stdin="runtime-owned-record\n")
