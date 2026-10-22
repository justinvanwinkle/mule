"""Interval rewrites through normal flow inference and runtime record indexing."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import If
from test.mule.compiler.rewrite_support import build_case

COMPARISONS = (
    ("<", "%compare_less", True),
    ("<=", "%compare_less_equal", True),
    (">", "%compare_greater", False),
    (">=", "%compare_greater_equal", False),
    ("==", "%compare_equal", False),
    ("!=", "%compare_not_equal", True),
)


@pytest.mark.parametrize("operator,known,expected", COMPARISONS)
@pytest.mark.parametrize("reverse", (False, True))
def test_disjoint_runtime_index_intervals_remove_comparison_and_branch(
    operator, known, expected, reverse
):
    # The indices remain runtime values. Their masks prove disjoint intervals.
    comparison = f"b {operator} a" if reverse else f"a {operator} b"
    source = f"""
def lookup(left: int, right: int, record):
    a = left & 15
    b = (right & 15) + 16
    if {comparison}:
        return record[a]
    return record[b]
"""
    compilation = compile_source(source)
    function = next(f for f in compilation.lambdas if f.source_name == "lookup")
    assert not list(known_calls(function, known))
    assert function.component is not None
    assert not any(
        isinstance(node, If)
        for block in function.component.blocks()
        for node in block.nodes()
    )
    # Removing the branch must preserve the selected runtime indexing operation.
    assert list(known_calls(function, "%getitem"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("operator,known,_", COMPARISONS)
def test_overlapping_and_unbounded_intervals_retain_runtime_comparisons(
    operator, known, _
):
    for assignments in ("a = left & 15\n    b = right & 15", "a = left\n    b = right"):
        compilation = compile_source(
            f"def compare(left: int, right: int):\n    {assignments}\n"
            f"    return a {operator} b\n"
        )
        function = next(f for f in compilation.lambdas if f.source_name == "compare")
        assert list(known_calls(function, known))
        check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "operator,known,folded",
    (
        ("<", "%compare_less", False),
        ("<=", "%compare_less_equal", True),
        (">", "%compare_greater", True),
        (">=", "%compare_greater_equal", False),
        ("==", "%compare_equal", False),
        ("!=", "%compare_not_equal", False),
    ),
)
def test_touching_interval_endpoints_preserve_possible_equality(
    operator, known, folded
):
    compilation = compile_source(
        "def compare(left: int, right: int):\n"
        "    a = left & 15\n"
        "    b = (right & 15) + 15\n"
        f"    return a {operator} b\n"
    )
    function = next(f for f in compilation.lambdas if f.source_name == "compare")
    assert bool(list(known_calls(function, known))) is not folded
    check_ir1_consistency(compilation.components)


def test_branch_narrowing_removes_redundant_negative_index_guard():
    # One-based input positions become exact integers, including when position is bool.
    # An int annotation alone also admits bool, outside this transform's eligibility.
    compilation = compile_source(
        "def lookup(position: int, record):\n"
        "    index = position - 1\n"
        "    if index < 0:\n"
        "        return record[index]\n"
        "    if index < 0:\n"
        "        return record[0]\n"
        "    return record[index]\n"
    )
    function = next(f for f in compilation.lambdas if f.source_name == "lookup")
    # The first guard remains necessary. Only its nonnegative edge kills the second.
    assert len(list(known_calls(function, "%compare_less"))) == 1
    check_ir1_consistency(compilation.components)


def _indexing_source():
    functions = []
    calls = []
    for operator, known, expected in COMPARISONS:
        suffix = known.removeprefix("%compare_")
        functions.append(
            f"""
def index_{suffix}(left: int, right: int, record):
    a = left & 15
    b = (right & 15) + 16
    if a {operator} b:
        return record[a]
    return record[b]
"""
        )
        calls.append(f"print(index_{suffix}(left, right, record))")
    return (
        "\n".join(functions)
        + "\n"
        + "\n".join(
            (
                "left = int(input())",
                "right = int(input())",
                "record = input()",
                *calls,
            )
        )
    )


def _assert_masked_index_has_no_interval_comparison(body):
    values = r"%[-a-zA-Z$._0-9]+"
    definitions = dict(re.findall(rf"^\s*({values}) = ([^\n]+)", body, re.MULTILINE))
    # LLVM can retain the tagged representation: x & 15 then uses mask 31.
    masks = {
        result
        for result, operation in definitions.items()
        if re.search(r"^and i(?:32|64) .*, (?:15|31)$", operation)
    }
    assert masks, body
    index_values = set(masks)
    value_operations = (
        "ptrtoint",
        "inttoptr",
        "ashr",
        "lshr",
        "shl",
        "and",
        "or",
        "xor",
        "add",
        "sub",
        "mul",
        "phi",
        "select",
        "zext",
        "sext",
        "trunc",
        "freeze",
        "bitcast",
    )
    while True:
        derived = {
            result
            for result, operation in definitions.items()
            if operation.split()[0] in value_operations
            and index_values.intersection(re.findall(values, operation))
        }
        if derived <= index_values:
            break
        index_values.update(derived)

    # The payload mask must feed actual indexing, not just an ownership tag test.
    indices = re.findall(
        rf"(?:call|invoke)[^\n]*@mule_fast_getitem\(ptr [^,]+, ptr (?:nonnull )?({values})\)",
        body,
    )
    assert index_values.intersection(indices), body
    for operation in definitions.values():
        comparison = re.match(r"icmp (\w+)\b", operation)
        if comparison is None:
            continue
        operands = re.findall(values, operation)
        index_operands = [operand for operand in operands if operand in index_values]
        # Null/tag checks and unsigned range assumptions are required bookkeeping.
        # Reject signed tests on either index and any comparison between indices.
        assert not (
            index_operands
            and (
                comparison[1] in {"slt", "sle", "sgt", "sge"} or len(index_operands) > 1
            )
        ), operation


@pytest.mark.integration
def test_optimized_interval_indexing_keeps_native_masks_without_comparison_dispatch(
    tmp_path,
):
    case = build_case(tmp_path, _indexing_source(), optimize=3, lto=False)
    for left, right in ((0, 0), (15, 15), (-1, -16), (1234567, -1234567)):
        case.compare(stdin=f"{left}\n{right}\n0123456789ABCDEFGHIJKLMNOPQRSTUV\n")
    for _, known, _ in COMPARISONS:
        suffix = known.removeprefix("%compare_")
        body = case.function_ir(rf"mule_fn_index_{suffix}\.(?:body|spec\.)")
        # The interval decision must disappear, not move into a comparison helper.
        assert not re.search(r"(?:call|invoke)[^\n]*@[^\n]*(?:compare|icmp)", body), (
            body
        )
        for definition in re.split(r"(?=^define\b)", body, flags=re.MULTILINE):
            if definition.startswith("define"):
                _assert_masked_index_has_no_interval_comparison(definition)


@pytest.mark.integration
def test_interval_near_misses_keep_effects_endpoints_and_negative_indices(tmp_path):
    source = """
def observe(label, value):
    print(label, value)
    if label == 'right' and value == 30:
        raise ValueError('observed endpoint')
    return value

def classify(left: int, right: int):
    a = left & 15
    b = right & 15
    print(a < b, a <= b, a > b, a >= b, a == b, a != b)
    c = b + 15
    print(a < c, a <= c, a > c, a >= c, a == c, a != c)
    print(left < right, left <= right, left > right, left >= right,
          left == right, left != right)
    print(observe('left', a) < observe('right', c))

def signed_index(index: int, record):
    if index < 0:
        print('negative')
    else:
        if index < 0:
            print('unreachable')
        print('nonnegative')
    return record[index]

left = int(input())
right = int(input())
record = input()
try:
    classify(left, right)
except ValueError:
    print('ValueError')
try:
    print(signed_index(left, record))
except IndexError:
    print('IndexError')
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for left, right in (
        (0, 0),
        (15, 0),
        (0, 15),
        (-1, 15),
        (-32, 0),
        (-33, 1),
        (32, 1),
    ):
        case.compare(stdin=f"{left}\n{right}\n0123456789ABCDEFGHIJKLMNOPQRSTUV\n")
