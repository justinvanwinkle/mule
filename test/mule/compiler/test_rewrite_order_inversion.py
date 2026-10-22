"""Negated comparisons in runtime record-filter predicates.

IR1 checks distinguish inversion from a backend which merely negates the old
comparison result. Native tests exercise the predicates through Python's filter
algorithm with records supplied at execution time.
"""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

# The index is the native comparison ABI opcode, not an LLVM temporary.
_OPERATORS = (
    ("eq", "==", "equal", "not_equal", 1, "ne"),
    ("ne", "!=", "not_equal", "equal", 0, "eq"),
    ("lt", "<", "less", "greater_equal", 5, "sge"),
    ("le", "<=", "less_equal", "greater", 4, "sgt"),
    ("gt", ">", "greater", "less_equal", 3, "sle"),
    ("ge", ">=", "greater_equal", "less", 2, "slt"),
)
_CONVERSIONS = {
    "integer": "int(row[{index}])",
    "boolean": "bool(int(row[{index}]))",
    "text": "str(row[{index}])",
    "bytes": "bytes([int(row[{index}])])",
}


def _predicate(domain, suffix, operator):
    conversion = _CONVERSIONS[domain]
    return (
        f"def keep_{domain}_{suffix}(row):\n"
        f"    left = {conversion.format(index=0)}\n"
        f"    right = {conversion.format(index=1)}\n"
        f"    return not (left {operator} right)\n"
    )


def _filter_program():
    definitions = []
    consumers = ["rows = [input().split(), input().split(), input().split()]\n"]
    for domain in _CONVERSIONS:
        for suffix, operator, *_ in _OPERATORS:
            definitions.append(_predicate(domain, suffix, operator))
            consumers.append(f"print(list(filter(keep_{domain}_{suffix}, rows)))\n")
    return "\n".join(definitions + consumers)


@pytest.fixture(scope="module")
def exact_order_compilation():
    return compile_source(_filter_program())


def _assert_runtime_comparison_has_no_extra_truth(case):
    wrapper = case.function_assembly(r"^mule_compare$")
    target = re.search(r"\bjmp\s+[0-9a-f]+ <([^>]+)>", wrapper)
    assert target, wrapper
    # The C ABI wrapper must forward the inverse opcode without changing it.
    assert not re.search(r"\b(?:mov|xor|add|sub|not|neg)\b[^\n]*%[er]di", wrapper)
    pending = [target[1], "mule_text_eq"]
    visited = set()
    while pending:
        symbol = pending.pop()
        if symbol in visited:
            continue
        visited.add(symbol)
        body = case.function_assembly(rf"^{re.escape(symbol)}$")
        assert not re.search(
            r"<(?:[^>]*(?:truth|coerce_compare_bool)|mule_unary)[^>]*>", body
        ), body
        for callee in re.findall(r"\b(?:call|jmp)\s+[^\n]*<([^>]+)>", body):
            if "+0x" in callee:
                continue
            # Exact text/bytes compare returns a boolean directly. These cold
            # boundaries retain ordinary numeric/BigInt and custom dispatch,
            # or sequence-element comparison, rather than the rewritten not.
            if "compare_slow" in callee or "compare_sequence_storage_slow" in callee:
                continue
            if callee.startswith(("memcmp@", "bcmp@")):
                continue
            pending.append(callee)
    assert any("text" in symbol for symbol in visited), visited


@pytest.mark.parametrize("domain", _CONVERSIONS)
@pytest.mark.parametrize(
    "suffix,operator,original,inverse,opcode,predicate", _OPERATORS
)
def test_filter_predicate_exact_order_emits_inverse_ir1(
    exact_order_compilation,
    domain,
    suffix,
    operator,
    original,
    inverse,
    opcode,
    predicate,
):
    compilation = exact_order_compilation
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == f"keep_{domain}_{suffix}"
    )
    assert list(known_calls(function, "%unary_not")) == []
    assert list(known_calls(function, f"%compare_{original}")) == []
    assert list(known_calls(function, f"%compare_{inverse}"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_filter_exact_order_has_optimized_inverse_comparison_and_oracle_results(
    tmp_path,
):
    case = build_case(tmp_path, _filter_program(), optimize=3, lto=False)
    for records in ("2 10\n10 2\n2 2\n", "0 1\n1 0\n0 0\n"):
        case.compare(stdin=records)

    for domain in _CONVERSIONS:
        for suffix, _, _, _, opcode, predicate in _OPERATORS:
            body = case.function_ir(rf"^mule_fn_keep_{domain}_{suffix}\.body$")
            # No separate boolean-negation operation may survive in this path.
            assert "@mule.fast.unary.3" not in body
            assert "@mule_unary" not in body
            if domain != "boolean":
                assert "@mule_truth(" not in body
            native = re.search(rf"\bicmp {predicate} i64\b", body)
            runtime = re.search(
                rf"@mule_compare(?:_bool)?\(i32(?: [a-z_]+)* {opcode},", body
            )
            helper = re.search(rf"@mule\.fast\.compare(?:\.bool)?\.{opcode}\(", body)
            text_equality = domain == "text" and opcode in (0, 1)
            if text_equality:
                matched = re.search(
                    r"(?P<result>%[\w.]+) = (?:tail )?call i32 @mule_text_eq\(", body
                )
                assert matched, body
                condition = re.search(
                    rf"(?P<result>%[\w.]+) = icmp (?P<test>eq|ne) i32 "
                    rf"{re.escape(matched['result'])}, 0",
                    body,
                )
                assert condition, body
                # LLVM may invert the i1 test and exchange select arms. Follow
                # both values instead of fixing the spelling of the comparison.
                true_on_match = opcode == 0
                true_arm = true_on_match == (condition["test"] == "ne")
                first, second = ("true", "false") if true_arm else ("false", "true")
                assert re.search(
                    rf"select i1 {re.escape(condition['result'])}, "
                    rf"ptr @mule\.{first}, ptr @mule\.{second}\b",
                    body,
                ), body
            else:
                assert native or runtime or helper, body
            if helper:
                # Follow any uninlined compiler helper: an inverse symbol alone
                # must not conceal the original comparison plus a truth conversion.
                helper_body = case.function_ir(
                    rf"^mule\.fast\.compare(?:\.bool)?\.{opcode}$"
                )
                assert re.search(
                    rf"@mule_compare(?:_bool)?\(i32(?: [a-z_]+)* {opcode},", helper_body
                )
                assert "@mule_truth(" not in helper_body

    # Follow the selected runtime boundary and its physical text-comparison
    # callees, not just the compiler-generated inverse-opcode wrapper.
    _assert_runtime_comparison_has_no_extra_truth(case)


def _boundary_program():
    definitions = []
    for suffix, operator, *_ in _OPERATORS:
        definitions.append(
            f"def unknown_{suffix}(left, right):\n"
            f"    return not (left {operator} right)\n"
            f"def floating_{suffix}(row):\n"
            "    left = float(row[0])\n"
            "    right = float(row[1])\n"
            f"    return not (left {operator} right)\n"
            f"def separated_{suffix}(row):\n"
            "    left = int(row[0])\n"
            "    right = int(row[1])\n"
            f"    result = left {operator} right\n"
            "    print('between', left, right, result)\n"
            "    return not result\n"
        )
    methods = []
    for suffix, *_ in _OPERATORS:
        methods.append(
            f"    def __{suffix}__(self, other):\n"
            f"        print('{suffix}', self.value, other.value)\n"
            "        return Verdict(self.value)\n"
        )
    custom = (
        "class Verdict:\n"
        "    def __init__(self, value):\n"
        "        self.value = value\n"
        "    def __bool__(self):\n"
        "        print('truth', self.value)\n"
        "        if self.value == 9:\n"
        "            raise ValueError('verdict')\n"
        "        return self.value > 0\n"
        "    def __del__(self):\n"
        "        print('release verdict', self.value)\n"
        "class Record:\n"
        "    def __init__(self, value):\n"
        "        self.value = value\n" + "".join(methods)
    )
    consumers = [
        "row = input().split()\n",
        "left = Record(int(row[0]))\n",
        "right = Record(int(row[1]))\n",
    ]
    for suffix, *_ in _OPERATORS:
        consumers.extend(
            [
                (
                    f"print('float', '{suffix}', list(filter(floating_{suffix}, "
                    "[['nan', row[1]], [row[0], 'nan'], row])))\n"
                ),
                f"print('separated', '{suffix}', list(filter(separated_{suffix}, [row])))\n",
                (
                    "try:\n"
                    f"    print('custom', '{suffix}', unknown_{suffix}(left, right))\n"
                    "except ValueError:\n"
                    "    print('verdict error')\n"
                ),
            ]
        )
    return "\n".join(definitions + [custom] + consumers)


@pytest.fixture(scope="module")
def order_boundary_compilation():
    return compile_source(_boundary_program())


@pytest.mark.parametrize("family", ("unknown", "floating", "separated"))
@pytest.mark.parametrize(
    "suffix,operator,original,inverse,opcode,predicate", _OPERATORS
)
def test_order_inversion_preserves_unknown_float_and_nonadjacent_boundaries(
    order_boundary_compilation,
    family,
    suffix,
    operator,
    original,
    inverse,
    opcode,
    predicate,
):
    compilation = order_boundary_compilation
    function = next(
        fn for fn in compilation.lambdas if fn.source_name == f"{family}_{suffix}"
    )
    assert list(known_calls(function, f"%compare_{original}"))
    assert list(known_calls(function, "%unary_not"))
    assert list(known_calls(function, f"%compare_{inverse}")) == []
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_order_inversion_boundaries_preserve_nan_custom_truth_and_effect_order(
    tmp_path,
):
    case = build_case(tmp_path, _boundary_program(), optimize=3, lto=False)
    # Custom operators all return the same verdict. Using the inverse method
    # would therefore change both the trace and the boolean result.
    for row in ("0 2\n", "2 0\n", "9 2\n"):
        case.compare(stdin=row)
