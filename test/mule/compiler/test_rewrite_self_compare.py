"""Reflexive value comparisons through normal record validation and filtering."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_COMPARISONS = (
    ("==", "%compare_equal"),
    ("!=", "%compare_not_equal"),
    ("<", "%compare_less"),
    ("<=", "%compare_less_equal"),
    (">", "%compare_greater"),
    (">=", "%compare_greater_equal"),
)
_NATURAL_VALUES = (
    pytest.param("len(raw)", id="integer-length"),
    pytest.param("bool(raw)", id="boolean-validation"),
    pytest.param("'accepted' if raw else 'empty'", id="text-label"),
    pytest.param("b'accepted' if raw else b'empty'", id="bytes-label"),
)


def _validation_source(value):
    checks = ", ".join(f"x {operator} x" for operator, _ in _COMPARISONS)
    return (
        "def validate_record(raw):\n"
        f"    x = {value}\n"
        f"    return ({checks}, x)\n"
        "for index in range(2):\n"
        "    row = input()\n"
        "    result = validate_record(row)\n"
        "    print(result)\n"
        "    if result[0] and result[3] and result[5]:\n"
        "        print('accepted', row)\n"
    )


@pytest.mark.parametrize("value", _NATURAL_VALUES)
def test_natural_reflexive_facts_remove_ir1_comparisons(value):
    compilation = compile_source(_validation_source(value))
    validator = next(
        fn for fn in compilation.lambdas if fn.source_name == "validate_record"
    )
    for _, operation in _COMPARISONS:
        assert not list(known_calls(validator, operation)), operation
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize("value", _NATURAL_VALUES)
def test_reflexive_record_validation_has_no_runtime_comparison(tmp_path, value):
    case = build_case(tmp_path, _validation_source(value))
    case.compare("\nrecord-π\n")
    case.compare("another\n\n")
    # Inspect every validator entry, not a selected wrapper or unrelated helper.
    # len/bool and label selection remain runtime-dependent. Only the six
    # comparisons disappear; LLVM may still compare tags or branch conditions.
    body = case.function_ir(r"^mule_fn_validate_record\.(?:body|spec\.)")
    assert not re.search(r"@(?:mule_compare\w*|mule\.fast\.compare[^\s(]*)", body)
    assert "ret " in body


@pytest.mark.integration
def test_float_filter_keeps_nan_comparison_semantics(tmp_path):
    checks = ", ".join(f"x {operator} x" for operator, _ in _COMPARISONS)
    source = (
        "def validate_float(raw):\n"
        "    x = float(raw)\n"
        f"    return ({checks})\n"
        "for index in range(4):\n"
        "    raw = input()\n"
        "    result = validate_float(raw)\n"
        "    print(raw, result)\n"
        "    if result[0]:\n"
        "        print('reflexive', raw)\n"
    )
    case = build_case(tmp_path, source)
    case.compare("nan\n0.0\n-0.0\n1.25\n")
    body = case.function_ir(r"^mule_fn_validate_float\.(?:body|spec\.)")
    # A floating comparison or its dynamic fallback must still examine x.
    assert re.search(r"\bfcmp\b|@mule_compare|@mule\.fast\.compare", body)


@pytest.mark.integration
def test_unknown_self_comparisons_preserve_custom_results_and_mutation(tmp_path):
    checks = ", ".join(f"x {operator} x" for operator, _ in _COMPARISONS)
    methods = ""
    for method in ("eq", "ne", "lt", "le", "gt", "ge"):
        methods += (
            f"    def __{method}__(self, other):\n"
            "        self.calls += 1\n"
            f"        print('{method}', self.calls, self is other)\n"
            "        return self.label\n"
        )
    source = (
        "class Record:\n"
        "    def __init__(self, label):\n"
        "        self.label = label\n"
        "        self.calls = 0\n" + methods + "def compare_record(x):\n"
        f"    return ({checks})\n"
        "for index in range(2):\n"
        "    record = Record(input())\n"
        "    result = compare_record(record)\n"
        "    calls = record.calls\n"
        "    del record\n"
        "    print(result, calls)\n"
        "    if result[0]:\n"
        "        print('accepted', result[0])\n"
    )
    compilation = compile_source(source)
    comparator = next(
        fn for fn in compilation.lambdas if fn.source_name == "compare_record"
    )
    for _, operation in _COMPARISONS:
        assert list(known_calls(comparator, operation)), operation
    case = build_case(tmp_path, source)
    case.compare("changed\n\n")


@pytest.mark.integration
def test_operand_reassignment_is_not_a_reflexive_comparison(tmp_path):
    source = ""
    for index, (operator, _) in enumerate(_COMPARISONS):
        source += (
            f"def replace_{index}(raw):\n"
            "    x = len(raw)\n"
            f"    verdict = x {operator} (x := x + 1)\n"
            "    return verdict, x\n"
        )
    source += "raw = input()\n"
    for index, _ in enumerate(_COMPARISONS):
        source += f"print(replace_{index}(raw))\n"
    case = build_case(tmp_path, source)
    case.compare("mutable record\n")
    case.compare("\n")


@pytest.mark.integration
def test_reflexive_comparisons_do_not_hide_unbound_local_errors(tmp_path):
    source = ""
    for index, (operator, _) in enumerate(_COMPARISONS):
        source += (
            f"def conditional_{index}(raw):\n"
            "    if raw:\n"
            "        x = len(raw)\n"
            f"    return x {operator} x\n"
            f"def deleted_{index}(raw):\n"
            "    x = len(raw)\n"
            "    del x\n"
            f"    return x {operator} x\n"
        )
    source += "raw = input()\n"
    for index, _ in enumerate(_COMPARISONS):
        for function in (f"conditional_{index}", f"deleted_{index}"):
            source += (
                "try:\n"
                f"    print('{function}', {function}(raw))\n"
                "except UnboundLocalError:\n"
                f"    print('{function}', 'unbound')\n"
            )
    case = build_case(tmp_path, source)
    case.compare("bound\n")
    case.compare("\n")
