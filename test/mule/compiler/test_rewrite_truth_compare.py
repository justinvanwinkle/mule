"""Truth-only rich comparisons keep custom-result effects and omit a second dispatch."""

import re

import pytest

from mule.compiler.vop import BOOLEAN_T
from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_borrowed_lifetimes import component_for

OPERATORS = (
    ("eq", "==", "__eq__"),
    ("ne", "!=", "__ne__"),
    ("lt", "<", "__lt__"),
    ("le", "<=", "__le__"),
    ("gt", ">", "__gt__"),
    ("ge", ">=", "__ge__"),
)

CONSUMERS = (
    ("branch", "if left {op} right:\n    return marker\nreturn None", True),
    ("not", "return not (left {op} right)", True),
    ("raw", "return left {op} right", False),
    ("and", "return (left {op} right) and marker", False),
    ("or", "return (left {op} right) or marker", False),
    ("escape", "return [left {op} right]", False),
    (
        "saved",
        "result = left {op} right\nif result:\n    return result\nreturn result",
        False,
    ),
)


def _callback_source(name, body, operator):
    statements = body.format(op=operator).splitlines()
    return f"def {name}(left, right, marker):\n" + "".join(
        f"    {statement}\n" for statement in statements
    )


@pytest.mark.parametrize(
    "label, operator, method", OPERATORS, ids=[label for label, _, _ in OPERATORS]
)
@pytest.mark.parametrize(
    "consumer, body, marked", CONSUMERS, ids=[consumer for consumer, _, _ in CONSUMERS]
)
def test_rich_comparison_marks_only_truth_consumers_from_source(
    label, operator, method, consumer, body, marked
):
    source = _callback_source("check", body, operator)
    component = component_for(source)
    comparisons = [
        vop
        for block in component.blocks
        for vop in block.vops
        if vop.info.name.startswith("compare-")
    ]
    assert len(comparisons) == 1, (label, consumer, comparisons)
    comparison = comparisons[0]
    assert (comparison.codegen_info is True) is marked
    if marked:
        assert comparison.results[0].tn.primitive_type is BOOLEAN_T


def _comparison_callbacks_source():
    # First-class predicate callbacks exercise their general object entries.
    # The decision arrives from stdin, so neither comparison nor truth can fold.
    source = """
class Decision:
    def __init__(self, mode):
        self.mode = mode

    def __bool__(self):
        print("truth", self.mode)
        if self.mode == "raise":
            raise ValueError("decision conversion")
        if self.mode == "invalid":
            return 1
        return self.mode == "yes"


class Ranked:
    def __init__(self, decision):
        self.decision = decision

"""
    for label, _operator, method in OPERATORS:
        source += f"""    def {method}(self, other):
        print("compare", "{label}")
        if self.decision.mode == "compare-error":
            raise LookupError("comparison")
        return self.decision

"""
    callbacks = []
    for label, operator, _method in OPERATORS:
        for consumer, body, _marked in CONSUMERS:
            name = f"compare_{consumer}_{label}"
            source += "\n" + _callback_source(name, body, operator)
            callbacks.append(f'    ("{consumer}", "{label}", {name}),\n')
    source += """
mode = input()
decision = Decision(mode)
left = Ranked(decision)
right = Ranked(decision)
marker = []
for consumer, operator, callback in (
"""
    source += "".join(callbacks)
    source += """
):
    print("callback", consumer, operator)
    try:
        result = callback(left, right, marker)
        if consumer == "escape":
            result = result[0]
        print(result is decision, result is marker, result is True, result is False)
    except Exception as error:
        print(type(error).__name__)
    print("alias", left.decision is decision, right.decision is decision)
"""
    return source


@pytest.fixture(scope="module")
def comparison_callbacks(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("truth-comparison-callbacks"),
        _comparison_callbacks_source(),
        optimize=3,
        lto=False,
    )


@pytest.mark.integration
@pytest.mark.parametrize("mode", ["yes", "no", "raise", "invalid", "compare-error"])
def test_rich_comparison_callbacks_match_python_effects_errors_and_aliases(
    comparison_callbacks, mode
):
    # The trace distinguishes one truth conversion from zero or two conversions.
    # Raw and escaping results must retain the actual Decision, not a Boolean.
    comparison_callbacks.compare(mode + "\n")


@pytest.mark.integration
def test_truth_only_callbacks_remove_the_caller_truth_dispatch(comparison_callbacks):
    for label, _operator, _method in OPERATORS:
        for consumer in ("branch", "not"):
            body = comparison_callbacks.function_ir(
                rf"^mule_fn_compare_{consumer}_{label}(?:\.|$)"
            )
            assert re.search(r"\bcall\b[^\n]*@mule_compare_bool\(", body), body
            assert not re.search(r"\bcall\b[^\n]*@mule_compare\(", body), body
            assert not re.search(r"\bcall\b[^\n]*@mule_truth\(", body), body
            assert not re.search(r"\bcall\b[^\n]*@mule_unary\(", body), body
            assert "@mule.true" in body, body


@pytest.mark.integration
def test_value_and_mixed_callbacks_keep_custom_comparison_results(comparison_callbacks):
    for label, _operator, _method in OPERATORS:
        for consumer in ("raw", "and", "or", "escape", "saved"):
            body = comparison_callbacks.function_ir(
                rf"^mule_fn_compare_{consumer}_{label}(?:\.|$)"
            )
            assert re.search(r"\bcall\b[^\n]*@mule_compare\(", body), body
            assert not re.search(r"\bcall\b[^\n]*@mule_compare_bool\(", body), body
            if consumer in ("and", "or", "saved"):
                assert re.search(r"\bcall\b[^\n]*@mule_truth\(", body), body


@pytest.mark.integration
def test_boolean_comparison_runtime_coercion_does_real_truth_and_cleanup(
    comparison_callbacks,
):
    runtime = comparison_callbacks.function_assembly(r"^mule_compare_bool$")
    coercion = comparison_callbacks.function_assembly(r"coerce_compare_bool")
    assert re.search(r"\b(?:call|jmp)\w*\s+[^\n]*<[^>]*coerce_compare_bool", runtime)
    assert re.search(r"\bcall\w*\s+[^\n]*<[^>]*truth", coercion), coercion
    # Releasing the original custom result must remain after its conversion.
    # Rust may inline release's atomic reference operation instead of calling it.
    assert re.search(r"<[^>]*release|\block\s", coercion), coercion
    assert not re.search(
        r"\b(?:call|jmp)\w*\s+[^\n]*<(?:[^>]*__rust_alloc|malloc|calloc|mule_box_)",
        coercion,
    ), coercion
