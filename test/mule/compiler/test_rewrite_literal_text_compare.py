"""Literal-state classifiers through inferred IR1 facts and optimized code."""

import re
from textwrap import dedent

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source, compile_toplevel
from mule.compiler.node import If
from test.mule.compiler.rewrite_support import build_case

OPERATORS = (
    ("==", "%compare_equal"),
    ("!=", "%compare_not_equal"),
    ("<", "%compare_less"),
    ("<=", "%compare_less_equal"),
    (">", "%compare_greater"),
    (">=", "%compare_greater_equal"),
)
TEXT_PAIRS = (("ready", "ready"), ("ready", "stopped"), ("", ""), ("é", "😀"))


def _classifier(name, operator, left, right):
    # The record is runtime input. Only its fixed parser-state decision folds.
    return dedent(
        f"""\
        def {name}(record):
            state = {left!r}
            if state {operator} {right!r}:
                return ("accept", record)
            return ("reject", record)
        """
    )


@pytest.mark.parametrize(("operator", "known"), OPERATORS)
@pytest.mark.parametrize(("left", "right"), TEXT_PAIRS)
def test_literal_state_comparison_folds_from_normal_source_facts(
    operator, known, left, right
):
    source = _classifier("classify", operator, left, right)
    before = compile_toplevel(source)
    assert list(known_calls(before.lambdas[1], known))

    after = compile_source(source)
    classifier = after.lambdas[1]
    assert not list(known_calls(classifier, known))
    # Dead classifier arms must disappear as well as the comparison itself.
    assert classifier.component is not None
    assert not any(
        isinstance(node, If)
        for block in classifier.component.blocks()
        for node in block.nodes()
    )
    check_ir1_consistency(after.components)


@pytest.mark.parametrize(("operator", "known"), OPERATORS)
def test_dynamic_token_comparison_retains_normal_matcher_fallback(operator, known):
    compilation = compile_source(
        "def classify(raw):\n"
        "    token = str(raw)\n"
        f"    return token {operator} 'ready'\n"
    )
    assert list(known_calls(compilation.lambdas[1], known))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_literal_state_classifiers_fold_in_optimized_runtime_record_processing(
    tmp_path,
):
    classifiers = [
        _classifier(f"classify_{index}_{pair}", operator, left, right)
        for index, (operator, _known) in enumerate(OPERATORS)
        for pair, (left, right) in enumerate(TEXT_PAIRS)
    ]
    calls = [
        f"label, retained = classify_{index}_{pair}(record)\n"
        "print(label, retained, retained is record)\n"
        for index in range(len(OPERATORS))
        for pair in range(len(TEXT_PAIRS))
    ]
    source = "\n".join(classifiers) + "\nrecord = input()\n" + "\n".join(calls)
    case = build_case(tmp_path, source)
    for record in ("ready", "", "record:é😀\x00tail"):
        case.compare(record + "\n")

    # Every selected body still consumes a runtime record. Its fixed decision
    # requires neither a runtime comparison nor a truth conversion. This also
    # rejects moving the comparison into Mule's comparison helper family.
    for index in range(len(OPERATORS)):
        for pair in range(len(TEXT_PAIRS)):
            body = case.function_ir(rf"classify_{index}_{pair}(?:[.$_]|$)")
            assert not re.search(
                r"\bcall\b[^\n]*@[^\n(]*(?:compare|text_eq|truth)", body
            ), body


@pytest.mark.integration
def test_dynamic_token_comparisons_preserve_effects_mutation_and_errors(tmp_path):
    expressions = ", ".join(
        f"token {operator} 'ready'" for operator, _known in OPERATORS
    )
    effect_expressions = "\n".join(
        f"    print(produce('left', raw, fail) {operator} "
        "produce('right', 'ready', fail))"
        for operator, _known in OPERATORS
    )
    source = dedent(
        f"""\
        def classify(token):
            return ({expressions})

        def produce(label, value, fail):
            print(label)
            if label == fail:
                raise ValueError(label)
            return value

        def effects(raw, fail):
        EFFECT_EXPRESSIONS

        def maybe_unbound(enabled):
            if enabled:
                state = 'ready'
            return state == 'ready'

        def mutate(raw):
            state = 'ready'
            def update():
                nonlocal state
                state = raw
            update()
            return state == 'ready', state < 'ready'

        raw = input()
        print(classify(raw))
        print(mutate(raw))
        effects(raw, 'never')
        try:
            effects(raw, 'left')
        except ValueError:
            print('left-error')
        try:
            effects(raw, 'right')
        except ValueError:
            print('right-error')
        try:
            print(maybe_unbound(False))
        except UnboundLocalError:
            print('unbound-error')
        print(maybe_unbound(True))
        try:
            print(classify(None))
        except TypeError:
            print('ordering-error')
        """
    ).replace("EFFECT_EXPRESSIONS", effect_expressions)
    case = build_case(tmp_path, source)
    for token in ("ready", "", "stopped", "é", "😀", "ready\x00tail"):
        case.compare(token + "\n")
