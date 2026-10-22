"""Membership proofs from ordinary flow facts, including late singleton facts."""

import re
from textwrap import dedent

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal, ir1_optimize_until_done
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source, compile_toplevel
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("operator, expected", [("in", False), ("not in", True)])
@pytest.mark.parametrize(
    "setup, elements",
    [
        ("value = len(raw) & 7", "-2, -1, 8, 9"),
        ("value = len(raw) & 7", "None, 'missing', b'missing'"),
        ("value = 'open' if raw else 'closed'", "0, False, None, b'open'"),
        ("value = b'open' if raw else b'closed'", "0, False, None, 'open'"),
    ],
)
@pytest.mark.parametrize("display", ["({elements})", "[{elements}]"])
def test_disjoint_flow_facts_fold_membership(
    setup, elements, display, operator, expected
):
    source = (
        f"def classify(raw):\n {setup}\n"
        f" return value {operator} {display.format(elements=elements)}\n"
    )
    compilation = compile_source(source)
    function = next(fn for fn in compilation.lambdas if fn.source_name == "classify")
    assert not list(known_calls(function, "%compare_in"))
    assert not list(known_calls(function, "%compare_not_in"))
    assert not list(known_calls(function, "%build_list"))
    assert not list(known_calls(function, "%build_tuple"))
    literal = constant_literal(function.return_.result)
    assert isinstance(literal, ast.BooleanLiteral)
    assert literal.value is expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "body",
    [
        # Both interval endpoints belong to [0, 7], not its disjoint complement.
        "value = len(raw) & 7\nreturn value in (0, 8)",
        "value = len(raw) & 7\nreturn value in (-1, 7)",
        "value = len(raw) & 7\nreturn value not in (-1, 3, 8)",
        # bool and int equality overlaps at zero and one.
        "value = len(raw) & 1\nreturn value in (False, True)",
        "value = 'open' if raw else None\nreturn value in (None, 0)",
        "if raw:\n value = len(raw) & 7\nreturn value in (-1, 8)",
        "value = float(raw)\nreturn value in (0, 1)",
        "return raw in (0, 1)",
    ],
)
def test_overlapping_nullable_unbound_and_unsupported_facts_keep_membership(body):
    source = "def classify(raw):\n " + body.replace("\n", "\n ") + "\n"
    compilation = compile_source(source)
    function = next(fn for fn in compilation.lambdas if fn.source_name == "classify")
    calls = list(known_calls(function, "%compare_in")) + list(
        known_calls(function, "%compare_not_in")
    )
    assert calls, body
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("display", ["(value,)", "[value]"])
@pytest.mark.parametrize(
    "operator, comparison", [("in", "%compare_equal"), ("not in", "%compare_not_equal")]
)
def test_late_singleton_element_fact_revisits_unchanged_container(
    display, operator, comparison
):
    source = (
        "def classify(raw, flag):\n"
        " value = 'open' if flag else 'closed'\n"
        f" return raw {operator} {display}\n"
    )
    # Before flow publication, neither operand has a reflexive primitive fact.
    # The initial IR1 fixed point cannot remove the singleton construction.
    initial = compile_toplevel(source)
    function = next(fn for fn in initial.lambdas if fn.source_name == "classify")
    ir1_optimize_until_done(function.component)
    assert list(known_calls(function, f"%compare_{operator.replace(' ', '_')}"))
    assert list(known_calls(function, "%build_tuple")) or list(
        known_calls(function, "%build_list")
    )
    check_ir1_consistency(initial.components)

    # Normal scheduling must revisit containment after the element's flow fact
    # changes, even though the container-producing node itself is unchanged.
    compilation = compile_source(source)
    function = next(fn for fn in compilation.lambdas if fn.source_name == "classify")
    assert not list(known_calls(function, "%compare_in"))
    assert not list(known_calls(function, "%compare_not_in"))
    assert not list(known_calls(function, "%build_tuple"))
    assert not list(known_calls(function, "%build_list"))
    assert list(known_calls(function, comparison))
    check_ir1_consistency(compilation.components)


CLASSIFIER = dedent("""\
    def disjoint_interval(raw):
        code = len(raw) & 7
        if code in (-2, -1, 8, 9):
            return code + 100
        return code + 200

    def disjoint_text(raw):
        label = 'open' if raw else 'closed'
        if label in (0, False, None, b'open'):
            return len(raw) + 100
        return len(raw) + 200

    def disjoint_bytes(raw):
        label = b'open' if raw else b'closed'
        if label not in (0, False, None, 'open'):
            return len(raw) + 200
        return len(raw) + 100

    def late_singleton(raw, flag):
        label = 'open' if flag else 'closed'
        if raw in [label]:
            return len(raw) + 100
        return len(raw) + 200

    raw = input()
    print(disjoint_interval(raw))
    print(disjoint_text(raw))
    print(disjoint_bytes(raw))
    print(late_singleton(raw, len(raw) & 1))
""")


@pytest.mark.integration
def test_runtime_record_classifier_eliminates_disjoint_and_singleton_searches(tmp_path):
    case = build_case(tmp_path, CLASSIFIER)
    for raw in ("", "a", "open", "closed", "abcdefg", "abcdefgh", "πππ"):
        case.compare(raw + "\n")

    for name in (
        "disjoint_interval",
        "disjoint_text",
        "disjoint_bytes",
        "late_singleton",
    ):
        body = case.function_ir(rf"mule_fn_{name}\.body$")
        # The compiled contains/not_contains algorithms must disappear, not
        # merely move from main into another membership wrapper.
        assert "contains" not in body
        assert "mule_tuple" not in body
        assert "mule_list" not in body
        assert "mule_storage_new" not in body
        assert "mule_resolve_method" not in body
    interval = case.function_ir(r"mule_fn_disjoint_interval\.body$")
    assert re.search(r"\band i64\b", interval)
    assert re.search(r"\badd i64\b", interval)
    assert "mule_compare" not in interval
    # Equality is still necessary for the runtime-selected singleton element.
    singleton = case.function_ir(r"mule_fn_late_singleton\.body$")
    assert "mule_text_eq" in singleton or "mule_compare" in singleton


BOUNDARIES = dedent("""\
    events = []

    class Probe:
        def __eq__(self, other):
            events.append(other)
            return other == 8

    def overlap(raw):
        code = int(raw) & 7
        return code in (0, 8), code in (-1, 7), code not in (-1, 3, 8)

    def nullable(raw):
        value = None if raw else 'open'
        return value in (None, 0)

    def unbound(raw):
        if raw:
            value = len(raw) & 7
        return value in (-1, 8)

    def unsupported(raw):
        value = float(raw)
        return value in (0, 1), value not in (0, 1)

    def effect(raw):
        events.append(raw)
        return len(raw) & 7

    def ordered(raw):
        return effect(raw) in (effect('x'), effect('yy'))

    raw = input()
    print(overlap(raw))
    print(nullable(raw))
    print(nullable(''))
    print(unsupported(raw))
    print(unbound(raw))
    try:
        print(unbound(''))
    except UnboundLocalError:
        print('unbound')
    print(Probe() in (7, 8))
    print(events)
    events.clear()
    print(ordered(raw))
    print(events)
    try:
        print(overlap('invalid'))
    except ValueError:
        print('invalid')
""")


@pytest.mark.integration
def test_membership_fallbacks_preserve_endpoints_errors_and_effect_order(tmp_path):
    case = build_case(tmp_path, BOUNDARIES)
    for raw in ("0", "1", "3", "7", "8", "-1"):
        case.compare(raw + "\n")
    for name in ("overlap", "nullable", "unbound", "unsupported", "ordered"):
        body = case.function_ir(rf"mule_fn_{name}\.body$")
        assert "contains" in body, name
