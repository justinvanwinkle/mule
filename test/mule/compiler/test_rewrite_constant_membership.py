"""Bounded primitive membership through ordinary flow analysis and consumers."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

PRIMITIVE_CASES = (
    ("1", "(True, 2, None)", True),
    ("True", "[0, 1, b'word']", True),
    ("False", "(0, 'word', None)", True),
    ("'word'", "[b'word', 1, None]", False),
    ("b'word'", "('word', False, None)", False),
    ("'λ'", "('word', 'λ', b'word')", True),
    ("None", "(False, 0, None)", True),
    ("None", "[False, 0, 'None']", False),
)


@pytest.mark.parametrize("left,container,expected", PRIMITIVE_CASES)
@pytest.mark.parametrize("operator", ("in", "not in"))
@pytest.mark.parametrize("via_alias", (False, True), ids=("literal", "local-alias"))
def test_primitive_membership_normal_flow_returns_folded_boolean(
    left, container, expected, operator, via_alias
):
    setup = f" category = {left}\n" if via_alias else ""
    operand = "category" if via_alias else left
    compilation = compile_source(
        f"def classify():\n{setup} return {operand} {operator} {container}\n"
    )
    function = compilation.lambdas[1]
    for operation in ("%compare_in", "%compare_not_in", "%build_list", "%build_tuple"):
        assert not list(known_calls(function, operation))
    result = constant_literal(function.return_.result)
    assert isinstance(result, ast.BooleanLiteral)
    assert result.value is (expected if operator == "in" else not expected)
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("size", (32, 33))
@pytest.mark.parametrize("brackets", (("(", ")"), ("[", "]")))
@pytest.mark.parametrize("operator", ("in", "not in"))
def test_membership_size_bound_applies_to_literal_tuple_and_list(
    size, brackets, operator
):
    opening, closing = brackets
    container = opening + ", ".join(map(str, range(size))) + closing
    # The local read prevents the core all-literal folder from deciding this
    # expression first. This cutoff belongs to the bounded containment transform.
    compilation = compile_source(
        f"def classify():\n category = 0\n return category {operator} {container}\n"
    )
    function = compilation.lambdas[1]
    calls = list(known_calls(function, "%compare_" + operator.replace(" ", "_")))
    if size == 32:
        assert not calls
        result = constant_literal(function.return_.result)
        assert isinstance(result, ast.BooleanLiteral)
        assert result.value is (operator == "in")
    else:
        assert calls, "The bounded transform must decline a 33-element container"
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "expression",
    (
        "1 in (1, 2.0)",
        "1 in [1.0, 2]",
        "'word' not in ('other', (1, 2))",
        "None in [None, (1,)]",
    ),
)
def test_membership_declines_entire_mixed_unsupported_container(expression):
    # A preceding primitive match must not conceal a later unsupported element.
    compilation = compile_source(f"def classify():\n return {expression}\n")
    function = compilation.lambdas[1]
    assert list(known_calls(function, "%compare_in")) or list(
        known_calls(function, "%compare_not_in")
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "setup,expression,expected",
    (
        ("category = value & 7", "category in (8, 9, None, b'bad')", False),
        ("category = value & 7", "category not in [-1, 8, 'bad']", True),
        ("category = f'{value}'", "category in [False, 0, b'bad', None]", False),
    ),
)
def test_membership_uses_normal_interval_and_primitive_type_facts(
    setup, expression, expected
):
    compilation = compile_source(
        f"def classify(value: int):\n {setup}\n return {expression}\n"
    )
    function = compilation.lambdas[1]
    assert not list(known_calls(function, "%compare_in"))
    assert not list(known_calls(function, "%compare_not_in"))
    result = constant_literal(function.return_.result)
    assert isinstance(result, ast.BooleanLiteral) and result.value is expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "source",
    (
        "def classify(flag):\n if flag:\n  category = 'word'\n return category in [1, None]\n",
        "def classify():\n return unknown_category not in (1, None)\n",
        "def classify(producer):\n return producer() in [1, None]\n",
        "def classify(category):\n del category\n return category in (1, None)\n",
    ),
)
def test_membership_does_not_discard_raising_or_effectful_left_operand(source):
    compilation = compile_source(source)
    function = compilation.lambdas[1]
    assert list(known_calls(function, "%compare_in")) or list(
        known_calls(function, "%compare_not_in")
    )
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_folded_membership_controls_live_runtime_token_consumers(tmp_path):
    functions = []
    calls = []
    for index, (left, container, _expected) in enumerate(PRIMITIVE_CASES):
        functions.append(
            f"def token_consumer_{index}(token):\n"
            f" category = {left}\n"
            f" accepted = category in {container}\n"
            " if accepted:\n"
            "  return token\n"
            " return token + '!'\n"
        )
        calls.append(f"print(token_consumer_{index}(token))\n")
    source = "\n".join(functions) + "token = input()\n" + "".join(calls)
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for token in ("", "identifier", "λ-value"):
        result = case.compare(stdin=token + "\n")
        assert result.stdout.splitlines() == [
            token if expected else token + "!"
            for _left, _container, expected in PRIMITIVE_CASES
        ]

    for index, (_left, _container, expected) in enumerate(PRIMITIVE_CASES):
        # The external entry wrapper invokes the compiled argument binder.
        # Inspect executable bodies, including any specialized bodies, instead.
        body = case.function_ir(rf"^mule_fn_token_consumer_{index}\.(?:body|spec\.)")
        assert not re.search(r"\bcall\b[^\n]*@[^\s(]*(?:compare|contains)", body), (
            f"token_consumer_{index} retains comparison or containment work"
        )
        assert not re.search(r"\bcall\b[^\n]*@mule_sequence_(?:tuple|list)", body), (
            f"token_consumer_{index} retains its membership container"
        )
        assert "ret ptr" in body
        if not expected:
            # The rejected-token consumer still performs its runtime concatenation.
            assert re.search(r"\bcall\b[^\n]*@mule_binary_reuse\([^,\n]*\b0,", body)

    compilation = compile_source(source)
    consumers = [
        fn
        for fn in compilation.lambdas
        if fn.source_name and fn.source_name.startswith("token_consumer_")
    ]
    assert consumers
    for function in consumers:
        assert not list(known_calls(function, "%compare_in"))
        assert not list(known_calls(function, "%build_list"))
        assert not list(known_calls(function, "%build_tuple"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_membership_fallback_preserves_errors_identity_and_evaluation_order(tmp_path):
    source = """
class Candidate:
    def __init__(self, label):
        self.label = label
        print('construct', label)
    def __eq__(self, other):
        print('equal', self.label, other)
        raise ValueError('candidate')

def missing_local(flag):
    if flag:
        category = 'word'
    return category in [1, None]

def left_effect(token):
    print('left', token)
    return 1

token = input()
print(left_effect(token) in (1, Candidate('after-match')))
try:
    print(1 in (Candidate('before-match'), 1))
except ValueError:
    print('candidate error')
try:
    print(missing_local(False))
except UnboundLocalError:
    print('unbound error')
try:
    print(unknown_category in [1, None])
except NameError:
    print('global error')
item = Candidate(token)
print(item in (item, 1))
print(1 in (1.0, 2))
print(None in [None, (1,)])
nan = float('nan')
print(nan in (nan, 1))
print(nan in (float('nan'), 1))
"""
    for size in (32, 33):
        for opening, closing in (("(", ")"), ("[", "]")):
            container = opening + ", ".join(map(str, range(size))) + closing
            source += f"print(token, 0 in {container}, 99 not in {container})\n"
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare(stdin="runtime-token\n")
