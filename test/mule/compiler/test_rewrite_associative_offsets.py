"""Associative offsets in runtime-selected table lookups and record positions."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import constant_literal
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_OFFSETS = [
    ("(index + 3) + 5", "%add", 8),
    ("(index - 10) + 3", "%subtract", 7),
    ("(index + 3) - 10", "%subtract", 7),
    ("(index - 3) - 5", "%subtract", 8),
    ("(index + 10) - 3", "%add", 7),
    ("(index + 5) - 5", None, None),
    ("(index - 5) + 5", None, None),
    ("(((index + 1) + 2) - 9) + 14", "%add", 8),
]


def _lookup_source(expression, *, local_constants=False):
    constants = " first = 3\n second = 5\n" if local_constants else ""
    return (
        "def lookup(rows, token):\n"
        " index = ord(token)\n"
        f"{constants}"
        f" return rows[{expression}]\n"
    )


def _lookup_ir1(source):
    compilation = compile_source(source)
    check_ir1_consistency(compilation.components)
    return next(fn for fn in compilation.lambdas if fn.source_name == "lookup")


@pytest.mark.parametrize(("expression", "operation", "offset"), _OFFSETS)
def test_associative_table_lookup_uses_one_net_offset(expression, operation, offset):
    # ord supplies a normal inferred exact-integer interval, not injected facts.
    fn = _lookup_ir1(_lookup_source(expression))
    arithmetic = [
        call for name in ("%add", "%subtract") for call in known_calls(fn, name)
    ]
    if operation is None:
        assert arithmetic == []
    else:
        assert len(arithmetic) == 1
        assert arithmetic[0].fun_info.name == operation
        constant = constant_literal(arithmetic[0].args[1])
        assert isinstance(constant, ast.IntegerLiteral)
        assert constant.value == offset
    assert list(known_calls(fn, "%getitem")), "The offset must feed a real lookup"


def test_associative_table_lookup_uses_flow_proven_local_constants():
    fn = _lookup_ir1(_lookup_source("(index + first) + second", local_constants=True))
    arithmetic = list(known_calls(fn, "%add"))
    assert len(arithmetic) == 1
    constant = constant_literal(arithmetic[0].args[1])
    assert isinstance(constant, ast.IntegerLiteral)
    assert constant.value == 8


@pytest.mark.parametrize(
    "body",
    [
        " return rows[(index + 3) + 5]\n",
        # bool arithmetic returns int; cancellation must not return the bool.
        " flag = bool(index)\n return rows[(flag + 5) - 5]\n",
        # Reading the intermediate elsewhere prevents consuming its only producer.
        (
            " intermediate = index + 3\n print(intermediate)\n"
            " return rows[intermediate + 5]\n"
        ),
    ],
    ids=["unknown-operand", "boolean-operand", "observable-intermediate"],
)
def test_associative_table_lookup_keeps_noninteger_and_shared_intermediates(body):
    fn = _lookup_ir1("def lookup(rows, index):\n" + body)
    arithmetic = [
        call for name in ("%add", "%subtract") for call in known_calls(fn, name)
    ]
    assert len(arithmetic) == 2


@pytest.mark.parametrize("sign", [1, -1])
def test_associative_offsets_respect_combined_constant_fold_limit(sign):
    # Each literal fits the 128-bit fold budget; their sum does not.
    magnitude = (1 << 128) - 1
    operator = "+" if sign > 0 else "-"
    expression = f"(index {operator} {magnitude}) {operator} 1"
    fn = _lookup_ir1(_lookup_source(expression))
    operation = "%add" if sign > 0 else "%subtract"
    assert len(list(known_calls(fn, operation))) == 2


@pytest.mark.integration
def test_associative_offsets_execute_optimized_runtime_table_lookups(tmp_path):
    functions = "\n".join(
        _lookup_source(expression).replace("def lookup(", f"def lookup_{i}(")
        for i, (expression, _operation, _offset) in enumerate(_OFFSETS)
    )
    calls = "\n".join(
        f"try:\n print(lookup_{i}(rows, token))\nexcept IndexError:\n print('outside')"
        for i in range(len(_OFFSETS))
    )
    case = build_case(
        tmp_path,
        functions + "\ntoken = input()\nrows = input()\n" + calls + "\n",
    )
    for stdin in ("A\n" + "abcdefghijklmnopqrstuvwxyz" * 4 + "\n", "z\nshort\n"):
        case.compare(stdin)

    # Follow the real lookup bodies, not an arithmetic-only stand-in. The proven
    # ord interval makes the remaining offset infallible native arithmetic.
    for i in (0, 7):
        body = case.function_ir(rf"^mule_fn_lookup_{i}\.(?:body|fn|spec\..*)$")
        # The offset may operate on decoded integers or on their tagged bits.
        assert re.search(r"\badd(?: nuw| nsw)* i64 [^\n]+, (?:8|16)\b", body), body
        for fallback in (
            "@mule_binary_reuse",
            "@mule.fast.binary",
            "@mule.fixnum.binary",
        ):
            assert fallback not in body, body
        assert "with.overflow" not in body, body
    for i in (5, 6):
        body = case.function_ir(rf"^mule_fn_lookup_{i}\.(?:body|fn|spec\..*)$")
        assert "@mule_binary_reuse" not in body, body
        assert "with.overflow" not in body, body


@pytest.mark.integration
def test_associative_offsets_preserve_bigint_intermediates_and_aliases(tmp_path):
    source = """
def position(index):
    intermediate = index + 17
    result = intermediate - 17
    print(intermediate, result, index)
    preceding = index - 17
    print(preceding, preceding + 17)
    return result

def locate(rows, index):
    offset = (index + 17) - 17
    try:
        print(rows[offset])
    except IndexError:
        print('outside', offset)

index = int(input())
alias = index
print(position(index) == index, index is alias)
locate('records', index)
"""
    case = build_case(tmp_path, source)
    # Cross tagged and signed-64 limits in an intermediate, then cancel back.
    for value in (0, 3, (1 << 62) - 1, (1 << 63) - 1, -(1 << 63), 1 << 129):
        case.compare(f"{value}\n")


@pytest.mark.integration
def test_associative_offsets_preserve_boolean_identity_and_operator_effects(tmp_path):
    case = build_case(
        tmp_path,
        """
class Offset:
    def __init__(self, value):
        self.value = value
    def __add__(self, amount):
        print('add', self.value, amount)
        self.value += amount
        return self
    def __sub__(self, amount):
        print('subtract', self.value, amount)
        if self.value < amount:
            raise ValueError('offset')
        return self.value - amount

def locate(rows, index):
    result = (index + 5) - 5
    print(result, result is True, result is False)
    return rows[result]

text = input()
print(locate('abcdef', bool(text)))
index = Offset(int(input()))
alias = index
try:
    print(locate('abcdef', index))
except ValueError:
    print('offset-error')
except IndexError:
    print('outside')
print(alias.value, index is alias)
try:
    locate('abcdef', 0.5)
except TypeError:
    print('nonintegral-offset')
print(locate('abcdef', 0))
""",
    )
    for stdin in ("yes\n2\n", "\n-1\n", "yes\n9\n"):
        case.compare(stdin)
