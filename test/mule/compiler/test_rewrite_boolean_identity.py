"""Boolean identities inferred from runtime predicates, not injected IR facts."""

import re

import pytest

from mule.compiler.ctype import BOOLEAN
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1opt import lvar_type
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

IDENTITIES = (
    ("or_right", "flag | False", "%bit_or"),
    ("or_left", "False | flag", "%bit_or"),
    ("xor_right", "flag ^ False", "%bit_xor"),
    ("xor_left", "False ^ flag", "%bit_xor"),
    ("and_right", "flag & True", "%bit_and"),
    ("and_left", "True & flag", "%bit_and"),
)


def _predicate_source(name, expression):
    return f"def {name}(raw):\n    flag = bool(raw)\n    return {expression}\n"


@pytest.mark.parametrize(("name", "expression", "operation"), IDENTITIES)
def test_boolean_identity_infers_predicate_and_result_type(name, expression, operation):
    compilation = compile_source(_predicate_source(name, expression))
    predicate = next(fn for fn in compilation.lambdas if fn.source_name == name)
    flag = next(local for local in predicate.locals if local.name == "flag")
    assert flag.sets, "The runtime predicate must still initialize its local."
    for assignment in flag.sets:
        assert assignment.value is not None
        assert lvar_type(assignment.value).fits(BOOLEAN)
    assert not list(known_calls(predicate, operation))
    assert predicate.return_ is not None
    assert lvar_type(predicate.return_.result).fits(BOOLEAN)
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("expression", "operation"),
    (
        ("flag | 0", "%bit_or"),
        ("0 | flag", "%bit_or"),
        ("flag ^ 0", "%bit_xor"),
        ("0 ^ flag", "%bit_xor"),
        ("flag & 1", "%bit_and"),
        ("1 & flag", "%bit_and"),
    ),
)
def test_boolean_integer_mix_does_not_return_the_boolean_operand(expression, operation):
    compilation = compile_source(_predicate_source("mixed", expression))
    predicate = next(fn for fn in compilation.lambdas if fn.source_name == "mixed")
    assert list(known_calls(predicate, operation))
    check_ir1_consistency(compilation.components)


_PREDICATES = "\n".join(
    _predicate_source(name, expression) for name, expression, _operation in IDENTITIES
)

_PROGRAM = (
    _PREDICATES
    + """
def aggregate(raw):
    return (or_right(raw), or_left(raw), xor_right(raw), xor_left(raw),
            and_right(raw), and_left(raw))

def show(raw):
    results = aggregate(raw)
    for result in results:
        print(result, type(result) is bool, result is True, result is False)

class Predicate:
    def __init__(self, value):
        self.value = value
        self.visits = 0

    def __bool__(self):
        self.visits += 1
        print("predicate", self.visits, self.value)
        if self.value == 9:
            raise ValueError("predicate failed")
        return self.value != 0

class InvalidPredicate:
    def __bool__(self):
        print("invalid predicate")
        return 1

class Bits:
    def __or__(self, other):
        print("or", other)
        return self

    def __ror__(self, other):
        print("ror", other)
        return self

    def __xor__(self, other):
        print("xor", other)
        return self

    def __rxor__(self, other):
        print("rxor", other)
        return self

    def __and__(self, other):
        print("and", other)
        return self

    def __rand__(self, other):
        print("rand", other)
        return self

def dynamic_bits(value):
    return (value | False, False | value, value ^ False, False ^ value,
            value & True, True & value)

def integer_literals(raw):
    flag = bool(raw)
    return (flag | 0, 0 | flag, flag ^ 0, 0 ^ flag, flag & 1, 1 & flag)

value = int(input())
for item in (0, value, -value):
    show(item)
    for result in integer_literals(item):
        print("integer", result, type(result) is int)
    for result in dynamic_bits(item):
        print("dynamic integer", result, type(result) is int)

predicate = Predicate(value)
show(predicate)
print("visits", predicate.visits)
for item in (Predicate(9), InvalidPredicate()):
    try:
        show(item)
    except ValueError:
        print("caught value error")
    except TypeError:
        print("caught type error")

bits = Bits()
for result in dynamic_bits(bits):
    print("same object", result is bits)
"""
)


@pytest.mark.integration
def test_boolean_predicate_aggregation_preserves_effects_and_eliminates_bitwise_work(
    tmp_path,
):
    case = build_case(tmp_path, _PROGRAM, optimize=3, lto=False)
    for value in (0, 7, -3):
        case.compare(stdin=f"{value}\n")

    for name, _expression, _operation in IDENTITIES:
        # The general entry accepts arbitrary runtime objects. Its bool conversion
        # remains either a call to bool or an inline truth conversion.
        body = case.function_ir(rf"^mule_fn_{name}\.body$")
        assert re.search(r"\bcall\b[^\n]*@mule(?:[._]call|_truth)", body)
        assert re.search(r"\bicmp\b", body)
        # These are the actual emitted binary helpers, not a synthetic surrogate.
        # Truth conversion has its own tag-mask operations, which must remain.
        assert not re.search(r"@mule\.(?:fast|fixnum)\.binary\.(?:10|11|12)\b", body)
        assert "@mule_binary" not in body
        assert not re.search(r"@[^\s(\"]*(?:bit_and|bit_or|bit_xor)", body)

    # Follow the actual builtin algorithm as well as the callers: bool's
    # dynamic conversion must not conceal the eliminated bitwise operations.
    conversion = case.function_assembly(r"builtins.*bool_")
    assert "truth" in conversion
    assert "mule_binary" not in conversion
