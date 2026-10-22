"""Real dispatch sites exercise class operands and tuple-free isinstance calls."""

import re

import pytest

from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

_CLASSES = """
class Alpha:
    pass

class Derived(Alpha):
    pass

class Beta:
    pass

class Other:
    pass
"""

_SELECT_VALUE = """
kind = input()
if kind == 'alpha':
    value = Alpha()
elif kind == 'derived':
    value = Derived()
elif kind == 'beta':
    value = Beta()
else:
    value = Other()
"""

_DIRECT = (
    _CLASSES
    + """
def count_alpha(records):
    total = 0
    for value in records:
        if isinstance(value, Alpha):
            total += 1
    return total

def count_supported(records):
    total = 0
    for value in records:
        if isinstance(value, (Alpha, Beta)):
            total += 1
    return total
"""
    + _SELECT_VALUE
    + """
records = [value, Derived(), Other()]
print(count_alpha(records), count_supported(records))
"""
)

_BOUNDARIES = (
    _CLASSES
    + """
def operand(label, candidate):
    print('operand', label)
    return candidate

def ordered(value):
    return isinstance(value, (
        operand('first', Alpha),
        operand('invalid', 42),
        operand('last', Beta),
    ))

def invalid_first(value):
    return isinstance(value, (42, Alpha))

def nested(value):
    return isinstance(value, ((Alpha, Beta), 42))

def empty(value):
    return isinstance(value, ())

def dynamic(value, classes):
    return isinstance(value, classes)
"""
    + _SELECT_VALUE
    + """
try:
    print('ordered', ordered(value))
except TypeError:
    print('ordered TypeError')
try:
    print('invalid first', invalid_first(value))
except TypeError:
    print('invalid first TypeError')
try:
    print('nested', nested(value))
except TypeError:
    print('nested TypeError')
print('empty', empty(value))
classes = tuple([Alpha, Beta])
print('dynamic', dynamic(value, classes))
bad_classes = tuple([Alpha, 42])
try:
    print('dynamic invalid', dynamic(value, bad_classes))
except TypeError:
    print('dynamic TypeError')
"""
)

_SHADOWED = (
    _CLASSES
    + """
def operand(label, candidate):
    print('operand', label)
    return candidate

def isinstance(value, classes):
    print('shadow', len(classes), classes[0] is Alpha, classes[1] is Beta)
    return 'custom result'

def route(value):
    return isinstance(value, (operand('first', Alpha), operand('second', Beta)))
"""
    + _SELECT_VALUE
    + """
print(route(value))
"""
)


def _function(compilation, name):
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


def test_isinstance_normal_source_separates_single_and_display_class_operands():
    compilation = compile_source(_DIRECT)
    for name, operand_count in (("count_alpha", 2), ("count_supported", 3)):
        function = _function(compilation, name)
        calls = list(known_calls(function, "%isinstance"))
        assert len(calls) == 1
        assert len(calls[0].args) == operand_count
        assert list(known_calls(function, "%build_tuple")) == []


def test_isinstance_display_match_keeps_evaluated_invalid_and_nested_operands():
    compilation = compile_source(_BOUNDARIES)
    for name, operand_count in (("ordered", 4), ("invalid_first", 3), ("nested", 3)):
        function = _function(compilation, name)
        calls = list(known_calls(function, "%isinstance"))
        assert len(calls) == 1
        assert len(calls[0].args) == operand_count
    # A parameter and the empty tuple are not display operands this rule can flatten.
    for name in ("empty", "dynamic"):
        assert list(known_calls(_function(compilation, name), "%isinstance")) == []


def test_isinstance_shadowed_builtin_keeps_observable_tuple_argument():
    compilation = compile_source(_SHADOWED)
    route = _function(compilation, "route")
    assert list(known_calls(route, "%isinstance")) == []
    assert len(list(known_calls(route, "%build_tuple"))) == 1


@pytest.mark.integration
def test_isinstance_runtime_dispatch_has_no_tuple_allocation_in_optimized_path(
    tmp_path,
):
    case = build_case(tmp_path, _DIRECT, optimize=3, lto=False)
    for kind in ("alpha", "derived", "beta", "other"):
        case.compare(stdin=kind + "\n")

    for name, class_count in (("count_alpha", 1), ("count_supported", 2)):
        body = case.function_ir(rf"mule_fn_{name}\.body$", optimized=True)
        assert re.search(
            rf"@mule_isinstance_any\([^\n]*,\s*i64\s+{class_count}\)", body
        ), body
        assert "@mule_sequence_tuple_owned(" not in body, body
        assert "@mule.cached.tuple" not in body, body
        assert "@mule.literal.tuple" not in body, body
        assert "@mule.call.2(" not in body, body

    # Follow the selected runtime helper rather than treating its selection as proof.
    # Only its non-class/metaclass fallback may invoke the compiled general protocol.
    helper = case.function_assembly(r"^mule_isinstance_any$")
    assert re.search(r"\b(?:cmp|test)[a-z]*\s", helper), helper
    assert "<mule_sequence_tuple" not in helper, helper
    assert "<mule_tuple_new" not in helper, helper
    if "<mule_isinstance_class>" in helper:
        plain_class = case.function_assembly(r"^mule_isinstance_class$")
        assert re.search(r"\b(?:cmp|test)[a-z]*\s", plain_class), plain_class
        assert "<mule_sequence_tuple" not in plain_class, plain_class
        assert "<mule_tuple_new" not in plain_class, plain_class
        assert "<mule_call" not in plain_class, plain_class


@pytest.mark.integration
def test_isinstance_class_expression_order_errors_and_fallbacks_match_python(tmp_path):
    case = build_case(tmp_path, _BOUNDARIES, optimize=3, lto=False)
    for kind in ("alpha", "derived", "beta", "other"):
        case.compare(stdin=kind + "\n")
    body = case.function_ir(r"mule_fn_ordered\.body$", optimized=True)
    assert re.search(r"@mule_isinstance_any\([^\n]*,\s*i64\s+3\)", body), body
    assert "@mule_sequence_tuple_owned(" not in body, body


@pytest.mark.integration
def test_isinstance_shadowed_builtin_observes_tuple_and_expression_order(tmp_path):
    case = build_case(tmp_path, _SHADOWED, optimize=3, lto=False)
    for kind in ("alpha", "other"):
        case.compare(stdin=kind + "\n")
