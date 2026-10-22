"""Display unpack rewrites retain Python stores, effects, and reference ownership."""

import re

import pytest

from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


def _function(source, name):
    compilation = compile_source(source)
    return next(fn for fn in compilation.lambdas if fn.source_name == name)


def _assert_elided(source, name):
    fn = _function(source, name)
    for operation in ("%build_tuple", "%build_list", "%unpack", "%unpack_ref"):
        assert not list(known_calls(fn, operation)), operation


def _assert_native_elided(case, name):
    body = case.function_ir(rf"^mule_fn_{name}\.")
    # Follow the lowering endpoints, not merely the choice of an unpack wrapper.
    # A display must not become an allocation hidden behind the fast unpack path.
    for symbol in (
        "mule_sequence_tuple_owned",
        "mule_sequence_list",
        "mule.fast.unpack",
        "mule_fast_unpack",
        "mule_tuple_slot",
        "mule_tuple_borrow_item",
        "mule_storage_read",
        "unpack.out",
    ):
        assert symbol not in body, symbol
    return body


@pytest.mark.integration
@pytest.mark.parametrize("display", ["(right, left + right)", "[right, left + right]"])
def test_unpack_display_state_update_removes_construction_and_unpack(tmp_path, display):
    source = f"""
def unpack_step(count, left, right):
    while count > 0:
        left, right = {display}
        count -= 1
    return left + right

count = int(input())
print(unpack_step(count, int(input()), int(input())))
"""
    _assert_elided(source, "unpack_step")
    case = build_case(tmp_path, source)
    body = _assert_native_elided(case, "unpack_step")
    # The runtime-dependent recurrence must remain, rather than folding the loop.
    assert re.search(r"\badd\b|@[^\s(]*add[^\s(]*\(", body)
    for stdin in ("0\n4\n9\n", "9\n1\n2\n", "5\n-3\n7\n"):
        case.compare(stdin=stdin)


@pytest.mark.integration
def test_unpack_constant_display_drives_runtime_state_without_tuple_load(tmp_path):
    source = """
def unpack_constants(count):
    low, high = (3, 11)
    while count > 0:
        low += high
        count -= 1
    return low

print(unpack_constants(int(input())))
"""
    _assert_elided(source, "unpack_constants")
    case = build_case(tmp_path, source)
    body = _assert_native_elided(case, "unpack_constants")
    assert "mule.literal.slot" not in body
    assert "mule.cached.tuple" not in body
    assert "mule.borrowed.tuple" not in body
    assert re.search(r"\b(add|mul)\b|@[^\s(]*(add|mul)[^\s(]*\(", body)
    for count in (0, 1, 17):
        case.compare(stdin=f"{count}\n")


@pytest.mark.integration
def test_unpack_display_rhs_effects_failure_and_alias_swaps_match_python(tmp_path):
    source = """
def produce(value, label, fail):
    print('rhs', label)
    if fail:
        raise ValueError('stop')
    return value

def unpack_effects(left, right, fail):
    original_left = left
    original_right = right
    try:
        left, right = (produce(right, 'first', False),
                       produce(left, 'second', fail))
    except ValueError:
        print('unchanged', left is original_left, right is original_right)
        return
    print('swapped', left is original_right, right is original_left)
    left.append(99)
    print('aliases', original_right, right)
    left, right = [right, left]
    print('restored', left is original_left, right is original_right)
    left, left = (right, left)
    print('repeated target', left is original_left)

seed = int(input())
unpack_effects([seed], [seed + 1], False)
unpack_effects([seed], [seed + 1], True)
shared = [seed]
unpack_effects(shared, shared, False)
"""
    _assert_elided(source, "unpack_effects")
    case = build_case(tmp_path, source)
    _assert_native_elided(case, "unpack_effects")
    # The actual RHS routine still executes observable printing and raising.
    producer = case.function_ir(r"^mule_fn_produce\.")
    assert "print" in producer
    for seed in (7, -4):
        case.compare(stdin=f"{seed}\n")


@pytest.mark.integration
def test_unpack_mismatch_nested_and_subscript_targets_keep_fallback_semantics(tmp_path):
    source = """
def produce(value, label):
    print('rhs', label)
    return value

def unpack_too_many(value):
    left = 'old-left'
    right = 'old-right'
    try:
        left, right = (produce(value, 'one'), produce(value + 1, 'two'),
                       produce(value + 2, 'three'))
    except ValueError:
        print('too many', left, right)

def unpack_too_few(value):
    left = 'old-left'
    right = 'old-right'
    try:
        left, right = [produce(value, 'only')]
    except ValueError:
        print('too few', left, right)

def unpack_nested(value, bad):
    first = 'old-first'
    second = 'old-second'
    third = 'old-third'
    if bad:
        inner = [value]
    else:
        inner = [value, value + 1]
    try:
        first, (second, third) = (produce(value + 2, 'outer'), inner)
    except ValueError:
        print('nested failure', first, second, third)
    else:
        print('nested success', first, second, third)

def unpack_subscript(target, value, index):
    second = 'old-second'
    try:
        target[index], second = (produce(value, 'store-first'),
                                 produce(value + 1, 'store-second'))
    except IndexError:
        print('subscript failure', target, second)
    else:
        print('subscript success', target, second)

value = int(input())
unpack_too_many(value)
unpack_too_few(value)
unpack_nested(value, False)
unpack_nested(value, True)
unpack_subscript([0], value, 0)
unpack_subscript([], value, 0)
"""
    for name in (
        "unpack_too_many",
        "unpack_too_few",
        "unpack_nested",
        "unpack_subscript",
    ):
        assert list(known_calls(_function(source, name), "%unpack")), name
    case = build_case(tmp_path, source)
    # Inspect actual calls rather than matching "unpack" in the function header.
    fallback = case.function_ir(r"^mule_fn_unpack_nested\.")
    assert re.search(r"(?:call|invoke)[^\n]*@[^\n]*(?:unpack|shape_matches)", fallback)
    for value in (0, 23):
        case.compare(stdin=f"{value}\n")
