"""Source-driven tuple selection rewrites and observable fallback boundaries."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("index", ["0", "1", "-1", "-2", "False", "True"])
def test_tuple_selection_bound_records_remove_tuple_and_index(index):
    compilation = compile_source(
        f"def select_record(left, right):\n return (left, right)[{index}]\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "select_record")
    assert not list(known_calls(fn, "%build_tuple"))
    assert not list(known_calls(fn, "%getitem"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("index", ["0", "1", "-1", "-2", "False", "True"])
def test_tuple_selection_constant_key_consumed_by_runtime_record_folds(index):
    compilation = compile_source(
        f"def label_record(record):\n return record + ('/first', '/last')[{index}]\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "label_record")
    assert not list(known_calls(fn, "%build_tuple"))
    assert not list(known_calls(fn, "%getitem"))
    assert list(known_calls(fn, "%add"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("index", ["2", "-3", "1.0", "'bad'"])
def test_tuple_selection_invalid_constant_index_keeps_runtime_error(index):
    compilation = compile_source(f"def select_constant():\n return (10, 20)[{index}]\n")
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "select_constant")
    assert list(known_calls(fn, "%getitem"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "expression",
    [
        "(left, right)[2]",
        "(left, right)[-3]",
        "(left, right)[1.0]",
        "(left, right)[index]",
        "(effect(left), right)[1]",
        "(left, effect(right))[0]",
        "(missing_global, right)[1]",
    ],
)
def test_tuple_selection_unsafe_or_dynamic_cases_retain_index(expression):
    compilation = compile_source(
        f"def select_record(left, right, index):\n return {expression}\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "select_record")
    assert list(known_calls(fn, "%getitem"))
    assert list(known_calls(fn, "%build_tuple"))
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    "body",
    [
        "if flag:\n  missing = left\n return (missing, right)[1]",
        "del left\n return (left, right)[1]",
        "pair = (left, right)\n keep(pair)\n return pair[1]",
    ],
)
def test_tuple_selection_unbound_and_multiple_use_cases_retain_index(body):
    compilation = compile_source(f"def select_record(left, right, flag):\n {body}\n")
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "select_record")
    assert list(known_calls(fn, "%getitem"))
    assert list(known_calls(fn, "%build_tuple"))
    check_ir1_consistency(compilation.components)


_SELECT_SOURCE = """
def select_first(left, right):
    return (left, right)[False]

def select_last(left, right):
    return (left, right)[-1]

def select_constant(record):
    return record + ("/first", "/last")[True]

def selected_effect(record):
    print("selected", record)
    return record

def select_effect(left, right):
    return (left, selected_effect(right), 42)[1]

left = [input()]
right = [input()]
first = select_first(left, right)
last = select_last(left, right)
print(first is left, last is right)
first.append("changed")
print(left, right)
print(select_constant(input()))
print(select_effect(left, right) is right)
"""


@pytest.mark.integration
def test_tuple_selection_runtime_records_remove_optimized_construction_and_lookup(
    tmp_path,
):
    case = build_case(tmp_path, _SELECT_SOURCE, optimize=3, lto=False)
    case.compare(stdin="alpha\nbeta\nrow-17\n")
    case.compare(stdin="\nΩ\nrow-99\n")
    # These complete caller bodies must not construct or index a tuple. The
    # selected effect remains a real call, so inspect its source body as well.
    effect_body = case.function_ir(r"mule_fn_selected_effect\.body$")
    bodies = [
        case.function_ir(rf"mule_fn_select_{name}\.body$")
        for name in ("first", "last", "constant", "effect")
    ]
    for implementation in (*bodies, effect_body):
        assert "mule_sequence_tuple_owned" not in implementation
        assert "constant.tuple" not in implementation
        assert not re.search(
            r"(?:getitem|tuple_slot|tuple_borrow_item)", implementation
        )
    assert all("ret ptr" in body for body in bodies)
    assert "selected_effect" in bodies[-1]
    assert "call " in effect_body


_BOUNDARY_SOURCE = """
events = []

def record(label, value):
    events.append(label)
    return value

def fail():
    events.append("failure")
    raise ValueError("dropped")

def effects(value):
    return (record("left", value), record("middle", value), record("right", value))[1]

def dropped_failure(value):
    return (fail(), record("must-not-run", value))[1]

def unbound(flag, value):
    if flag:
        missing = value
    return (missing, value)[1]

def deleted(value):
    del value
    return (value, 20)[1]

def global_missing(value):
    return (unknown_record, value)[1]

def multiple_use(left, right):
    pair = (left, right)
    selected = pair[1]
    selected.append("mutated")
    print(pair[1] is selected, pair[0] is left, pair)
    return selected

value = [input()]
print(effects(value) is value, events)
try:
    dropped_failure(value)
except ValueError:
    print("value-error", events)
print(unbound(True, value) is value)
try:
    unbound(False, value)
except UnboundLocalError:
    print("conditional-unbound")
try:
    deleted(value)
except UnboundLocalError:
    print("deleted-unbound")
try:
    global_missing(value)
except NameError:
    print("global-unbound")
for index in (2, -3, 1.0, "bad"):
    try:
        print((10, 20)[index])
    except IndexError:
        print("constant-index-error")
    except TypeError:
        print("constant-type-error")
    try:
        print((record("error-left", value), record("error-right", value))[index])
    except IndexError:
        print("index-error", events)
    except TypeError:
        print("type-error", events)
left = [input()]
print(multiple_use(left, value) is value)
"""


@pytest.mark.integration
def test_tuple_selection_fallbacks_preserve_effects_errors_and_aliases(tmp_path):
    case = build_case(tmp_path, _BOUNDARY_SOURCE, optimize=3, lto=False)
    case.compare(stdin="selected-record\nother-record\n")
