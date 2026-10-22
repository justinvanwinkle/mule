"""Empty truth defaults fold inside callbacks used by the compiled filter."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


def fallback_source(container):
    return f"""
def keep_missing(item):
    if item is None:
        return not {container}
    return item

def select_records(records):
    return list(filter(keep_missing, records))
"""


@pytest.mark.parametrize("container", ["[]", "()"])
def test_empty_truth_default_folds_in_normal_filter_compilation(container):
    compilation = compile_source(fallback_source(container))
    predicate = next(
        fn for fn in compilation.lambdas if fn.source_name == "keep_missing"
    )
    assert not list(known_calls(predicate, "%build_list"))
    assert not list(known_calls(predicate, "%build_tuple"))
    assert not list(known_calls(predicate, "%unary_not"))
    # The callback still distinguishes the runtime item from the missing sentinel.
    assert list(known_calls(predicate, "%compare_is"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
@pytest.mark.parametrize("container", ["[]", "()"])
def test_filter_empty_default_removes_allocation_and_truth_test(tmp_path, container):
    source = (
        fallback_source(container)
        + """
value = int(input())
marker = [value]
records = [None, 0, value, [], marker]
selected = select_records(records)
print(selected)
print(selected[0] is None, selected[-1] is marker)
marker.append(value + 1)
print(selected[-1])
"""
    )
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for value in (0, 7, -3):
        case.compare(stdin=f"{value}\n")

    body = case.function_ir(r"mule_fn_keep_missing(?:\.|$)")
    # Filter must still truth-test runtime callback results in its own unit.
    # This callback only compares identity; its empty-container truth work vanishes.
    assert "@mule.true" in body, body
    assert re.search(r"\bicmp (?:eq|ne)\b", body), body
    assert not re.search(
        r"@[^\s(]*(?:sequence_(?:list|tuple)|truth|unary\.3)[^\s(]*\s*\(",
        body,
    ), body


@pytest.mark.parametrize("container", ["[record(value)]", "(record(value),)"])
def test_nonempty_truth_retains_effectful_element_evaluation(container):
    compilation = compile_source(
        "def record(value):\n print(value)\n return value\n"
        f"def reject(value):\n return not {container}\n"
    )
    predicate = next(fn for fn in compilation.lambdas if fn.source_name == "reject")
    assert list(known_calls(predicate, "%unary_not"))
    operation = "%build_list" if container.startswith("[") else "%build_tuple"
    assert list(known_calls(predicate, operation))
    check_ir1_consistency(compilation.components)


EFFECTFUL_FILTER = """
events = []

class Element:
    def __bool__(self):
        raise AssertionError('container truth must not inspect an element')

def record(value):
    events.append(value)
    if value < 0:
        raise ValueError('element failure')
    return Element()

def reject_list(value):
    return not [record(value)]

def reject_tuple(value):
    return not (record(value),)

def reject_alias(items, value):
    alias = items
    items.append(value)
    result = not items
    print(alias is items, alias)
    return result

value = int(input())
for predicate in (reject_list, reject_tuple):
    events.clear()
    try:
        print(list(filter(predicate, [value, -1, value + 1])))
    except ValueError as error:
        print(str(error))
    print(events)

items = []
print(not items)
print(reject_alias(items, value))
print(not items)
"""


@pytest.mark.integration
def test_nonempty_and_mutated_filter_boundaries_match_python(tmp_path):
    case = build_case(tmp_path, EFFECTFUL_FILTER, optimize=3, lto=False)
    for value in (0, 9, -2):
        case.compare(stdin=f"{value}\n")
