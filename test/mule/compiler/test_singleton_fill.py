"""Singleton repetition specializes in IR1, without changing Python dispatch."""

import re
from textwrap import dedent

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case


@pytest.mark.parametrize("expression", ["[item] * n", "n * [item]"])
def test_singleton_repeat_exact_count_uses_fill(expression):
    compilation = compile_source(
        "def process():\n"
        " def repeat(item, n):\n"
        f"  return {expression}\n"
        " for n in range(-2, 10):\n"
        "  print(repeat(None, n))\n"
        "process()\n"
    )
    fn = next(fn for fn in compilation.lambdas if fn.source_name == "repeat")
    assert list(known_calls(fn, "%_list_fill"))
    assert not list(known_calls(fn, "%build_list"))
    check_ir1_consistency(compilation.components)


def test_singleton_repeat_unknown_count_preserves_dispatch():
    compilation = compile_source("def repeat(item, n):\n return [item] * n\n")
    fn = compilation.lambdas[1]
    assert list(known_calls(fn, "%multiply"))
    assert not list(known_calls(fn, "%_list_fill"))


_FILL_SOURCE = dedent(
    """
    def process(n):
        def right(item, count):
            return [item] * count
        def left(item, count):
            return count * [item]
        item = [n]
        a = right(item, n)
        b = left(item, n)
        table = {}
        for index in range(n):
            table[index] = index
        print(len(table), table.get(n - 1, -1))
        print(len(a), len(b))
        if n > 0:
            print(a[0] is item, a[-1] is b[0])
            item.append(9)
            print(a[0])
            a[0] = None
            print(b[-1])
        else:
            print(item)

    for n in range(-2, len(input())):
        process(n)

    def observe(item, count):
        try:
            print(len([item] * count))
            print(len(count * [item]))
        except OverflowError:
            print('OverflowError')
        except MemoryError:
            print('MemoryError')
        except TypeError:
            print('TypeError')

    for count in (-(1 << 100), -(1 << 63), 1 << 63, True, False, 1.5):
        observe(None, count)

    try:
        print(len([None] * -(1 << 100)))
    except OverflowError:
        print('OverflowError')
    try:
        print(len((1 << 63) * [None]))
    except OverflowError:
        print('OverflowError')

    try:
        print(len([None] * ((1 << 63) - 1)))
    except MemoryError:
        print('MemoryError')

    class Count:
        def __index__(self):
            print('index')
            return 3
    class Reflected:
        def __rmul__(self, sequence):
            print('reflected', sequence)
            return [4]
    observe(None, Count())
    observe(None, Reflected())

    def element():
        print('element')
        return []
    def count():
        print('count')
        return len(input())
    print(len([element()] * count()))
    print(len(count() * [element()]))
    """
)


def _integer_loop_ir(function):
    """Select cyclic blocks around the native induction value, not cold guards."""
    labels = list(re.finditer(r"^([-\w.$]+):[^\n]*$", function, re.MULTILINE))
    blocks = {
        match[1]: function[
            match.end() : labels[index + 1].start()
            if index + 1 < len(labels)
            else len(function)
        ]
        for index, match in enumerate(labels)
    }
    successors = {
        label: set(re.findall(r"label %([-\w.$]+)", body)) & blocks.keys()
        for label, body in blocks.items()
    }

    def reachable(start):
        found = set()
        pending = [start]
        while pending:
            label = pending.pop()
            if label not in found:
                found.add(label)
                pending.extend(successors[label] - found)
        return found

    induction = [
        label for label, body in blocks.items() if re.search(r"= phi i64", body)
    ]
    assert induction, "The fill loop must carry a native integer induction value"
    selected = set()
    for header in induction:
        selected.update(
            label for label in reachable(header) if header in reachable(label)
        )
    return "\n".join(blocks[label] for label in selected)


@pytest.mark.integration
def test_singleton_fill_preserves_identity_counts_and_evaluation_order(tmp_path):
    case = build_case(tmp_path, _FILL_SOURCE)
    fill = case.function_ir(
        r"^mule_sequences_mule_fn__list_fill\.body$", unit="builtins"
    )
    assert "mule_storage_reserve" in fill
    loop = _integer_loop_ir(fill)
    assert re.search(r"icmp (?:eq|[su]lt) i64", loop)
    assert re.search(r"add (?:nuw )?(?:nsw )?i64", loop)
    assert "@mule_storage_append(" in loop
    assert not re.search(r"@mule_(?:binary|compare|unary)", loop)
    indices = case.function_ir(r"_empty_indices\.spec\.\d+$", unit="builtins")
    assert "@mule_sequences_mule_fn__list_fill.body(" in indices
    for operation in ("right", "left"):
        repeat = case.function_ir(rf"\.{operation}\.spec\.\d+$")
        if "@mule.protocol._list_fill(" in repeat:
            protocol = case.function_ir(r"^mule\.protocol\._list_fill$")
            assert "@mule_sequences_mule_fn__list_fill.body(" in protocol
        else:
            assert "@mule_sequences_mule_fn__list_fill.body(" in repeat
    case.compare("abcd\nx\nxx\n")
    case.compare("\n\n\n")
