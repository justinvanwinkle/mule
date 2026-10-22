"""Counted range loops must retain runtime bounds and Python edge semantics."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def _assert_native_induction(body):
    # Follow the loop-carried value, rather than accepting an unrelated add in
    # argument preparation. XOR of the yielded values keeps this loop observable.
    phis = re.findall(r"(%[-\w.]+) = phi i64 ([^\n]+)", body)
    additions = re.findall(r"(%[-\w.]+) = add(?: nuw| nsw)* i64 ([^\n]+)", body)
    comparisons = re.findall(
        r"icmp (?:slt|sgt|sle|sge|ult|ugt|ule|uge) i64 ([^\n]+)", body
    )
    assert any(
        re.search(rf"{re.escape(current)}(?=[,\s])", operands)
        and re.search(rf"{re.escape(following)}(?=[,\s])", incoming)
        and any(
            re.search(rf"{re.escape(value)}(?=[,\s])", comparison)
            for comparison in comparisons
            for value in (current, following)
        )
        for current, incoming in phis
        for following, operands in additions
    ), body
    # These include the generated alwaysinline next-item adapter and its Rust
    # fast helper. Neither may implement induction behind a different symbol.
    assert not re.search(
        r"@[^\s(]*(?:next_item|sequence_available|catch_exception)[^\s(]*\(",
        body,
    ), body


def test_range_prepare_runtime_bounded_loops_use_native_induction(tmp_path):
    case = build_case(
        tmp_path,
        """def forward_kernel(seed: int):
    stop = seed & 1023
    checksum = 0
    for index in range(3, stop, 3):
        checksum ^= index
    return checksum

def reverse_kernel(seed: int):
    start = seed & 1023
    checksum = 0
    for index in range(start, 3, -3):
        checksum ^= index
    return checksum

seed = int(input())
print(forward_kernel(seed), reverse_kernel(seed))
""",
    )
    for seed in (0, 3, 4, 31, 1023, 1024, -1):
        case.compare(stdin=f"{seed}\n")
    for kernel in ("forward_kernel", "reverse_kernel"):
        body = case.function_ir(rf"mule_fn_{kernel}\.body$")
        _assert_native_induction(body)


def test_range_prepare_boundaries_and_shadowed_callable_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """def bounded_step(seed: int):
    step = seed & 3
    values = []
    try:
        for index in range(1, 8, step):
            values.append(index)
        print('step', values)
    except ValueError:
        print('zero-step', values)

def boolean_bounds(flag):
    values = []
    for index in range(False, flag, True):
        values.append((index, type(index) is int))
    print('bool', values)

def upper_edge(seed: int):
    offset = seed & 1
    values = []
    for index in range(4611686018427387901 + offset, 4611686018427387903, 3):
        values.append(index)
    print('upper', values)

def lower_edge(seed: int):
    offset = seed & 1
    values = []
    for index in range(-4611686018427387902 - offset, -4611686018427387904, -3):
        values.append(index)
    print('lower', values)

def unbounded(start, stop, step):
    values = []
    for index in range(start, stop, step):
        values.append(index)
    print('wide', values)

def shadowed(range, seed):
    def argument(label, value):
        print(label)
        return value
    values = []
    for index in range(argument('start', seed), argument('stop', seed + 3), argument('step', 1)):
        values.append(index)
    print('shadow', values)

def substitute(start, stop, step):
    print('substitute', start, stop, step)
    return [stop, start]

seed = int(input())
bounded_step(seed)
boolean_bounds(False)
boolean_bounds(True)
upper_edge(seed)
lower_edge(seed)
unbounded(4611686018427387904, 4611686018427387910, 2)
unbounded(-4611686018427387905, -4611686018427387911, -2)
unbounded(5, 1, 1)
unbounded(1, 5, -1)
unbounded(4, 4, 1)
shadowed(substitute, seed)
""",
    )
    for seed in (0, 1, 2, 3):
        case.compare(stdin=f"{seed}\n")
