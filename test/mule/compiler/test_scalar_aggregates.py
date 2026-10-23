"""Replace local fixed aggregates without changing their child lifetimes."""

import subprocess

import pytest

from test.mule.compiler.rewrite_support import build_case


def test_local_list_alias_mutation_removes_allocation_with_runtime_values(tmp_path):
    case = build_case(
        tmp_path,
        """\
def kernel(size):
    total = 0
    for index in range(size):
        row = [index, index + 1]
        alias = row
        alias[0] = index + 2
        total += row[0] + row[1]
    return total

print(kernel(int(input())))
""",
        optimize=2,
    )
    kernel = case.function_ir(r"^mule_fn_kernel\.(body|spec\.[0-9]+)$")
    assert "@mule_sequence_list(" not in kernel
    assert "@mule_fast_setitem" not in kernel
    assert "@mule_list_getitem" not in kernel
    assert "add i64" in kernel or "llvm.sadd.with.overflow.i64" in kernel
    for value in ("0\n", "1\n", "17\n"):
        case.compare(value)


def test_last_list_alias_deletion_releases_children_in_reverse_order(tmp_path):
    case = build_case(
        tmp_path,
        """\
from _weakref import ref

class Watch:
    pass

def first_gone(reference):
    print('first')

def second_gone(reference):
    print('second')

def kernel(value):
    row = [Watch(), Watch()]
    first = ref(row[0], first_gone)
    second = ref(row[1], second_gone)
    alias = row
    del row
    del alias
    print('done', first() is None, second() is None, value)

kernel(int(input()))
""",
        optimize=2,
    )
    case.compare("10\n")


@pytest.mark.parametrize("display", ("[left, right]", "(left, right)"))
def test_local_aggregate_with_object_children_preserves_identity(tmp_path, display):
    case = build_case(
        tmp_path,
        f"""\
def first(left, right):
    pair = {display}
    alias = pair
    del pair
    selected = alias[0]
    del alias
    return selected

left = [int(input())]
right = [left[0] + 1]
selected = first(left, right)
print(selected is left, selected, right)
""",
        optimize=2,
    )
    body = case.function_ir(r"^mule_fn_first\.body$")
    assert "@mule_sequence_list(" not in body
    assert "@mule_sequence_tuple_owned(" not in body
    for value in ("3\n", "-7\n"):
        case.compare(value)


def test_escaping_resized_and_overlapping_aggregates_keep_object_semantics(tmp_path):
    case = build_case(
        tmp_path,
        """\
published = None

def publish(row):
    global published
    published = row
    row[0] += 10

def apply(value, action):
    row = [value, value + 1]
    action(row)
    return row

def overlap(size):
    saved = [100, 101]
    total = 0
    for index in range(size):
        row = [index, index + 1]
        if index == 0:
            saved = row
        total += saved[0] + row[0]
    return saved, row, total

def resized(value):
    row = [value]
    row.append(value + 1)
    return row

def invalid(value):
    pair = [value]
    try:
        return pair[2]
    except IndexError:
        return 'invalid'

def replacement_error(value):
    row = [value, value + 1]
    try:
        row[0] = 10 // (value - value)
    except ZeroDivisionError:
        return row

value = int(input())
row = apply(value, publish)
print(row is published, row)
saved, latest, total = overlap(4)
print(saved is latest, saved, latest, total)
print(resized(value), invalid(value), replacement_error(value))
""",
        optimize=2,
    )
    for value in ("3\n", "-7\n"):
        case.compare(value)


def test_scalar_children_keep_error_cleanup_order_relative_to_other_locals(tmp_path):
    case = build_case(
        tmp_path,
        """\
from _weakref import ref

handles = []

class Watch:
    pass

def make(label):
    value = Watch()
    def gone(reference):
        print(label)
    handles.append(ref(value, gone))
    return value

def kernel(divisor):
    row = [make('first'), make('second')]
    alias = row
    other = make('other')
    quotient = 1 // divisor
    selected = alias[0]
    del row
    del alias
    return quotient, selected, other

try:
    result = kernel(int(input()))
    print(result[0])
    del result
except ZeroDivisionError:
    print('caught')
""",
        optimize=2,
    )
    body = case.function_ir(
        r"^mule_fn_kernel\.(?:body(?:\.fallback\.fixnum)?|spec\.\d+)$"
    )
    assert "@mule_sequence_list(" not in body
    case.compare("1\n")
    # Traceback records do not own frame locals. Error cleanup therefore
    # precedes the caller's handler, unlike CPython's traceback-local lifetime.
    result = subprocess.run(
        [str(case.executable)],
        input="0\n",
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "second\nfirst\nother\ncaught\n"


def test_scalar_candidate_retains_checked_element_contracts(tmp_path):
    case = build_case(
        tmp_path,
        """\
from mule import optimize

@optimize(safety=3)
def checked(value, convert):
    row: list[int] = [convert(value)]
    alias = row
    return alias[0] + 1

value = input()
callbacks = [int, str]
for convert in callbacks:
    try:
        print(checked(value, convert))
    except TypeError:
        print('checked')
""",
        optimize=2,
    )
    # CPython ignores annotations, so only the accepted integer path shares
    # its result. Mule must retain its explicitly requested element check.
    result = subprocess.run(
        [str(case.executable)],
        input="4\n",
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "5\nchecked\n"
