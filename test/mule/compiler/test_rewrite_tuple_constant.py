"""Recursive tuple constants in runtime-indexed record processing."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case

pytestmark = pytest.mark.integration


def _assert_tuple_cache_hit(case, lookup):
    """Follow the consumed table's loader and prove its non-null path reuses it."""
    loaders = set(
        re.findall(r"call (?:fastcc )?ptr @(mule\.literal\.tuple\.\d+)\(\)", lookup)
    )
    assert loaders, "The runtime lookup must consume a loaded constant table"
    assert "mule_sequence_tuple_owned" not in lookup
    for loader in loaders:
        # Follow the call result into the actual indexing operation, rather than
        # accepting an unrelated cache helper elsewhere in the LLVM module.
        values = re.findall(
            rf"(%[\w.-]+) = call (?:fastcc )?ptr @{re.escape(loader)}\(\)", lookup
        )
        assert all(
            re.search(rf"@mule_fast_getitem\(ptr nonnull {re.escape(value)},", lookup)
            for value in values
        )
        body = case.function_ir(rf"^{re.escape(loader)}$")
        labels = list(re.finditer(r"^([\w.-]+):[^\n]*\n", body, re.MULTILINE))
        blocks = {
            match[1]: body[
                match.end() : labels[index + 1].start()
                if index + 1 < len(labels)
                else body.rfind("}")
            ]
            for index, match in enumerate(labels)
        }
        entry = blocks[labels[0][1]]
        loaded = re.search(
            r"(%[\w.-]+) = load ptr, ptr (@mule\.literal\.slot\.\d+)", entry
        )
        assert loaded, "The loader must read the table's persistent cache slot"
        cached, slot = loaded.groups()
        comparison = re.search(
            rf"(%[\w.-]+) = icmp (eq|ne) ptr {re.escape(cached)}, null", entry
        )
        assert comparison, "The loader must test the cached table for absence"
        branch = re.search(
            rf"br i1 {re.escape(comparison[1])}, label %([\w.-]+), label %([\w.-]+)",
            entry,
        )
        assert branch
        hit = branch[2] if comparison[2] == "eq" else branch[1]
        pending = [hit]
        reachable = set()
        while pending:
            label = pending.pop()
            if label not in reachable:
                reachable.add(label)
                pending.extend(re.findall(r"label %([\w.-]+)", blocks[label]))
        hot = entry + "\n" + "\n".join(blocks[label] for label in reachable)
        # No hidden helper may allocate on a cache hit. The only permitted
        # calls retain the already-loaded object or describe stack lifetimes.
        calls = re.findall(r"\bcall\b[^\n@]*@([\w.-]+)\(", hot)
        assert all(
            callee == "mule_retain" or callee.startswith("llvm.lifetime.")
            for callee in calls
        ), calls
        assert not re.search(rf"store ptr [^\n]*ptr {re.escape(slot)}\b", hot)
        returns = re.findall(r"ret ptr (%[\w.-]+)", hot)
        assert returns, "The cache-hit path must return the existing table"
        for value in returns:
            if value != cached:
                phi = re.search(rf"{re.escape(value)} = phi ptr ([^\n]+)", hot)
                assert phi, "The returned table must derive from the cache load"
                incoming = re.findall(r"\[ (%[\w.-]+), %([\w.-]+) \]", phi[1])
                hot_incoming = [
                    value for value, label in incoming if label in reachable
                ]
                assert hot_incoming and all(value == cached for value in hot_incoming)
        # Cold initialization must publish an actual constructed tuple into the
        # same slot whose non-null value the hit path returns.
        allocations = re.findall(
            r"(%[\w.-]+) = (?:tail )?call ptr @mule_sequence_tuple_owned\(", body
        )
        assert any(
            re.search(rf"store ptr {re.escape(value)}, ptr {re.escape(slot)}\b", body)
            for value in allocations
        )


def test_recursive_constant_record_table_avoids_repeated_construction(tmp_path):
    case = build_case(
        tmp_path,
        """
def lookup_record(index):
    table = (
        ('zero', (None, False, 0, 0.0, b'zero', 1208925819614629174706176)),
        ('one', (True, 1, 1.25, 'text', b'bytes', 1208925819614629174706177)),
        ('two', (None, True, 2, 2.5, b'two', 1208925819614629174706178)),
    )
    return table[index]

count = int(input())
first = lookup_record(0)
print(first is lookup_record(0), first[1] is lookup_record(0)[1])
total = 0
i = 0
while i < count:
    record = lookup_record(i % 3)
    print(record[0], record[1])
    total += record[1][5] + i
    i += 1
print(total, first)
for index in [-1, 3, -4]:
    try:
        print(lookup_record(index))
    except IndexError:
        print('index error')
""",
    )
    for count in (0, 1, 7):
        case.compare(f"{count}\n")

    lookup = case.function_ir(r"lookup_record.*(?:body|spec)")
    assert "getitem" in lookup, "The runtime table lookup must remain observable"
    _assert_tuple_cache_hit(case, lookup)


def test_tuple_near_misses_preserve_freshness_order_and_error_cleanup(tmp_path):
    case = build_case(
        tmp_path,
        """
def mutable_record(value):
    return ('mutable', ([value], (None, 1208925819614629174706176)))

def dynamic_record(value):
    return ('dynamic', (value, None))

events = []
def field(value, fail):
    events.append(value)
    if fail:
        raise ValueError('field failed')
    return value

def evaluated_record(value, fail):
    return (field(value, False), (field(value + 1, fail), field(value + 2, False)))

value = int(input())
a = mutable_record(value)
b = mutable_record(value)
print(a is b, a[1] is b[1], a[1][0] is b[1][0])
a[1][0][0] = value + 10
print(a, b)
x = dynamic_record(value)
y = dynamic_record(value)
print(x is y, x[1] is y[1], x, y)
for fail in [False, True, False]:
    try:
        print(evaluated_record(value, fail))
    except ValueError:
        print('field error')
print(events, a, b)
""",
    )
    for value in (5, 1208925819614629174706176):
        case.compare(f"{value}\n")

    # Each returned record escapes through identity tests and printing. Mutable
    # and runtime-dependent elements must keep real tuple construction paths.
    for function in ("mutable_record", "dynamic_record", "evaluated_record"):
        body = case.function_ir(rf"{function}.*(?:body|spec)")
        assert "mule_sequence_tuple_owned" in body
