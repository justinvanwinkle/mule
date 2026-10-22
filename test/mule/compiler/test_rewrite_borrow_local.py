"""Stable local roots remove copies without changing observable lifetimes."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_borrowed_lifetimes import component_for


@pytest.mark.parametrize(
    "body",
    [
        (
            "    result = value is (None if flag else False)\n"
            "    return result, value is None\n"
        ),
        (
            "    hits = 0\n"
            "    for index in range(flag):\n"
            "        hits += value is (None if index else False)\n"
            "    return hits, value is None\n"
        ),
        (
            "    try:\n"
            "        result = value is (10 // flag)\n"
            "    except ZeroDivisionError:\n"
            "        return value is None\n"
            "    return result, value is None\n"
        ),
    ],
    ids=["branch", "loop-backedge", "exception-successor"],
)
def test_local_read_borrows_from_live_root_through_control_flow(body):
    component = component_for("def check(value, flag):\n" + body)
    comparisons = [
        operation
        for block in component.blocks
        for operation in block.vops
        if operation.info.name == "compare-is"
    ]
    assert comparisons
    # Inspect the operand actually consumed by the comparison, not an unrelated
    # borrowed load elsewhere in the function.
    operand = comparisons[0].args[0].tn
    acquisition, _ = component.writes[operand][0]
    assert acquisition.info.name == "load-local"
    assert acquisition.codegen_info.name == "value"
    assert not acquisition.codegen_info.move
    assert acquisition.codegen_info.borrow
    assert operand.borrowed_from is acquisition.args[0].tn


@pytest.mark.parametrize(
    ("body", "resume"),
    [
        (
            (
                "    result = value is (None if flag else (value := False))\n"
                "    return result, value\n"
            ),
            False,
        ),
        (
            (
                "    alias = value\n"
                "    result = value is (None if flag else (alias := alias + 1.0))\n"
                "    return result, value, alias\n"
            ),
            False,
        ),
        (
            ("    result = value is (yield flag)\n    return result, value\n"),
            True,
        ),
        (
            (
                "    def replace():\n"
                "        nonlocal value\n"
                "        value = flag\n"
                "        return value\n"
                "    result = value is replace()\n"
                "    return result, value\n"
            ),
            False,
        ),
    ],
    ids=["owner-overwrite", "alias-uniqueness", "suspension", "callback-cell"],
)
def test_local_read_requires_ownership_across_invalidating_boundary(body, resume):
    component = component_for("def check(value, flag):\n" + body, resume=resume)
    comparison = next(
        operation
        for block in component.blocks
        for operation in block.vops
        if operation.info.name == "compare-is"
    )
    operand = comparison.args[0].tn
    assert operand.borrowed_from is None


@pytest.mark.integration
def test_runtime_record_branch_has_native_comparison_without_owned_read(tmp_path):
    case = build_case(
        tmp_path,
        """
class Record:
    def __init__(self, payload):
        self.payload = payload

def record_match(value, flag):
    result = value is (None if flag else False)
    return result or value is None

raw = input()
record = Record(raw)
for value in (record, raw, None, False):
    print(record_match(value, bool(len(raw))))
    print(record_match(value, False))
print(record.payload)
""",
    )
    body = case.function_ir(r"^mule_fn_record_match\.body$")
    assert re.search(r"icmp (?:eq|ne) ptr", body), body
    # LLVM inlines the fast ownership wrappers. Reject both out-of-line retain
    # calls and an inlined reference-count increment. Root destruction remains
    # required; do not ban releases of the owned parameter at function exit.
    assert not re.search(r"@mule(?:\.fast\.)?_?retain\b", body), body
    assert not re.search(r"atomicrmw\s+add\b.*?,\s+i\d+\s+1\b", body), body
    assert not re.search(r"atomicrmw\s+sub\b.*?,\s+i\d+\s+-1\b", body), body
    assert "@mule_compare" not in body, body
    parameters = re.findall(r"ptr (%[\w.]+)", body.split("\n", 1)[0])
    root = parameters[1]
    # Follow the root's inlined release through the last-reference test to
    # destruction. Merely finding a decrement could match the flag's cleanup.
    decrements = list(
        re.finditer(
            rf"(%[\w.]+) = atomicrmw sub ptr {re.escape(root)}, i64 1 release",
            body,
        )
    )
    assert decrements, body
    for decrement in decrements:
        last = re.search(
            rf"(%[\w.]+) = icmp eq i64 {re.escape(decrement[1])}, 1\n"
            r"\s+br i1 \1, label %([\w.]+),",
            body[decrement.end() :],
        )
        assert last, body
        destroy_block = re.search(
            rf"^{re.escape(last[2])}:.*?\n(.*?)(?=^[\w.]+:|^}})",
            body,
            re.MULTILINE | re.DOTALL,
        )
        assert destroy_block, body
        destroyed = {root}
        destroyed.update(
            phi[1]
            for phi in re.finditer(r"(%[\w.]+) = phi ptr ([^\n]+)", destroy_block[1])
            if re.search(rf"\[ {re.escape(root)},", phi[2])
        )
        assert any(
            re.search(
                rf"@mule_destroy\(ptr(?: nonnull)? {re.escape(value)}\)",
                destroy_block[1],
            )
            for value in destroyed
        ), destroy_block[1]
    for raw in ("", "record-λ", "False"):
        case.compare(raw + "\n")


@pytest.mark.integration
def test_runtime_local_roots_preserve_mutation_escape_and_destruction(tmp_path):
    case = build_case(
        tmp_path,
        """
class Record:
    def __init__(self, label):
        self.label = label
    def __del__(self):
        print("destroy", self.label)

def process(value, count):
    hits = 0
    for index in range(count):
        hits += value is (None if index else False)
    return hits, value is None

def replace(flag):
    value = Record("old")
    result = value is (None if flag else (value := Record("new")))
    print("replace", result, value.label)
    del value

def callback():
    value = Record("callback-old")
    def change():
        nonlocal value
        value = Record("callback-new")
        print("changed")
        return value
    print("callback", value is change())
    del value

def escaping():
    value = Record("escaped")
    escaped = [value]
    value = None
    print("escape", escaped[0].label)
    del escaped

def suspended():
    value = Record("suspended")
    result = value is (yield value.label)
    print("resumed", result, value.label)
    del value

def handled(divisor):
    value = Record("handled")
    try:
        result = value is (10 // divisor)
        print("normal", result)
    except ZeroDivisionError:
        print("handler", value.label)
    finally:
        print("finally", value.label)
    del value

def mutate(number):
    value = number
    alias = value
    result = value is (alias := alias + 1.0)
    print("mutation", result, value == number, alias == number + 1.0)

raw = input()
count = len(raw)
for value in (None, False, raw):
    print("processed", process(value, count))
replace(False)
replace(True)
callback()
escaping()
generator = suspended()
print("yielded", next(generator))
try:
    generator.send(None)
except StopIteration:
    print("finished")
del generator
handled(0)
handled(count + 1)
mutate(float(count) + 0.25)
""",
    )
    for raw in ("", "runtime-record"):
        case.compare(raw + "\n")
