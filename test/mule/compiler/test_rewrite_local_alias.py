"""Stable local copies must not add ownership work to record dispatch."""

import re

import pytest

from test.mule.compiler.rewrite_support import build_case
from test.mule.compiler.test_borrowed_lifetimes import component_for


@pytest.mark.parametrize(
    "body",
    [
        "alias = record\nreturn alias is expected",
        "alias = record\nforwarded = alias\nreturn forwarded is expected",
        (
            "alias = record\nif flag is None:\n    return alias is expected\n"
            "return alias is expected"
        ),
    ],
    ids=["record", "forwarded-record", "branch"],
)
def test_stable_record_alias_propagates_from_normal_source(body):
    component = component_for(
        "def route(record, expected, flag):\n"
        + "\n".join("    " + line for line in body.splitlines())
        + "\n"
    )
    comparisons = [
        operation
        for block in component.blocks
        for operation in block.vops
        if operation.info.name == "compare-is"
    ]
    assert comparisons
    loads = [
        operation
        for block in component.blocks
        for operation in block.vops
        if operation.info.name == "load-local"
    ]
    assert any(load.args[0].tn.leaf.name == "record" for load in loads)
    assert all(
        load.args[0].tn.leaf.name not in {"alias", "forwarded"} for load in loads
    )


@pytest.mark.parametrize(
    "body",
    [
        "alias = record\nrecord = expected\nreturn alias is record",
        "alias = record\nalias = expected\nreturn alias is record",
        "alias = record\ncallback(record)\nreturn alias is expected",
        "alias = record\nresult = 1 // flag\nreturn alias is expected",
        (
            "alias = record\nif flag:\n    callback(record)\n    return expected\n"
            "return alias is expected"
        ),
        (
            "alias = record\nwhile flag:\n    callback(record)\n    flag = False\n"
            "return alias is expected"
        ),
    ],
    ids=[
        "owner-reassigned",
        "alias-reassigned",
        "callback",
        "implicit-exception",
        "unread-branch",
        "loop",
    ],
)
def test_record_alias_with_lifetime_barrier_keeps_independent_owner(body):
    component = component_for(
        "def route(record, expected, flag, callback):\n"
        + "\n".join("    " + line for line in body.splitlines())
        + "\n"
    )
    aliases = [
        tn for tn in component.tns if tn.leaf is not None and tn.leaf.name == "alias"
    ]
    assert aliases
    assert all(alias.borrowed_from is None for alias in aliases)
    assert any(component.writes.get(alias) for alias in aliases)


def test_captured_record_alias_owns_cell_value_after_source_replacement():
    component = component_for(
        "def route(record, expected):\n"
        "    alias = record\n"
        "    def read():\n"
        "        return alias\n"
        "    record = expected\n"
        "    return read()\n"
    )
    operations = [operation for block in component.blocks for operation in block.vops]
    store = next(op for op in operations if op.info.name == "value-cell-set")
    cell, value = (ref.tn for ref in store.args)
    acquisition, _ = component.writes[value][0]
    assert acquisition.info.name == "load-local"
    assert acquisition.args[0].tn.leaf.name == "record"
    assert not acquisition.codegen_info.borrow
    assert value.borrowed_from is None
    # Captured variables live in an owned cell rather than a separate plain
    # alias slot. The cell stores the original value before record changes.
    closure = next(op for op in operations if op.info.name == "make-function")
    captured = [
        component.writes[ref.tn][0][0]
        for ref in closure.args
        if component.writes.get(ref.tn)
    ]
    capture = next(
        op for op in captured if op.info.name == "load-local" and op.args[0].tn is cell
    )
    assert not capture.codegen_info.borrow
    assert capture.results[0].tn.borrowed_from is None
    assert cell.borrowed_from is None
    assert cell is not acquisition.args[0].tn


_ROUTING = """
def route(record, expected):
    alias = record
    forwarded = alias
    return forwarded is expected

def route_direct(record, expected):
    return record is expected

def workflow(raw):
    records = [raw, raw + ':accepted', [raw]]
    for record in records:
        print(route(record, record), route_direct(record, record))
        print(route(record, records), route_direct(record, records))

workflow(input())
"""


@pytest.mark.integration
def test_record_routing_has_direct_identity_and_no_alias_refcount_work(tmp_path):
    case = build_case(tmp_path, _ROUTING)
    case.compare("batch:alpha\n")
    case.compare("\u03bb\U0001f642\n")
    aliased = case.function_ir(r"^mule_fn_route\.body$")
    direct = case.function_ir(r"^mule_fn_route_direct\.body$")
    assert "icmp eq ptr" in aliased
    # The actual ownership helpers inline to atomic additions/subtractions.
    # Check those instructions as well as surviving calls, not helper selection.
    assert not re.search(r"atomicrmw\s+add\b", aliased)
    assert not re.search(r"\bcall\b[^\n]*@[^\s(]*(?:retain|incref)", aliased)
    releases = r"atomicrmw\s+sub\b|\bcall\b[^\n]*@[^\s(]*(?:release|decref|destroy)"
    assert re.search(releases, aliased), "Final owner cleanup must remain present"
    assert len(re.findall(releases, aliased)) <= len(re.findall(releases, direct))
    # Frame TLS lookup and safepoints take no record arguments. Traceback
    # recording retains globals, not either identity operand. These shared
    # entry/error operations are not alias ownership work.
    infrastructure = {
        "mule_frame_head_slot",
        "mule_safepoint",
        "mule_traceback_here",
    }
    calls = re.findall(r"\bcall\b[^\n]*@([^\s(]+)\(([^\n]*)\)", aliased)
    direct_calls = re.findall(r"\bcall\b[^\n]*@([^\s(]+)\(", direct)
    for symbol, arguments in calls:
        if symbol in infrastructure:
            assert "%parameter." not in arguments
            assert sum(call == symbol for call, _ in calls) <= direct_calls.count(
                symbol
            )
        else:
            # Final destruction is required. No other language helper may
            # conceal a retain/release pair removed from this identity path.
            assert symbol == "mule_destroy" or symbol.startswith("llvm.")


_LIFETIME_WORKFLOW = """
class Record:
    def __init__(self, label):
        self.label = label
    def __del__(self):
        print('destroy', self.label)

def reassignment(label):
    record = Record(label)
    alias = record
    record = None
    print('after owner replacement', alias.label)
    alias = None
    print('after alias replacement')

def callback(label):
    record = Record(label)
    alias = record
    def replace():
        nonlocal record
        record = None
        print('callback retained', alias.label)
    replace()
    print('callback returned', alias.label)
    alias = None
    print('callback alias released')

def exception(label, divisor):
    record = Record(label)
    alias = record
    try:
        record = None
        value = 10 // divisor
        print('quotient', value)
    except ZeroDivisionError:
        print('handler retained', alias.label)
    finally:
        print('finally retained', alias.label)
    alias = None
    print('exception alias released')

def capture(label):
    record = Record(label)
    alias = record
    def read():
        return alias.label
    record = None
    print('captured', read())
    alias = None
    print('captured alias released')

def branch(label, select):
    record = Record(label)
    alias = record
    record = None
    if select:
        print('read branch', alias.label)
    else:
        print('unread branch')
    alias = None
    print('branch alias released')

def loop(label, count):
    record = Record(label)
    alias = record
    for index in range(count):
        record = None
        print('loop retained', index, alias.label)
    alias = None
    record = None
    print('loop owners released')

label = input()
reassignment(label)
callback(label)
exception(label, 0)
exception(label, 2)
capture(label)
branch(label, True)
branch(label, False)
loop(label, 0)
loop(label, 2)
"""


@pytest.mark.integration
def test_record_alias_lifetime_boundaries_match_native_oracle(tmp_path):
    case = build_case(tmp_path, _LIFETIME_WORKFLOW)
    case.compare("runtime-record\n")
