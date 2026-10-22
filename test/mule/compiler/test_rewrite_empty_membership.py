"""Empty membership folds in runtime filtering without deleting evaluated work."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

EMPTY_FILTERS = (
    ("tuple_in", "in", "()"),
    ("list_in", "in", "[]"),
    ("tuple_not_in", "not in", "()"),
    ("list_not_in", "not in", "[]"),
)


def _filter_source(name, operator, container):
    # The checksum consumes actual record characters even when every record is
    # rejected. Only membership and the temporary container may disappear.
    return f"""
def filter_{name}(records: str):
    checksum = 0
    accepted = 0
    index = 0
    while index < len(records):
        value = records[index]
        checksum += ord(value)
        if value {operator} {container}:
            accepted += 1
        index += 1
    return checksum + accepted
"""


@pytest.mark.parametrize("name,operator,container", EMPTY_FILTERS)
def test_empty_membership_filter_ir1_removes_only_membership_and_container(
    name, operator, container
):
    compilation = compile_source(_filter_source(name, operator, container))
    function = compilation.lambdas[1]
    for operation in (
        "%compare_in",
        "%compare_not_in",
        "%build_list",
        "%build_tuple",
    ):
        assert not list(known_calls(function, operation)), operation
    # Indexing is evaluated before membership and can fail. Its result also
    # feeds ord(), so folding cannot discard the runtime record scan.
    assert list(known_calls(function, "%getitem"))
    check_ir1_consistency(compilation.components)


@pytest.mark.integration
def test_empty_membership_runtime_filter_has_no_membership_or_container_work(tmp_path):
    source = "\n".join(_filter_source(*case) for case in EMPTY_FILTERS)
    source += "\nrecords = input()\n"
    for name, _operator, _container in EMPTY_FILTERS:
        source += f"print(filter_{name}(records))\n"
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for records in ("", "alpha,beta", "aé🚀z"):
        case.compare(stdin=records + "\n")

    (tmp_path / "empty-membership.optimized.ll").write_text(case.optimized_llvm)
    for name, _operator, _container in EMPTY_FILTERS:
        # The dispatch wrapper also reaches a general body for non-str callers.
        # Check the actual str entry selected by the runtime-input calls, not
        # those required general fallback paths or argument-binding wrappers.
        symbol = rf"mule_fn_filter_{name}\.spec\.\d+"
        body = case.function_ir(rf"^{symbol}$")
        selected = bool(re.search(rf"\bcall\b[^\n]*@{symbol}\(", case.optimized_llvm))
        assert selected, f"{name}: specialized filter is not called"
        # Membership lowers to compiled contains/not_contains, and displays
        # lower to these sequence allocation primitives. Neither path belongs
        # in the optimized filter. No user helper hides either operation.
        calls = re.findall(r'\bcall\b[^\n]*?@(?:"([^"\n]+)"|([^\s(]+))\(', body)
        called_symbols = [quoted or plain for quoted, plain in calls]
        forbidden = [
            called
            for called in called_symbols
            if "contains" in called
            or "mule_sequence_list" in called
            or "mule_sequence_tuple" in called
        ]
        assert not forbidden, f"{name}: membership/container work remains: {forbidden}"
        # Retain the actual indexed scan and data-dependent accumulation, not
        # merely a folded return from an unused synthetic predicate.
        has_comparison = bool(re.search(r"\bicmp\b", body))
        has_branch = bool(re.search(r"\bbr i1\b", body))
        has_indexed_read = any("getitem" in called for called in called_symbols)
        assert has_comparison and has_branch, f"{name}: runtime scan control is absent"
        assert has_indexed_read, f"{name}: runtime indexed record read is absent"


def _boundary_source(name, operator, container):
    return f"""
def effect_{name}(value):
    print("evaluate", value)
    if value == "raise":
        raise ValueError("operand")
    return value

def call_{name}(value):
    return effect_{name}(value) {operator} {container}

def bound_{name}(condition, value):
    if condition:
        local = value
    return local {operator} {container}

def deleted_{name}(value):
    del value
    return value {operator} {container}

def global_{name}():
    return current_record {operator} {container}

def attribute_{name}(record):
    return record.value {operator} {container}

def index_{name}(records, position):
    return records[position] {operator} {container}

def dynamic_{name}(value, container):
    return value {operator} container

print("case", "{name}")
print(call_{name}(payload))
try:
    call_{name}("raise")
except ValueError:
    print("call error")
print(bound_{name}(True, payload))
try:
    bound_{name}(False, payload)
except UnboundLocalError:
    print("unbound")
try:
    deleted_{name}(payload)
except UnboundLocalError:
    print("deleted")
current_record = payload
print(global_{name}())
del current_record
try:
    global_{name}()
except NameError:
    print("global missing")
current_record = payload + " updated"
print(global_{name}())
print(attribute_{name}(Record(payload)))
try:
    attribute_{name}(Record("raise"))
except ValueError:
    print("attribute error")
print(index_{name}([payload], 0))
try:
    index_{name}([payload], 1)
except IndexError:
    print("index error")
values = []
print(dynamic_{name}(payload, values))
values.append(payload)
print(dynamic_{name}(payload, values))
print(dynamic_{name}(payload, Hook()))
"""


@pytest.mark.integration
def test_empty_membership_preserves_effects_errors_and_dynamic_fallback(tmp_path):
    source = """
class Record:
    def __init__(self, value):
        self._value = value

    @property
    def value(self):
        print("attribute", self._value)
        if self._value == "raise":
            raise ValueError("attribute")
        return self._value

class Hook:
    def __contains__(self, value):
        print("contains", value)
        return True

payload = input()
"""
    source += "\n".join(_boundary_source(*case) for case in EMPTY_FILTERS)
    case = build_case(tmp_path, source, optimize=3, lto=False)
    for payload in ("record", "é🚀"):
        case.compare(stdin=payload + "\n")
