"""Constant-name hasattr through record filtering and attribute lookup protocols."""

import re
import subprocess

import pytest

from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

LOOKUP_FORMS = """
def direct_probe(record):
    return hasattr(record, "available")

def dynamic_probe(record, name):
    return hasattr(record, name)

def nonidentifier_probe(record):
    return hasattr(record, "not-an-identifier")

def shadowed_probe(record, hasattr):
    return hasattr(record, "available")
"""


RECORD_SCAN = (
    """
events = []

class Observation:
    def __init__(self, state):
        self.state = state

    def __del__(self):
        events.append("release:" + self.state)

class Record:
    def __init__(self, state):
        self.state = state

    @property
    def available(self):
        events.append("read:" + self.state)
        if self.state == "missing":
            raise AttributeError("unavailable")
        if self.state == "broken":
            raise RuntimeError("descriptor failed")
        if self.state == "none":
            return None
        if self.state == "false":
            return False
        return Observation(self.state)

class RawRecord:
    def __init__(self, value):
        self.value = value

def field_probe(record):
    return hasattr(record, "value")

def optional_probe(record):
    return hasattr(record, "optional")

def replacement(record, name):
    events.append("replacement:" + name)
    return "replacement result"

def scan_records(states):
    found = 0
    missing = 0
    failed = 0
    for state in states:
        record = Record(state)
        try:
            if direct_probe(record):
                found += 1
            else:
                missing += 1
        except RuntimeError:
            failed += 1
    return found, missing, failed
"""
    + LOOKUP_FORMS
    + """

print(scan_records(input().split(",")))
raw = RawRecord(int(input()))
lookup_name = input()

# The same FIELD cache site must observe deletion and reassignment.
print(field_probe(raw), field_probe(raw))
del raw.value
print(field_probe(raw))
raw.value = 17
print(field_probe(raw))

# Class mutation must invalidate an ABSENT entry before any instance dict exists.
print(optional_probe(raw), optional_probe(raw))
RawRecord.optional = raw.value
print(optional_probe(raw))
del RawRecord.optional
print(optional_probe(raw))

# Creating the instance dictionary must invalidate the cached class-only miss.
attributes = raw.__dict__
attributes["optional"] = raw.value
print(optional_probe(raw))
del attributes["optional"]
print(optional_probe(raw))

attributes["not-an-identifier"] = raw.value
print(nonidentifier_probe(raw))
print(dynamic_probe(raw, lookup_name))
print(shadowed_probe(raw, replacement))
print(events)
"""
)


GLOBAL_SHADOW = """
events = []

class Record:
    def __init__(self, value):
        self.value = value

def replacement(record, name):
    events.append(name)
    return record.value + 1

def global_probe(record):
    return hasattr(record, "value")

hasattr = replacement
record = Record(int(input()))
print(global_probe(record))
print(events)
"""


def test_hasattr_constant_identifier_matches_without_rewriting_dynamic_or_shadowed_calls():
    compilation = compile_source(LOOKUP_FORMS)
    functions = {function.source_name: function for function in compilation.lambdas}
    direct = list(known_calls(functions["direct_probe"], "%hasattr"))
    assert len(direct) == 1
    assert direct[0].info.name == "available"
    assert len(direct[0].args) == 1
    for name in ("dynamic_probe", "nonidentifier_probe", "shadowed_probe"):
        assert not list(known_calls(functions[name], "%hasattr")), name

    shadowed = compile_source(GLOBAL_SHADOW)
    global_probe = next(
        function
        for function in shadowed.lambdas
        if function.source_name == "global_probe"
    )
    assert not list(known_calls(global_probe, "%hasattr"))


@pytest.fixture(scope="module")
def record_case(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("hasattr-records"), RECORD_SCAN, optimize=3, lto=False
    )


@pytest.mark.integration
@pytest.mark.parametrize(
    "stdin",
    [
        "ready,missing,broken,none,false,ready\n23\nvalue\n",
        "missing,missing,missing\n-19\noptional\n",
        "broken,ready,broken\n0\nnot-an-identifier\n",
        "none,false\n18446744073709551616\nabsent\n",
    ],
)
def test_hasattr_record_filter_preserves_descriptor_effects_errors_and_cache_changes(
    record_case, stdin
):
    # Observation.__del__ makes release of the discarded descriptor result visible.
    record_case.compare(stdin)


@pytest.mark.integration
def test_hasattr_optimized_record_probe_bypasses_builtin_lookup_and_call(record_case):
    direct = record_case.function_ir(r"fn_direct_probe(?:\.|$)")
    assert "@mule_hasattr(" in direct
    assert "cell:__main__:hasattr" not in direct
    assert "@mule.call.2(" not in direct

    # Follow the actual linked Rust implementation, not just its declaration.
    # It performs attribute lookup directly. Descriptor dispatch is required and
    # is not the generic builtin call that this rewrite removes.
    predicate = record_case.function_assembly(r"^mule_hasattr$")
    lookup = record_case.function_assembly(r"^mule_getattr_default$")
    resolved = record_case.function_assembly(r"^mule_get_attr_cached$")
    assert "mule_getattr_default" in _runtime_callees(record_case, predicate)
    assert "mule_get_attr_cached" in _runtime_callees(record_case, lookup)
    assert "mule_resolve_method" in _runtime_callees(record_case, resolved)
    for body in (predicate, lookup, resolved):
        for callee in _runtime_callees(record_case, body):
            assert not re.search(r"fn_hasattr(?:\.|$)", callee)
            assert not callee.startswith("mule_get_global")


def _runtime_callees(case, body):
    """Resolve direct and RIP/GOT calls in the linked x86-64 runtime body.

    Rust emits GOT-indirect calls even between these exported local helpers.
    The dynamic relocation identifies the function; the GOT label alone does not.
    """
    result = subprocess.run(
        ["readelf", "-rW", case.executable],
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    symbols = {
        int(address, 16): symbol
        for address, symbol in re.findall(
            r"^([0-9a-fA-F]+) <([^>]+)>:", case.assembly, re.MULTILINE
        )
    }
    relocations = {}
    for line in result.stdout.splitlines():
        fields = line.split()
        if len(fields) >= 4 and fields[2] == "R_X86_64_RELATIVE":
            address = int(fields[0], 16)
            target = int(fields[3], 16)
            if target in symbols:
                relocations[address] = symbols[target]
        elif len(fields) >= 5 and fields[2] == "R_X86_64_GLOB_DAT":
            relocations[int(fields[0], 16)] = fields[4]

    callees = set(re.findall(r"\b(?:callq?|jmpq?)\s+[0-9a-fA-F]+\s+<([^>]+)>", body))
    for slot in re.findall(r"\b(?:callq?|jmpq?)\s+\*[^#\n]+#\s+([0-9a-fA-F]+)\b", body):
        address = int(slot, 16)
        assert address in relocations, f"unresolved runtime call through GOT {slot}"
        callees.add(relocations[address])
    return callees


@pytest.mark.integration
def test_hasattr_dynamic_name_retains_general_lookup(record_case):
    dynamic = record_case.function_ir(r"fn_dynamic_probe(?:\.|$)")
    nonidentifier = record_case.function_ir(r"fn_nonidentifier_probe(?:\.|$)")
    shadowed = record_case.function_ir(r"fn_shadowed_probe(?:\.|$)")
    for body in (dynamic, nonidentifier, shadowed):
        assert "@mule_hasattr(" not in body


@pytest.mark.integration
def test_hasattr_global_rebinding_calls_replacement_with_original_arguments(tmp_path):
    case = build_case(tmp_path, GLOBAL_SHADOW, optimize=3, lto=False)
    for stdin in ("41\n", "-2\n"):
        case.compare(stdin)
    body = case.function_ir(r"fn_global_probe(?:\.|$)")
    assert "@mule_hasattr(" not in body
