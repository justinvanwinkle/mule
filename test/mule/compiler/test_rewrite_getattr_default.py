"""Constant-name default lookup through normal record-processing programs."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from test.mule.compiler.rewrite_support import build_case

RECORD_LOOKUP = """
class Record:
    def __init__(self, region):
        self.region = region

class LegacyRecord:
    def __init__(self, label):
        self.label = label

def preferred_region(record, fallback):
    return getattr(record, "region", fallback)

def runtime_region(record, name, fallback):
    return getattr(record, name, fallback)

def unusual_region(record, fallback):
    return getattr(record, "not-an-identifier", fallback)

def shadow_region(record, fallback, getattr):
    return getattr(record, "region", fallback)

def replacement(record, name, fallback):
    print("replacement", name)
    return fallback

fallback = [input()]
record = Record(input())
legacy = LegacyRecord(input())
for attempt in range(3):
    print(preferred_region(record, fallback))
    print(preferred_region(legacy, fallback) is fallback)
print(runtime_region(record, input(), fallback))
print(unusual_region(record, fallback) is fallback)
print(shadow_region(record, fallback, replacement) is fallback)

# The same lookup site must observe an instance dictionary created after a miss.
legacy.region = input()
print(preferred_region(legacy, fallback))
del legacy.region
print(preferred_region(legacy, fallback) is fallback)

# Class mutation must invalidate a cached absence as well.
LegacyRecord.region = input()
print(preferred_region(legacy, fallback))
del LegacyRecord.region
print(preferred_region(legacy, fallback) is fallback)
"""


DESCRIPTOR_LOOKUP = """
events = []
marker = [input()]

class Present:
    def __init__(self, value):
        self.value = value

    @property
    def region(self):
        events.append("read")
        return self.value

class Missing:
    @property
    def region(self):
        events.append("missing")
        raise AttributeError("descriptor has no region")

class Broken:
    @property
    def region(self):
        events.append("broken")
        raise ValueError("descriptor failed")

class Recovered:
    def __init__(self, value):
        self.value = value

    @property
    def region(self):
        events.append("missing")
        raise AttributeError("use the attribute hook")

    def __getattr__(self, name):
        events.append("hook " + name)
        return self.value

def make_default():
    events.append("default")
    return marker

def descriptor_region(record):
    return getattr(record, "region", make_default())

def fail_default():
    events.append("failed default")
    raise RuntimeError("default failed")

def failing_default_region(record):
    return getattr(record, "region", fail_default())

present = Present(input())
recovered = Recovered(input())
for record in (present, Missing(), Broken(), recovered):
    for attempt in range(2):
        events = []
        try:
            value = descriptor_region(record)
            print(value is marker, value)
        except ValueError:
            print("ValueError")
        print(events)

events = []
try:
    failing_default_region(present)
except RuntimeError:
    print("RuntimeError", events)
"""


REBOUND_LOOKUP = """
def replacement(record, name, fallback):
    print("replacement", name, record)
    return fallback

getattr = replacement

def preferred_region(record, fallback):
    return getattr(record, "region", fallback)

fallback = [input()]
print(preferred_region(input(), fallback) is fallback)
"""


def _function(compilation, source_name):
    return next(fn for fn in compilation.lambdas if fn.source_name == source_name)


def test_constant_default_lookup_matches_without_injected_receiver_facts():
    compilation = compile_source(RECORD_LOOKUP)
    function = _function(compilation, "preferred_region")
    lookups = list(known_calls(function, "%getattr_default"))
    assert len(lookups) == 1
    assert lookups[0].info.name == "region"
    assert len(lookups[0].args) == 2
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("name", ["runtime_region", "unusual_region", "shadow_region"])
def test_dynamic_nonidentifier_and_shadowed_lookup_keep_general_semantics(name):
    compilation = compile_source(RECORD_LOOKUP)
    assert not list(known_calls(_function(compilation, name), "%getattr_default"))
    check_ir1_consistency(compilation.components)


def test_module_rebinding_does_not_select_builtin_default_lookup():
    compilation = compile_source(REBOUND_LOOKUP)
    function = _function(compilation, "preferred_region")
    assert not list(known_calls(function, "%getattr_default"))
    check_ir1_consistency(compilation.components)


@pytest.fixture(scope="module")
def record_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("default-getattr-records"), RECORD_LOOKUP)


@pytest.mark.integration
@pytest.mark.parametrize(
    "stdin",
    [
        "unknown\neu-west\nlegacy\nregion\nap-south\nus-east\n",
        "fallback\nhéllo🚀\nold\nmissing\nnew region\nclass region\n",
        "\n\nlegacy\nregion\n\n\n",
    ],
)
def test_default_lookup_preserves_runtime_records_and_cache_invalidation(
    record_case, stdin
):
    record_case.compare(stdin)


@pytest.mark.integration
def test_constant_default_lookup_removes_builtin_wrapper_from_optimized_path(
    record_case,
):
    body = record_case.function_ir(r"^mule_fn_preferred_region\.(?:body|spec\.\d+)$")
    assert "@mule_getattr_default(" in body
    assert "cell:__main__:getattr" not in body
    assert not re.search(r"@mule\.call\.[0-9]+\(", body)

    # Follow the selected Rust lookup implementation rather than crediting its name.
    # Descriptor invocation remains legitimate inside the ordinary lookup fallback.
    wrappers = re.findall(
        r'^define[^\n]*@"([^"\n]*fn_getattr[^"\n]*)"',
        record_case.builtins_llvm,
        re.MULTILINE,
    )
    assert wrappers, "the generic compiled getattr wrapper must exist in builtins"
    for symbol in (
        "mule_getattr_default",
        "mule_get_attr_cached",
        "mule_resolve_method",
    ):
        assembly = record_case.function_assembly(rf"^{symbol}$")
        for wrapper in wrappers:
            assert not re.search(rf"<{re.escape(wrapper)}(?:\+[^>]*)?>", assembly)
        assert not re.search(r"<mule\.call\.[0-9]+(?:\+[^>]*)?>", assembly)
    lookup = record_case.function_assembly(r"^mule_getattr_default$")
    assert re.search(r"\b(?:cmp|test)[a-z]*\s", lookup), lookup
    assert re.search(r"\bmov[a-z]*\s+[^\n]*\(", lookup), lookup


@pytest.mark.integration
def test_default_evaluation_and_descriptor_errors_match_python(tmp_path):
    case = build_case(tmp_path, DESCRIPTOR_LOOKUP)
    case.compare("marker\nrecord region\nrecovered region\n")
    body = case.function_ir(r"^mule_fn_descriptor_region\.(?:body|spec\.\d+)$")
    assert "@mule_getattr_default(" in body
    assert "cell:__main__:getattr" not in body


@pytest.mark.integration
def test_rebound_default_lookup_calls_the_replacement(tmp_path):
    case = build_case(tmp_path, REBOUND_LOOKUP)
    case.compare("fallback\nrecord\n")
    body = case.function_ir(r"^mule_fn_preferred_region\.(?:body|spec\.\d+)$")
    assert "@mule_getattr_default(" not in body
