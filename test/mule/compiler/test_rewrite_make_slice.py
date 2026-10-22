"""Exercise syntax-generated slices through scanning and protocol fallbacks."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.rewrite_support import build_case

SCANNER = """
def scan_fields(text):
    fields = []
    start = 0
    end = 0
    while end < len(text):
        if text[end] == ":":
            fields.append(text[start:end])
            start = end + 1
        end += 1
    fields.append(text[start:])
    return fields

text = input()
print(scan_fields(text))
"""


def test_make_slice_direct_syntax_fuses_before_runtime_lowering():
    llvm = emit_llvm(SCANNER)
    definitions = re.findall(
        r'^define[^\n]*@"?mule_fn_scan_fields[^\n]*\n.*?^}',
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert definitions
    body = "\n".join(definitions)
    assert "@mule.fast.getslice(" in body
    assert "make_slice" not in body


@pytest.mark.integration
def test_make_slice_scanner_avoids_temporary_slice_on_fast_path(tmp_path):
    case = build_case(tmp_path, SCANNER)
    for text in ("alpha:beta::gamma", "héllo:🚀:終", "", ":tail:"):
        case.compare(text + "\n")

    # Clang can inline the wrapper. Inspect both locations, not just its selection.
    path = case.function_ir(r"mule_fn_scan_fields|mule\.fast\.getslice")
    assert "@mule_fast_getslice(" in path
    fast_call = path.index("@mule_fast_getslice(")
    assert "make_slice" not in path[:fast_call]
    assert re.search(r"icmp (?:eq|ne) ptr [^\n]+, null", path[fast_call:])
    # The helper copies substring/container storage without constructing a
    # temporary slice and delegating to the generic item protocol.
    fast = case.function_assembly(r"^mule_fast_getslice$")
    assert re.search(r"\b(?:mov|cmp|test)\b", fast)
    assert re.search(r"text_slice_range|mule_sequence_list|mule_sequence_tuple", fast)
    assert not re.search(r"make_slice|allocate_instance|mule_resolve_method", fast)
    # Cold generic receivers and non-unit steps still need the slice protocol.
    assert "make_slice" in case.llvm


@pytest.mark.integration
def test_make_slice_bounds_steps_and_container_ownership_match_python(tmp_path):
    case = build_case(
        tmp_path,
        """
text = input()
start = int(input())
stop = int(input())
step = int(input())
print(text[start:stop])
print(text[start:stop:1])
try:
    print(text[start:stop:step])
except ValueError:
    print("zero step")
print(text[:])
print(text[None:None:None])
values = [[text], ["middle"], ["last"]]
copied = values[:]
print(copied is values, copied[0] is values[0])
values[0].append("changed")
del values
print(copied)
print((text, "middle", "last")[start:stop:step or 1])
print(b"abcdef"[start:stop:step or 1])
""",
    )
    for start, stop, step in ((1, 4, 1), (-8, 99, 2), (5, -9, -1), (3, 1, 0)):
        case.compare(f"hé🚀llo終\n{start}\n{stop}\n{step}\n")


@pytest.mark.integration
def test_make_slice_effectful_bounds_and_custom_receiver_preserve_order(tmp_path):
    case = build_case(
        tmp_path,
        """
events = []

class Bound:
    def __init__(self, label, value):
        self.label = label
        self.value = value

    def __index__(self):
        events.append(self.label)
        return self.value

class Receiver:
    def __getitem__(self, key):
        events.append("getitem")
        print(key.start, key.stop, key.step)
        return "selected"

def receiver(value):
    events.append("receiver")
    return value

def bound(label, value):
    events.append(label)
    return value

text = input()
print(receiver(text)[bound("start", 1):bound("stop", len(text)):bound("step", 1)])
print(events)
events.clear()
print(text[Bound("index-start", 1):Bound("index-stop", len(text)):Bound("index-step", 2)])
print(events)
events.clear()
print(receiver(Receiver())[bound("start", 1):bound("stop", 4):bound("step", -1)])
print(events)
events.clear()
try:
    print(receiver(text)[bound("start", "invalid"):bound("stop", 4):bound("step", 1)])
except TypeError:
    print("bad bound", events)
""",
    )
    case.compare("hé🚀llo終\n")


@pytest.mark.integration
def test_make_slice_shared_explicit_slice_retains_identity_and_effects(tmp_path):
    # A stored slice is a global constructor call, not the syntax-generated
    # single-use producer. Moving it across effects would change key identity.
    case = build_case(
        tmp_path,
        """
class Receiver:
    def __init__(self):
        self.previous = None

    def __getitem__(self, key):
        print("same key", key is self.previous)
        self.previous = key
        return key.start, key.stop, key.step

text = input()
key = slice(1, len(text), 2)
receiver = Receiver()
print(receiver[key])
print("between")
print(receiver[key])
print(text[key])
print(key.start, key.stop, key.step)
""",
    )
    case.compare("hé🚀llo終\n")
