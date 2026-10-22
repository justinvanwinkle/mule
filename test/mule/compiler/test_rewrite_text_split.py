"""Exact-text split selection and runtime behavior on delimited input records."""

import re

import pytest

from test.mule.compiler.rewrite_support import _checked_output, build_case

pytestmark = pytest.mark.integration


RECORD_PROGRAM = """
def split_record(record: str, separator: str, limit: int):
    return record.split(separator, limit)

def split_all(record: str, separator: str):
    return record.split(separator)

record = input()
separator = input()
limit = int(input())
try:
    print(split_record(record, separator, limit))
except (ValueError, TypeError, OverflowError) as error:
    print(type(error).__name__)
try:
    print(split_all(record, separator))
except (ValueError, TypeError, OverflowError) as error:
    print(type(error).__name__)
"""


NORMALIZED_PROGRAM = """
def normalized_fields(record):
    return f"{record}".split("::", 2)

fields = normalized_fields(input())
print(fields)
first = fields[0]
fields[0] = "replaced"
print(first, fields)
"""


FALLBACK_PROGRAM = """
class Separator(str):
    pass

class Override(str):
    def split(self, sep=None, maxsplit=-1):
        print("override", sep, maxsplit)
        return ["custom"]

class Limit:
    def __init__(self, value):
        self.value = value

    def __index__(self):
        print("index", self.value)
        return self.value

def whitespace(record: str, separator: str, limit: int):
    return record.split()

def none_separator(record: str, separator: str, limit: int):
    return record.split(None, limit)

def keyword_arguments(record: str, separator: str, limit: int):
    return record.split(sep=separator, maxsplit=limit)

def subclass_receiver(record: Override, separator: str, limit: int):
    return record.split(separator, limit)

def subclass_separator(record: str, separator: Separator, limit: int):
    return record.split(separator, limit)

def unknown_arguments(record, separator, limit):
    return record.split(separator, limit)

def index_limit(record: str, separator: str, limit: Limit):
    return record.split(separator, limit)

def floating_limit(record: str, separator: str):
    return record.split(separator, 1.5)

def mark(label, value):
    print(label)
    return value

record = input()
separator = input()
limit = int(input())
print(whitespace(record, separator, limit))
print(none_separator(record, separator, limit))
print(keyword_arguments(record, separator, limit))
print(subclass_receiver(Override(record), separator, limit))
print(subclass_separator(record, Separator(separator), limit))
print(unknown_arguments(record, separator, limit))
print(unknown_arguments(Override(record), separator, limit))
print(index_limit(record, separator, Limit(limit)))
try:
    print(floating_limit(record, separator))
except TypeError:
    print("floating limit TypeError")

# Arguments still run left to right, and maxsplit conversion precedes checking
# whether the separator is empty. These calls intentionally cannot specialize.
try:
    print(record.split(mark("empty separator", ""), mark("float limit", 1.5)))
except (ValueError, TypeError) as error:
    print(type(error).__name__)
try:
    print(record.split(mark("separator", separator), mark("index limit", Limit(limit))))
except (ValueError, TypeError) as error:
    print(type(error).__name__)
"""


@pytest.fixture(scope="module")
def record_case(tmp_path_factory):
    return build_case(tmp_path_factory.mktemp("rewrite-split-record"), RECORD_PROGRAM)


@pytest.fixture(scope="module")
def fallback_case(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("rewrite-split-fallback"), FALLBACK_PROGRAM
    )


@pytest.fixture(scope="module")
def normalized_case(tmp_path_factory):
    return build_case(
        tmp_path_factory.mktemp("rewrite-split-normalized"), NORMALIZED_PROGRAM
    )


@pytest.mark.parametrize(
    "record",
    [
        "a sufficiently long customer identifier::a sufficiently long status::tail::kept",
        "café::東京🚀::終",
        "::empty::",
        "no delimiter",
    ],
)
def test_split_normalized_record_preserves_fields_and_owned_results(
    normalized_case, record
):
    normalized_case.compare(record + "\n")


def _split_call_target(operands, symbols, relocations):
    # PIE GOT calls otherwise appear as _DYNAMIC offsets, hiding their target.
    if operands.startswith("*"):
        reference = re.search(r"# ([0-9a-f]+) <([^>]+)>", operands)
        if reference is None:
            return "unresolved indirect call"
        address = int(reference[1], 16)
        return symbols.get(relocations.get(address), reference[2])
    reference = re.match(r"[0-9a-f]+ <([^>]+)>", operands)
    return reference[1] if reference else "unresolved direct call"


def _assert_split_native_path(assembly, symbols, relocations, allowed_calls, events):
    """Require ordered work on one returning CFG path with only checked callees.

    Cold dispatch and error blocks remain legal outside that path. Every call
    on the selected path must resolve to a permitted callee, including GOT calls.
    Each non-primitive permitted helper receives its own path check below.
    """
    instructions = [
        (int(address, 16), op, operands)
        for address, op, operands in re.findall(
            r"^\s*([0-9a-f]+):\s+([a-z][a-z0-9]*)\s*(.*?)\s*$",
            assembly,
            re.MULTILINE,
        )
    ]
    assert instructions, assembly
    indices = {address: index for index, (address, _, _) in enumerate(instructions)}
    pending = [(0, 0)]
    visited = set()
    while pending:
        index, stage = pending.pop()
        if index >= len(instructions) or (index, stage) in visited:
            continue
        visited.add((index, stage))
        _, op, operands = instructions[index]
        work = f"{op} {operands}"
        if op.startswith("call"):
            target = _split_call_target(operands, symbols, relocations)
            if re.search(allowed_calls, target) is None:
                continue
            work = f"call {target}"
        if stage < len(events) and re.search(events[stage], work):
            stage += 1
        if op.startswith("ret"):
            if stage == len(events):
                return
            continue
        if op.startswith("j"):
            target = re.match(r"([0-9a-f]+)\s+<", operands)
            if target is not None and int(target[1], 16) in indices:
                destination = indices[int(target[1], 16)]
                branch_stage = stage
                if (
                    destination <= index
                    and stage < len(events)
                    and events[stage] == "backedge"
                ):
                    branch_stage += 1
                pending.append((destination, branch_stage))
            if op == "jmp":
                continue
        pending.append((index + 1, stage))
    raise AssertionError(f"No native returning path performs {events!r}:\n{assembly}")


def test_split_normalized_record_scans_and_constructs_results_without_dispatch(
    normalized_case,
):
    body = normalized_case.function_ir(
        r"mule_fn_normalized_fields\.(?:body|spec\.\d+)$"
    )
    assert re.search(r"\bcall\b[^\n]*@mule_text_split\(", body)
    assert "@mule_resolve_method(" not in body
    assert "@mule_call_resolved(" not in body

    symbols = {
        int(address, 16): name
        for address, name in re.findall(
            r"^([0-9a-f]+) <([^>]+)>:", normalized_case.assembly, re.MULTILINE
        )
    }
    relocations = {
        int(address, 16): int(target, 16)
        for address, target in re.findall(
            r"^([0-9a-f]+)\s+R_X86_64_RELATIVE\s+\*ABS\*\+0x([0-9a-f]+)",
            _checked_output(["objdump", "-R", str(normalized_case.executable)]),
            re.MULTILINE,
        )
    }
    # Check the multi-byte Latin1 arm used by the long input above. The native
    # search and substring copy are outlined, so selecting split alone is not
    # sufficient. Required list and substring allocations are permitted.
    allocate = r"___rust(?:_alloc$|_realloc$|_no_alloc_shim)"
    cases = [
        (
            r"^mule_text_split$",
            rf"{allocate}|Text4find|latin1_slice_value|new_untracked_value",
            [
                r"call .*Text4find",
                r"call .*latin1_slice_value",
                r"mov\w* %rax,\(%[a-z0-9]+,%[a-z0-9]+,8\)",
                r"call .*new_untracked_value",
            ],
        ),
        (
            r"Text4find",
            r"^bcmp@",
            [
                r"cmp\w* .*\(",
                r"call bcmp@",
                "backedge",
                r"call bcmp@",
                r"mov\w* \$0x1,%eax",
            ],
        ),
        (
            r"latin1_slice_value",
            rf"{allocate}|finish_grow|^memcpy@",
            [r"call .*finish_grow", r"call memcpy@", r"mov\w* .*?,[^,]*\(%rax\)"],
        ),
        (
            r"new_untracked_value",
            allocate,
            [r"call .*___rust_alloc$", r"mov\w* .*?,[^,]*\(%rax\)"],
        ),
        (
            r"RawVecInner11finish_grow",
            allocate,
            [r"call .*___rust_(?:alloc|realloc)$"],
        ),
    ]
    for pattern, allowed, events in cases:
        _assert_split_native_path(
            normalized_case.function_assembly(pattern),
            symbols,
            relocations,
            allowed,
            events,
        )


@pytest.mark.parametrize("function", ["split_record", "split_all"])
def test_split_record_matches_without_generic_method_resolution(record_case, function):
    # Inspect the actual O3 specialized entry, not only the compiler's emitted IR.
    # input() remains runtime data. Annotations alone must not stand in for proof
    # of the receiver, separator, and optional integer limit.
    body = record_case.function_ir(rf"mule_fn_{function}\.spec\.\d+$")
    assert re.search(r"\bcall\b[^\n]*@mule_text_split\(", body), (
        f"{function} retains generic method resolution instead of selecting "
        "mule_text_split for runtime text inputs"
    )
    assert "@mule_resolve_method(" not in body
    assert "@mule_call_resolved(" not in body


@pytest.mark.parametrize(
    ("record", "separator", "limit"),
    [
        ("name::status::detail", "::", 1),
        ("name::status::detail", "::", 0),
        ("name::status::detail", "::", -1),
        ("name::status::detail", "::", -7),
        (",a,,b,", ",", 20),
        ("", ",", 1),
        ("no separator here", ",", 2),
        ("café§status§é", "§", 1),
        ("東京🚀status🚀終", "🚀", 1),
        ("東京🚀status🚀終", "🚀", -1),
        ("left\x00right\x00", "\x00", -1),
        ("record", "", 0),
        ("record", "", -1),
        ("left:right", ":", 2**100),
        ("left:right", ":", -(2**100)),
        ("left:right", "", 2**100),
    ],
)
def test_split_runtime_records_match_python(record_case, record, separator, limit):
    record_case.compare(f"{record}\n{separator}\n{limit}\n")


def test_split_ineligible_calls_do_not_select_exact_text_helper(fallback_case):
    for function in (
        "whitespace",
        "none_separator",
        "keyword_arguments",
        "subclass_receiver",
        "subclass_separator",
        "index_limit",
        "floating_limit",
    ):
        body = fallback_case.function_ir(
            rf"mule_fn_{function}(?:\.|$)", optimized=False
        )
        assert "@mule_text_split(" not in body, function

    # An unknown public entry must retain dynamic lookup even if a separately
    # generated exact-text entry can specialize one of its ordinary callers.
    unknown = fallback_case.function_ir(
        r"mule_fn_unknown_arguments\.body$", optimized=False
    )
    assert "@mule_text_split(" not in unknown
    assert "@mule_resolve_method(" in unknown


@pytest.mark.parametrize(
    "stdin",
    [
        " first::second::tail \n::\n1\n",
        " left right  tail \n \n0\n",
        "東京🚀status🚀終\n🚀\n-1\n",
    ],
)
def test_split_fallbacks_preserve_overrides_conversion_and_error_order(
    fallback_case, stdin
):
    fallback_case.compare(stdin)
