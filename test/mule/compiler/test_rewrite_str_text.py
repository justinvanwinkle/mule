"""Exact-text str rewrites through inferred local and observed call facts."""

import re

import pytest

from mule.compiler.debug import check_ir1_consistency
from mule.compiler.main import compile_source
from mule.compiler.node import LambdaVar, Ref
from test.mule.compiler.rewrite_support import build_case

TEXT_RECORDS = """
def observed_text(value):
    return str(value)

def record(raw):
    def closed_text(value):
        return str(value)

    text = f"record<{raw}>"
    closed = closed_text(text)
    observed = observed_text(text)
    print(closed is text, observed is text, type(observed) is type(text))
    saved = [closed, observed]
    text = "replacement"
    print(saved[0], saved[1], text)
    return saved

saved = record(input())
print(saved[0] is saved[1], saved[0])
"""


def test_str_text_closed_call_feedback_returns_the_actual_argument():
    compilation = compile_source(TEXT_RECORDS)
    closed = next(
        function
        for function in compilation.lambdas
        if function.source_name == "closed_text"
    )
    result = closed.return_.result.use()
    assert isinstance(result, Ref)
    assert isinstance(result.leaf, LambdaVar)
    assert result.leaf is closed.vars[0]
    check_ir1_consistency(compilation.components)


def _assert_identity_result(body, argument, result):
    """Successful returns preserve the argument; failure returns may be null."""
    phis = dict(re.findall(r"(%[\w.]+) = phi ptr ([^\n]+)", body))

    def origins(value):
        if value in (argument, "null"):
            return {value}
        assert value in phis, (value, body)
        incoming = re.findall(r"\[\s*([^,]+),", phis[value])
        assert incoming, phis[value]
        return set().union(*(origins(item.strip()) for item in incoming))

    assert argument in origins(result)
    # Safepoints, stack frames, traceback reporting, and ownership remain valid.
    # No call may hide conversion in another helper, including on the error arm.
    calls = re.findall(r'\bcall\b[^\n]*?@(?:"([^"\n]+)"|([^\s(]+))\(', body)
    targets = [quoted or plain for quoted, plain in calls]
    assert all(
        target
        in {
            "mule_retain",
            "mule_release",
            "mule_destroy",
            "mule_frame_head_slot",
            "mule_safepoint",
            "mule_traceback_here",
        }
        or target.startswith("llvm.lifetime.")
        for target in targets
    ), targets


def _assert_identity_entry(body):
    header = body.splitlines()[0]
    parameters = re.findall(r"ptr(?:\s+\w+)*\s+(%[\w.]+)", header)
    assert parameters, header
    returned = re.findall(r"\bret ptr ([%\w.]+)", body)
    assert returned, body
    for result in returned:
        _assert_identity_result(body, parameters[-1], result)


@pytest.mark.integration
def test_str_text_runtime_records_remove_conversion_in_specialized_entries(tmp_path):
    case = build_case(tmp_path, TEXT_RECORDS, optimize=3, lto=False)
    # The general public entry must remain available for non-text callers.
    general = case.function_ir(r"^mule_fn_observed_text\.body$", optimized=False)
    assert "mule.call.1" in general
    specialized = case.function_ir(
        r"^mule_fn_observed_text\.spec\.\d+$", optimized=False
    )
    assert "mule.call.1" not in specialized
    assert "mule_get_global" not in specialized

    for raw in ("", "mutable-looking row 17", "λ雪🙂"):
        case.compare(raw + "\n")


def _original_binding_blocks(body):
    """Follow the known function arm, while retaining ordinary error branches."""
    blocks = {}
    current = None
    for line in body.splitlines()[1:]:
        label = re.match(r"^([\w.]+):", line)
        if label:
            current = label[1]
            blocks[current] = []
        elif current is not None:
            blocks[current].append(line)
    guards = set(
        re.findall(
            r"(%[\w.]+) = call i32 @mule_function_matches\([^\n]*"
            r"@mule_fn_observed_text\.fn[^\n]*",
            body,
        )
    )
    matched = {
        result: comparison == "ne"
        for result, comparison, guard in re.findall(
            r"(%[\w.]+) = icmp (eq|ne) i32 (%[\w.]+), 0", body
        )
        if guard in guards
    }
    pending = [next(iter(blocks))]
    reachable = set()
    while pending:
        label = pending.pop()
        if label in reachable:
            continue
        reachable.add(label)
        block = "\n".join(blocks[label])
        branch = re.search(r"br i1 (%[\w.]+), label %([\w.]+), label %([\w.]+)", block)
        if branch and branch[1] in matched:
            pending.append(branch[2] if matched[branch[1]] else branch[3])
        else:
            pending.extend(re.findall(r"label %([\w.]+)", block))
    return blocks, reachable


def _assert_record_returns_original_text(case):
    body = case.function_ir(r"^mule_fn_record\.body$")
    blocks, reachable = _original_binding_blocks(body)
    definitions = dict(re.findall(r"^\s*(%[\w.]+) = ([^\n]+)", body, re.MULTILINE))

    def origins(value):
        if value == "null":
            return set()
        instruction = definitions[value]
        if instruction.startswith("phi ptr "):
            incoming = re.findall(r"\[\s*([^,]+), %([\w.]+)\s*\]", instruction)
            return set().union(
                *(
                    origins(item.strip())
                    for item, label in incoming
                    if label in reachable
                )
            )
        call = re.search(r'\bcall\b[^@]*@(?:"([^"\n]+)"|([^\s(]+))\(', instruction)
        assert call, (value, instruction)
        symbol = call[1] or call[2]
        if symbol == "mule_format_format_value.body":
            # This is the runtime field formatting, before str(text).
            assert re.search(r"\(ptr [^,]+, ptr %parameter\.0,", instruction)
            return {value}
        assert re.search(r"closed_text\.body$|observed_text\.spec\.\d+$", symbol), (
            value,
            instruction,
        )
        # A surviving identity helper must itself return its input, with no
        # conversion call hidden inside. Fully inlined identities need no helper.
        _assert_identity_entry(case.function_ir(rf"^{re.escape(symbol)}$"))
        arguments = re.findall(r"ptr(?:\s+\w+)*\s+(%[\w.]+)", instruction)
        return origins(arguments[-1])

    returned = [
        value
        for label in reachable
        for line in blocks[label]
        for value in re.findall(r"\bret ptr ([%\w.]+)", line)
    ]
    assert returned, body
    roots = set().union(*(origins(value) for value in returned))
    assert roots, body
    assert re.search(
        r"call[^\n]*@mule_format_format_value\.body\(ptr [^,]+, ptr %parameter\.0,",
        body,
    ), body
    original_path = "\n".join("\n".join(blocks[label]) for label in reachable)
    assert not re.search(
        r"\bcall[^\n]*@(?:mule_call_owned|\"?mule\.call\.)", original_path
    )


@pytest.mark.integration
@pytest.mark.parametrize("helper", ["closed", "observed"])
def test_str_text_record_return_preserves_identity_without_conversion(tmp_path, helper):
    nested = (
        """    def closed_text(value):
        return str(value)
"""
        if helper == "closed"
        else ""
    )
    source = f"""
def observed_text(value):
    return str(value)

def record(raw):
{nested}    text = f"{{raw:>12}}"
    return {helper}_text(text)

print(record(input()))
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    _assert_record_returns_original_text(case)
    for raw in ("row 19", "λ雪🙂"):
        case.compare(raw + "\n")


STR_BOUNDARIES = """
events = []

class Rendered:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        events.append("render:" + self.value)
        return self.value

    def __del__(self):
        events.append("released:" + self.value)

class Broken:
    def __str__(self):
        events.append("broken-str")
        raise ValueError("conversion failed")

class Invalid:
    def __str__(self):
        return 7

def convert(value):
    return str(value)

def supplied(value):
    events.append("argument")
    return value

def consume(value):
    events.append("consume:" + value)
    return value

raw = input()
text = f"<{raw}>"
print(convert(text) is text)
for value in (True, None, 17, b"row"):
    print(convert(value))
result = consume(convert(supplied(Rendered(text))))
print(result, events)
events.clear()
try:
    consume(convert(supplied(Broken())))
except ValueError:
    print("value-error", events)
try:
    convert(Invalid())
except TypeError:
    print("type-error")
print(result)
"""


@pytest.mark.integration
def test_str_text_near_misses_preserve_identity_types_effects_and_errors(tmp_path):
    case = build_case(tmp_path, STR_BOUNDARIES, optimize=3, lto=False)
    for raw in ("payload", "λ雪"):
        case.compare(raw + "\n")


@pytest.mark.integration
def test_str_text_subclass_conversion_preserves_override_and_exact_result_type(
    tmp_path,
):
    source = """
events = []

class Label(str):
    def __str__(self):
        events.append("label-str")
        return "label-result"

class PlainLabel(str):
    pass

def convert(value):
    return str(value)

raw = input()
text = f"<{raw}>"
print(convert(text) is text)
for value in (Label(raw), PlainLabel(raw)):
    converted = convert(value)
    print(converted, converted is value, type(converted) is str)
print(events)
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare("payload\n")


@pytest.mark.integration
def test_str_text_rebound_builtin_is_not_an_identity_conversion(tmp_path):
    source = """
events = []

def convert(raw):
    text = f"<{raw}>"
    return str(text)

def replacement(value):
    events.append(value)
    return "changed:" + value

raw = input()
print(convert(raw))
str = replacement
print(convert(raw), events)
"""
    case = build_case(tmp_path, source, optimize=3, lto=False)
    case.compare("runtime row\n")
