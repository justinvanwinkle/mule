"""Normal-source formatting rewrites in runtime-dependent message assembly."""

import re

import pytest

from mule import ast
from mule.compiler.debug import check_ir1_consistency
from mule.compiler.ir1util import known_calls
from mule.compiler.main import compile_source
from mule.compiler.node import Constant, LambdaVar, Ref
from test.mule.compiler.rewrite_support import _llvm_functions, build_case


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        ("f'{42}'", "42"),
        ("f'{-17!s}'", "-17"),
        ("f'{42!r}'", "42"),
        ("f'{True}'", "True"),
        ("f'{False!r}'", "False"),
        ("f'{None!s}'", "None"),
        ("f'{\"ready\"}'", "ready"),
        ("f'{\"é\"!s}'", "é"),
    ],
)
def test_format_literal_tag_folds_before_runtime_message_assembly(expression, expected):
    compilation = compile_source(f"def message(raw):\n return {expression} + raw\n")
    fn = next(lam for lam in compilation.lambdas if lam.source_name == "message")
    assert not list(known_calls(fn, "%format"))
    additions = list(known_calls(fn, "%add"))
    assert additions, "Runtime message input must still be consumed"
    use = additions[0].args[0].use()
    assert isinstance(use, Ref)
    assert isinstance(use.leaf, Constant)
    assert isinstance(use.leaf.value, ast.TextLiteral)
    assert use.leaf.value.value == expected
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize("conversion", ["", "!s"])
def test_format_exact_text_reuses_message_input_without_injected_facts(conversion):
    compilation = compile_source(
        "def message_text(raw):\n text = 'ready' if raw else 'error'\n"
        f" return f'{{text{conversion}}}'\n"
    )
    fn = next(lam for lam in compilation.lambdas if lam.source_name == "message_text")
    assert not list(known_calls(fn, "%format"))
    use = fn.return_.result.use()
    assert isinstance(use, Ref)
    assert isinstance(use.leaf, LambdaVar)
    assert use.leaf.name == "text"
    check_ir1_consistency(compilation.components)


@pytest.mark.parametrize(
    ("parameters", "expression"),
    [
        ("text: str", "f'{text!r}'"),
        ("text: str", "f'{text!a}'"),
        ("text: str", "f'{text:>12}'"),
        ("value", "f'{value}'"),
        ("flag", "f'{text}'"),
        ("raw", "f'{\"é\"!r}'"),
        ("raw", "f'{\"é\"!a}'"),
        ("raw", f"f'{{{1 << 128}}}'"),
    ],
)
def test_format_near_misses_keep_protocol_in_normal_pipeline(parameters, expression):
    setup = " if flag:\n  text = 'ready'\n" if parameters == "flag" else ""
    compilation = compile_source(
        f"def message({parameters}):\n{setup} return {expression}\n"
    )
    fn = next(lam for lam in compilation.lambdas if lam.source_name == "message")
    assert list(known_calls(fn, "%format"))
    check_ir1_consistency(compilation.components)


_MESSAGES = """
def message_text(text: str):
    return f"{text}"

def message_string(text: str):
    return f"{text!s}"

def tagged_message(text: str):
    return f"{42}:{True!r}:{None!s}:{'ready'}:" + text

raw = input()
saved = []
for suffix in ("", "é", "next"):
    # The payload is newly constructed at runtime, not an interned literal.
    payload = f"{raw}{suffix}"
    plain = message_text(payload)
    converted = message_string(payload)
    print(plain is payload, converted is payload)
    saved.append(tagged_message(plain))
    del payload
print(saved)
"""


@pytest.mark.integration
def test_format_messages_remove_dispatch_and_intermediate_text_allocation(tmp_path):
    case = build_case(tmp_path, _MESSAGES, optimize=3, lto=False)
    for raw in ("event-17\n", "café 'quoted'\n", "\n"):
        case.compare(stdin=raw)

    definitions = dict(_llvm_functions(case.optimized_llvm))
    bookkeeping = {
        "mule_retain",
        "mule_release",
        "mule_destroy",
        "mule_error_pending",
        "mule_frame_head_slot",
        "mule_safepoint",
        "mule_traceback_here",
        "mule_enter_frame",
        "mule_leave_frame",
    }
    # Runtime identity observations above trace the returned value back to the
    # original payload. Follow the corresponding optimized call path as well:
    # LLVM may inline the body into the guarded wrapper or keep a separate body.
    for name in ("message_text", "message_string"):
        roots = [
            body
            for symbol, body in definitions.items()
            if symbol.startswith(f"mule.direct.mule_fn_{name}.")
        ]
        assert roots, f"No optimized call path for {name}"
        pending = []
        for root in roots:
            # The false callable-identity arm must retain dynamic invocation.
            # Inspect only the matching callable's successful frame-entry arm.
            entered = root.index("@mule_enter_frame(")
            leaving = root.index("@mule_leave_frame(", entered)
            first_line = root.rfind("\n", 0, entered) + 1
            last_line = root.rfind("\n", 0, leaving) + 1
            pending.append(root[first_line:last_line])
        visited = set()
        while pending:
            body = pending.pop()
            assert not re.search(r"mule_format_|mule_render_|mule_text_result", body), (
                body
            )
            call_lines = [
                line
                for line in body.splitlines()
                if re.search(r"\b(call|invoke)\b", line)
            ]
            assert all("@" in line for line in call_lines), call_lines
            calls = re.findall(r'\b(?:call|invoke)\b[^\n]*@"?([^"\s(]+)', body)
            for callee in calls:
                if callee in bookkeeping or callee.startswith("llvm."):
                    continue
                # Stack frame allocas are not intermediate text allocations.
                # Follow other compiled helpers instead of trusting their labels.
                assert not re.search(
                    r"alloc|mule_text_new|mule_literal_text", callee
                ), body
                assert callee in definitions, f"Unexamined helper: {callee}\n{body}"
                if callee not in visited:
                    visited.add(callee)
                    pending.append(definitions[callee])

    tag_body = case.function_ir(r"^mule_fn_tagged_message\.body$")
    assert "ret ptr" in tag_body
    assert not re.search(r"mule_format_|mule_render_|mule_text_result", tag_body)


_EFFECTS = """
events = []
raw = input()

class Field:
    def __init__(self, text):
        self.text = text

    def __str__(self):
        events.append("str")
        return self.text

    def __repr__(self):
        events.append("repr")
        return "é<" + self.text + ">"

    def __format__(self, spec):
        events.append("format:" + spec)
        return spec + "[" + self.text + "]"

def specification():
    events.append("spec")
    return "mark"

def next_field():
    events.append("next")
    return raw + "!"

def optional_message(flag):
    if flag:
        text = raw + "?"
    return f"{text}"

class BrokenString:
    def __str__(self):
        events.append("bad-str")
        return 12

class BrokenFormat:
    def __format__(self, spec):
        events.append("bad-format")
        raise ValueError("broken")

class BrokenRepr:
    def __repr__(self):
        events.append("bad-repr")
        return 12

class BrokenFormatResult:
    def __format__(self, spec):
        events.append("bad-format-result")
        return 12

class Styled(str):
    def __str__(self):
        events.append("subclass-str")
        return "string:" + raw

    def __format__(self, spec):
        events.append("subclass-format:" + spec)
        return "format:" + raw

print(f"{Field(raw)}:{next_field()}")
print(f"{Field(raw)!s}:{next_field()}")
print(f"{Field(raw)!r}")
print(f"{Field(raw)!a}")
print(f"{Field(raw):{specification()}}:{next_field()}")
print(f"{Styled(raw)}", f"{Styled(raw)!s}")
print(f"{raw!r}", f"{raw!a}", f"{raw:>12}")
for value in (True, None, 31, -2.5, b"wire"):
    print(f"{value}", f"{value!s}", f"{value!r}")
for flag in (True, False):
    try:
        print(optional_message(flag))
    except UnboundLocalError:
        print("unbound")
try:
    print(f"{BrokenString()!s}:{next_field()}")
except TypeError:
    print("str-type-error")
try:
    print(f"{BrokenRepr()!r}:{next_field()}")
except TypeError:
    print("repr-type-error")
try:
    print(f"{BrokenRepr()!a}:{next_field()}")
except TypeError:
    print("ascii-type-error")
try:
    print(f"{BrokenFormatResult()}:{next_field()}")
except TypeError:
    print("format-type-error")
try:
    print(f"{BrokenFormat()}:{next_field()}")
except ValueError:
    print("format-error")
print(events)
# Fresh successful conversions after errors exercise retained payload ownership.
print(f"{Field(raw)!s}", raw)
"""


@pytest.mark.integration
def test_format_fallback_preserves_conversion_order_errors_and_live_payload(tmp_path):
    case = build_case(tmp_path, _EFFECTS, optimize=3, lto=False)
    for raw in ("résumé\n", "'\\quoted'\n"):
        case.compare(stdin=raw)
