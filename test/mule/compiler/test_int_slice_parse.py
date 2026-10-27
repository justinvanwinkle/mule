"""Tests for int(text) and int(text[start:stop]) slice-parse fusion."""

from mule.compiler import emit_llvm
from test.mule.compiler.test_native import run_native


def _function_body(ir: str, name: str) -> str:
    lines = ir.splitlines()
    for index, line in enumerate(lines):
        if line.startswith(f'define internal ptr @"{name}"('):
            end = lines.index("}", index)
            return "\n".join(lines[index : end + 1])
    raise AssertionError(f"missing function {name!r}")


def test_int_slice_parse_fuses_in_specialized_text_body():
    source = """def parse_span(text, start, stop):
    return int(text[start:stop])

print(parse_span("abc-12345xyz", 3, 9))
"""
    ir = emit_llvm(source, filename="int_slice_parse.py")
    spec_body = _function_body(ir, "mule_fn_parse_span.spec.0")
    assert "call ptr @mule_parse_int_slice(" in spec_body
    assert "@mule.fast.getslice(" not in spec_body
    assert "@mule_getslice(" not in spec_body
    assert "@mule.call.1(" not in spec_body


def test_int_text_parse_direct_helper_when_argument_is_proven_text():
    source = """def parse_whole(x):
    s = str(x)
    return int(s)

print(parse_whole("  42  "))
"""
    ir = emit_llvm(source, filename="int_text_parse.py")
    body = _function_body(ir, "mule_fn_parse_whole.body")
    assert "call ptr @mule_parse_int_text(" in body
    assert "cell:__main__:int" not in body


def test_int_slice_parse_skipped_when_int_rebound_or_step_given():
    source = """int = lambda x: 99

def parse_rebound(text, start, stop):
    return int(text[start:stop])

print(parse_rebound("12345", 1, 4))
"""
    ir = emit_llvm(source, filename="int_rebound.py")
    spec_body = _function_body(ir, "mule_fn_parse_rebound.spec.0")
    assert "@mule_parse_int_slice(" not in spec_body
    assert "@mule_parse_int_text(" not in spec_body


def test_int_slice_and_text_parse_native_semantics(tmp_path):
    source = """def parse_span(text, start, stop):
    return int(text[start:stop])

def parse_open_start(text, stop):
    return int(text[:stop])

def parse_open_stop(text, start):
    return int(text[start:])

def parse_full_slice(text):
    return int(text[:])

def parse_direct(text):
    return int(text)

s = "xx  -9876543210  yy"
print(parse_span(s, 2, 17))
print(parse_span("123456", -5, -1))
print(parse_open_start("  42abc", 4))
print(parse_open_stop("abc -77 ", 3))
print(parse_full_slice("  +1_000_000  "))
print(parse_direct("  -000123  "))
big = "9" * 40
print(parse_span("aa" + big + "bb", 2, 42) == int(big))
print(parse_direct(big) == int(big))

for bad_args in (
    ("123", 2, 1),
    ("123", 1, 1),
    ("   ", 0, 3),
    ("12_3_", 0, 5),
    ("_123", 0, 4),
    ("12__3", 0, 5),
    ("+ 1", 0, 3),
    ("12a3", 0, 4),
):
    try:
        parse_span(bad_args[0], bad_args[1], bad_args[2])
        print("unexpected-ok")
    except ValueError:
        print("ValueError")
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "-9876543210",
        "2345",
        "42",
        "-77",
        "1000000",
        "-123",
        "True",
        "True",
        "ValueError",
        "ValueError",
        "ValueError",
        "ValueError",
        "ValueError",
        "ValueError",
        "ValueError",
        "ValueError",
    ]


def test_checked_int_parsing_and_range_preserve_python_behavior(tmp_path):
    source = """from mule_types import optimize

@optimize(safety=3)
def checked(text):
    value = int(text)
    return [index for index in range(value, value + 5, 2)]

print(checked("-2"))
try:
    checked("invalid")
except ValueError:
    print("invalid")
"""
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == ["[-2, 0, 2]", "invalid"]
