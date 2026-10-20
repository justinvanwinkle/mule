"""LLVM removes private leaf boxes while ordinary object lifetimes remain valid."""

import re
import subprocess

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import DECLARATIONS
from mule.compiler.driver import Toolchain, link_llvm
from mule.compiler.vop import FLOAT_BOX_LAYOUT
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_native import run_native


def function_body(llvm, symbol):
    match = re.search(
        rf"^define[^\n]*@{re.escape(symbol)}\([^\n]*\)[^\n]*\{{\n(.*?)^\}}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert match is not None, f"missing LLVM function {symbol}"
    return match.group(1)


@pytest.mark.integration
def test_private_box_roundtrip_eliminates_real_pool_calls_without_lto(tmp_path):
    layout = FLOAT_BOX_LAYOUT
    llvm = (
        DECLARATIONS
        + "\n"
        + layout.codec.helpers(layout)
        + f"""
declare double @strtod(ptr, ptr)
define double @roundtrip(double %input) noinline {{
entry:
  %box = call ptr @{layout.symbol()}(double %input)
  %value = call double @{layout.symbol("load")}(ptr %box)
  call void @{layout.symbol("private.release")}(ptr %box)
  ret double %value
}}
define i32 @main(i32 %argc, ptr %argv) {{
entry:
  %slot = getelementptr ptr, ptr %argv, i64 1
  %text = load ptr, ptr %slot
  %input = call double @strtod(ptr %text, ptr null)
  %actual = call double @roundtrip(double %input)
  %actual.bits = bitcast double %actual to i64
  %input.bits = bitcast double %input to i64
  %wrong = icmp ne i64 %actual.bits, %input.bits
  %status = zext i1 %wrong to i32
  ret i32 %status
}}
"""
    )
    source = tmp_path / "roundtrip.ll"
    source.write_text(llvm)
    optimized = tmp_path / "roundtrip.opt.ll"
    result = subprocess.run(
        [
            Toolchain().llvm_tool("opt"),
            "-passes=default<O3>",
            "-S",
            source,
            "-o",
            optimized,
        ],
        check=False,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr
    body = function_body(optimized.read_text(), "roundtrip")
    assert re.search(r"\bret double %input\b", body)
    assert not re.search(r"\b(?:call|alloca|load|store)\b", body)

    executable = link_llvm(llvm, tmp_path / "roundtrip", optimize=3, lto=False)
    for value in ("3.25", "-0.0", "inf", "nan"):
        result = subprocess.run(
            [executable, value], check=False, capture_output=True, text=True, timeout=10
        )
        assert result.returncode == 0, (value, result.stderr)
    assembly = subprocess.run(
        ["objdump", "-d", "--disassemble=roundtrip", executable],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    assert "<roundtrip>:" in assembly
    assert "\tcall" not in assembly
    assert "\tret" in assembly


def test_identity_observed_local_boxes_use_private_lifetimes(tmp_path):
    source = """
def private(text):
    value = float(text)
    first = value + 1.0
    second = value + 2.0
    alias = first
    print(first is alias, first is second)
    return first + second
print(private('2.5'))
print(private('-0.0'))
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_private.body"')
    assert f"call void @{FLOAT_BOX_LAYOUT.symbol('private.release')}(" in body
    compare(tmp_path, source)


def test_fresh_factory_result_transfers_private_ownership_to_caller(tmp_path):
    source = """
def outer(text):
    value = float(text)
    def make(argument):
        return argument + 1.0
    first = make(value)
    second = make(value)
    return first is second
print(outer('2.5'))
print(outer('inf'))
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_outer.body"')
    assert f"call void @{FLOAT_BOX_LAYOUT.symbol('private.release')}(" in body
    compare(tmp_path, source)


def test_container_global_and_closure_escapes_preserve_aliases(tmp_path):
    compare(
        tmp_path,
        """
saved_global = None
def escape(text):
    global saved_global
    value = float(text) + 1.0
    saved = [value]
    saved_global = value
    def captured():
        return value
    print(saved[0] is value, captured() is value, saved_global is value)
    del saved_global
    return saved, captured
saved, captured = escape('2.5')
print(saved[0] is captured(), saved[0] + 2.0)
""",
    )


def test_private_box_owners_are_released_on_early_exception_return(tmp_path):
    source = """
def check(text, denominator):
    value = float(text) + 1.0
    alias = value
    try:
        1.0 / float(denominator)
    except ZeroDivisionError:
        return False
    return value is alias
print(check('2.5', '2.0'))
print(check('2.5', '-0.0'))
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_check.body"')
    assert f"call void @{FLOAT_BOX_LAYOUT.symbol('private.release')}(" in body
    compare(tmp_path, source)


def test_physical_float_weak_handle_prevents_private_destruction(tmp_path):
    source = """
from _mule_native import weak_handle, weak_target
def check(text):
    value = float(text) + 1.0
    handle = weak_handle(value, None, None)
    print(weak_target(handle) is value)
    del value
    print(weak_target(handle) is None)
check('2.5')
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_check.body"')
    assert f"call void @{FLOAT_BOX_LAYOUT.symbol('private.release')}(" not in body
    result = run_native(tmp_path, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True\nTrue\n"


def test_private_aliases_survive_branches_errors_deletion_and_shared_values(tmp_path):
    compare(
        tmp_path,
        """
def exercise(text, denominator, choose_first):
    value = float(text)
    first = value + 1.0
    second = value + 2.0
    alias = first if choose_first else second
    shared = [value + 10.0]
    try:
        print(alias is first, alias is second, shared[0])
        print(first / float(denominator))
    except ZeroDivisionError:
        print('zero', alias is first, alias is second)
    del first
    print(alias + 0.5)
    del alias, second
exercise('3.25', '2.0', True)
exercise('3.25', '-0.0', False)
""",
    )
