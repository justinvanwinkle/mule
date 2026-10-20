"""Proven calls pass numeric values without replacing Python object entries."""

import re
import subprocess

import pytest

from mule.compiler import build, emit_llvm
from test.mule.compiler.test_exception_groups import compare
from test.python_host import python_host

FLOAT_CALL = """
def outer(n, label):
    def calculate(count, description, weight):
        total = 0.5
        for index in range(count):
            total += weight
        print(description)
        return total
    return calculate(n, label, 1.5) * 2.0
print(outer(0, 'zero'))
print(outer(3, 'three'))
"""

INTEGER_CALL = """
def outer(text, label):
    def clipped(count, description):
        print(description)
        return count & 127
    return clipped(int(text) & 255, label) + 1
print(outer('0', 'zero'))
print(outer('511', 'bounded'))
print(outer('1267650600228229401496703205887', 'large input'))
"""


def numeric_entries(llvm, function, returned):
    entries = re.findall(
        rf"^define internal {returned} "
        rf'(@"[^"\n]*{re.escape(function)}[^"\n]*\.numeric\.[^"\n]*")'
        r"\(([^\n]*)\) \{\n(.*?)^\}",
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    assert entries, f"{function} must have an internal {returned} entry"
    return entries


def test_mixed_object_body_returns_double_and_accepts_numeric_float_argument(tmp_path):
    llvm = emit_llvm(FLOAT_CALL)
    entries = numeric_entries(llvm, "calculate", "double")
    scalar_entries = [entry for entry in entries if "double %parameter.2" in entry[1]]
    assert scalar_entries, "the numeric-only weight parameter must be a double"
    for symbol, parameters, body in scalar_entries:
        assert "ptr %error.flag" in parameters
        assert "ptr %parameter.1" in parameters
        assert "fadd double" in body
        assert "call ptr @mule_box_f64" not in body
        assert "call i32 @mule_unbox_f64" not in body
        assert "@mule.box.f64(" not in body
        assert "@mule.box.f64.load(" not in body
        assert f"call double {symbol}(" in llvm
    assert re.search(
        r'^define internal ptr @"[^"\n]*calculate[^"\n]*\.body"\(', llvm, re.MULTILINE
    )
    compare(tmp_path, FLOAT_CALL)


def test_mixed_object_body_uses_proven_fixnum_argument_and_return_width(tmp_path):
    llvm = emit_llvm(INTEGER_CALL)
    entries = numeric_entries(llvm, "clipped", "i64")
    scalar_entries = [entry for entry in entries if "i64 %parameter.0" in entry[1]]
    assert scalar_entries, "the masked argument must use its proven machine width"
    for symbol, parameters, _body in scalar_entries:
        assert "ptr %error.flag" in parameters
        assert "ptr %parameter.1" in parameters
        assert f"call i64 {symbol}(" in llvm
    compare(tmp_path, INTEGER_CALL)


def test_identity_bearing_float_argument_stays_object_in_numeric_entry(tmp_path):
    source = """
def outer(text):
    original = float(text)
    saved = []
    def remember(value, output):
        output.append(value)
        return value
    result = remember(original, saved) * 2.0
    print(saved[0] is original)
    return result
print(outer('0.5'))
print(outer('-0.0'))
print(outer('inf'))
print(outer('nan'))
"""
    llvm = emit_llvm(source)
    entries = numeric_entries(llvm, "remember", "double")
    for _symbol, parameters, _body in entries:
        assert "ptr %parameter.0" in parameters
        assert "ptr %parameter.1" in parameters
    compare(tmp_path, source)


def test_numeric_call_keeps_actual_captured_environment(tmp_path):
    source = """
def outer(text, n, label):
    weight = float(text)
    def calculate(count, description):
        total = 0.5
        for index in range(count):
            total += weight
        print(description)
        return total
    return calculate(n, label) * 2.0
print(outer('1.5', 0, 'empty'))
print(outer('1.5', 2, 'first'))
print(outer('2.5', 2, 'second'))
"""
    llvm = emit_llvm(source)
    entries = numeric_entries(llvm, "calculate", "double")
    assert any("ptr %closure" in parameters for _symbol, parameters, _body in entries)
    compare(tmp_path, source)


def test_numeric_call_propagates_exception_and_preserves_failed_update(tmp_path):
    source = """
def outer(text, label):
    divisor = float(text)
    def divide(value, denominator, description):
        print(description)
        previous = value
        try:
            value /= denominator
        except ZeroDivisionError:
            print(value == previous)
            raise
        return value
    try:
        print(divide(3.0, divisor, label) * 2.0)
    except ZeroDivisionError:
        print('caller caught')
outer('2.0', 'success')
outer('0.0', 'positive zero')
outer('-0.0', 'negative zero')
"""
    llvm = emit_llvm(source)
    numeric_entries(llvm, "divide", "double")
    compare(tmp_path, source)


def test_object_callers_keep_numeric_identity_precision_and_boolean_results(tmp_path):
    compare(
        tmp_path,
        """
def identities(n):
    def literal():
        return 0.5
    def identity(value):
        return value
    def calculate(count):
        total = 0.5
        for index in range(count):
            total += 1.5
        return total
    original = float('0.5')
    saved = [original]
    print(literal() is literal(), identity(original) is saved[0])
    print(calculate(0) is calculate(0), calculate(1) is calculate(1))
    print(identity(original) + 1.0, literal() + 1.0, calculate(n) * 2.0)
identities(0)
identities(3)
def unbounded(text):
    def identity(value):
        return value
    value = int(text)
    result = identity(value)
    print(result == value, result + 1)
    print(identity(True) is True, identity(False) is False)
unbounded('1267650600228229401496703205376')
unbounded('-1267650600228229401496703205377')
""",
    )


def test_bool_arguments_preserve_identity_at_numeric_call_boundaries(tmp_path):
    source = """
def outer(value, label):
    def clipped(number, description):
        print(description, number is True, number is False)
        return (number & 127) + 1
    return clipped(value, label) + 1
print(outer(5, 'integer'))
print(outer(True, 'true'))
print(outer(False, 'false'))
"""
    compare(tmp_path, source)


@pytest.mark.integration
def test_numeric_calls_execute_with_optimized_mixed_bodies_and_finally(tmp_path):
    source = (
        FLOAT_CALL
        + INTEGER_CALL
        + """
def checked_ratio(text):
    def divide(value, denominator):
        try:
            return value / denominator
        finally:
            print('inner finally')
    try:
        print(divide(3.0, float(text)) * 2.0)
    except ZeroDivisionError:
        print('caught')
    finally:
        print('outer finally')
checked_ratio('2.0')
checked_ratio('0.0')
"""
    )
    source_path = tmp_path / "program.py"
    source_path.write_text(source)
    oracle = subprocess.run(
        [python_host(), source_path],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    assert oracle.returncode == 0, oracle.stderr
    executable = build(
        source_path, tmp_path / "program", optimize=3, lto=False, workers=1
    )
    source_path.unlink()
    native = subprocess.run(
        [executable], check=False, capture_output=True, text=True, timeout=10
    )
    assert native.returncode == 0, native.stderr
    assert native.stdout == oracle.stdout
