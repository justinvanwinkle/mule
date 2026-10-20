"""Successful tagged arithmetic bypasses fallible runtime-result handling."""

import re
import subprocess

import pytest

from mule.compiler import build, emit_llvm
from mule.compiler.driver import Toolchain


@pytest.mark.parametrize("operator", ["+", "-"])
def test_fixnum_success_reaches_return_without_null_check(tmp_path, operator):
    source = tmp_path / "arithmetic.ll"
    source.write_text(emit_llvm(f"def arithmetic(x, y): return x {operator} y\n"))
    optimized = subprocess.run(
        [
            Toolchain().llvm_tool("opt"),
            "-passes=always-inline,function(sroa,instcombine,simplifycfg)",
            "-S",
            source,
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    ).stdout
    body = re.search(
        r'^define[^\n]*@"?mule_fn_arithmetic.spec.0"?\([^\n]*\)[^\n]*\{\n(.*?)^\}',
        optimized,
        re.MULTILINE | re.DOTALL,
    )
    assert body is not None
    blocks = dict(
        re.findall(
            r"^([\w.]+):[^\n]*\n(.*?)(?=^[\w.]+:|\Z)",
            body[1],
            re.MULTILINE | re.DOTALL,
        )
    )
    arithmetic = next(
        block for block in blocks.values() if ".with.overflow.i64" in block
    )
    edge = re.search(r"br i1 %[^,]+, label %([\w.]+), label %([\w.]+)", arithmetic)
    assert edge is not None
    current = edge[2]
    visited = set()
    while current not in visited:
        visited.add(current)
        block = blocks[current]
        assert not re.search(r"icmp (?:eq|ne) ptr .*null", block), block
        if re.search(r"\bret ptr\b", block):
            break
        following = re.search(r"br label %([\w.]+)", block)
        assert following is not None, block
        current = following[1]
    else:
        pytest.fail("successful arithmetic never returns")


def test_optimized_add_sub_boundaries_and_failure_cleanup(tmp_path):
    values = [-(2**62) - 1, -(2**62), -1, 0, 1, 2**62 - 1, 2**62]
    source = tmp_path / "arithmetic.py"
    source.write_text(
        f"""def add(x, y): return x + y
def sub(x, y): return x - y
def inplace_add(x, y):
    x += y
    return x
def inplace_sub(x, y):
    x -= y
    return x
values = {values!r}
for left in values:
    for right in values:
        print(add(left, right), sub(left, right))
for function in (add, sub):
    try:
        function([], 1)
    except TypeError:
        print('caught')
print(add('a', 'b'), add(True, False), sub(True, False))
shared = [1]
print(inplace_add(shared, [2]) is shared, shared)
print(inplace_add(4611686018427387903, 1))
print(inplace_sub(-4611686018427387904, 1))
"""
    )
    executable = build(source, tmp_path / "arithmetic", optimize=1, workers=1)
    result = subprocess.run(
        [executable], capture_output=True, text=True, check=True, timeout=10
    )
    expected = "".join(
        f"{left + right} {left - right}\n" for left in values for right in values
    )
    assert result.stdout == (
        expected
        + "caught\ncaught\nab 1 1\nTrue [1, 2]\n"
        + "4611686018427387904\n-4611686018427387905\n"
    )
