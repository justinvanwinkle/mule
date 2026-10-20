"""Rejected entry predicates stay relational and local to their invocation."""

import re
import subprocess

import pytest

from mule.compiler import emit_llvm
from mule.compiler.ctype import (
    BOOLEAN,
    FIXNUM_MAX,
    FIXNUM_MIN,
    FLOAT,
    TEXT_TYPE,
    UNKNOWN,
    IntegerType,
)
from mule.compiler.driver import Toolchain
from mule.compiler.ltn import FixnumGuard
from test.mule.compiler.test_native import run_native


def test_fixnum_guard_preserves_only_constrained_parameter_positions():
    fixnum = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    guard = FixnumGuard.from_parameters((UNKNOWN, fixnum, UNKNOWN, fixnum))
    assert guard is not None
    assert guard.parameter_indices == (1, 3)


@pytest.mark.parametrize(
    "other",
    [
        BOOLEAN,
        FLOAT,
        TEXT_TYPE,
        IntegerType(0, FIXNUM_MAX),
        IntegerType(FIXNUM_MIN, 0),
        IntegerType(FIXNUM_MIN, FIXNUM_MAX, may_be_unbound=True),
    ],
)
def test_fixnum_guard_does_not_discard_another_required_condition(other):
    fixnum = IntegerType(FIXNUM_MIN, FIXNUM_MAX)
    assert FixnumGuard.from_parameters((fixnum, other)) is None


@pytest.mark.parametrize("parameters", [(), (UNKNOWN,), (UNKNOWN, UNKNOWN)])
def test_fixnum_guard_requires_a_tested_parameter(parameters):
    assert FixnumGuard.from_parameters(parameters) is None


@pytest.mark.parametrize(
    "body",
    ["return x + y", "left = x\n    right = y\n    return right + left"],
)
def test_rejected_fixnum_entry_eliminates_only_its_impossible_arithmetic(
    tmp_path, body
):
    source = tmp_path / "rejected.ll"
    llvm = emit_llvm(f"def add(x, y):\n    {body}\n")
    source.write_text(llvm)
    toolchain = Toolchain()
    output = tmp_path / "rejected.opt.ll"
    subprocess.run(
        [
            toolchain.clang,
            *toolchain.compile_flags(),
            "-Wno-override-module",
            "-S",
            "-emit-llvm",
            source,
            "-o",
            output,
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    # The ordinary object build uses this Clang optimization pipeline. Inspect
    # the canonical body before LLVM can remove it as unused in this module.
    for suffix, keeps_fixnum_path, module in (
        ("body", True, llvm),
        ("body.fallback.fixnum", False, output.read_text()),
    ):
        body = re.search(
            rf'^define[^\n]*@"?mule_fn_add\.{re.escape(suffix)}"?\([^\n]*\)[^\n]*\{{\n(.*?)^\}}',
            module,
            re.MULTILINE | re.DOTALL,
        )
        assert body is not None
        assert "@mule_binary_reuse" in body[1]
        assert ("@llvm.sadd.with.overflow" in body[1]) is keeps_fixnum_path


def test_rejected_guard_preserves_mixed_operands_and_reassigned_locals(tmp_path):
    result = run_native(
        tmp_path,
        """def add(x, y):
    return x + y
alias = add
print(alias(2, 3), alias(2, 0.5), alias(True, 4), alias(2 ** 70, 1), alias('a', 'b'))
def replace_right(x, y):
    previous = x + y
    y = 1
    return x + y
print(replace_right(2, 0.5), replace_right(2.5, 1))
def three(x, y, z):
    return x + y + z
print(three(1, 2, 0.5))
def replace_in_loop(x, y):
    result = x + y
    for index in (1, 2):
        y = index
        result = x + y
    return result
print(replace_in_loop(2, 0.5))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "5 2.5 5 1180591620717411303425 ab\n3 3.5\n3.5\n4\n"
