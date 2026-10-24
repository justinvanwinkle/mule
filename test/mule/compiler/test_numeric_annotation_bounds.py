"""Annotation checks receive real numeric bounds across the native ABI."""

import re
import subprocess

from mule.compiler import emit_llvm
from mule.compiler.driver import Toolchain


def test_int32_contract_passes_tagged_integer_bounds(tmp_path):
    source = tmp_path / "bounds.ll"
    source.write_text(
        emit_llvm(
            "from mule_types import int32\ndef identity(value: int32): return value\n"
        )
    )
    toolchain = Toolchain()
    optimized = subprocess.run(
        [
            toolchain.clang,
            *toolchain.compile_flags(),
            "-Wno-override-module",
            "-S",
            "-emit-llvm",
            source,
            "-o",
            "-",
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    ).stdout
    checks = re.findall(r"call i32 @mule_assert_integer\([^\n]+", optimized)
    assert checks
    assert all("inttoptr (i64 -4294967295 to ptr)" in check for check in checks)
    assert all("inttoptr (i64 4294967295 to ptr)" in check for check in checks)
