"""Build native executables from Mule-generated LLVM IR and the object runtime."""

import os
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

from .frontend import lower
from .llvm import emit


class ToolchainError(RuntimeError):
    """The native toolchain or target runtime could not produce an artifact."""


def emit_llvm(source: str, filename: str = "<unknown>") -> str:
    return emit(lower(source, filename))


def build(
    source: str | Path,
    output: str | Path,
    *,
    llvm_output: str | Path | None = None,
    clang: str = "clang",
) -> Path:
    """Compile a source file into a native executable, replacing output on success.

    The initial target is 64-bit Linux with Mule's own runtime and GMP bigints.
    Generated programs never link or initialize CPython.
    """
    source_path, output_path = Path(source), Path(output)
    if output_path.resolve() == source_path.resolve():
        raise ValueError("the executable must not replace the source file")
    if llvm_output is not None:
        llvm_path = Path(llvm_output)
        if llvm_path.resolve() in (source_path.resolve(), output_path.resolve()):
            raise ValueError("LLVM output must differ from source and executable")
    llvm = emit_llvm(source_path.read_text(encoding="utf-8"), str(source_path))
    compiler = shutil.which(clang)
    if compiler is None:
        raise ToolchainError(f"LLVM Clang compiler '{clang}' was not found")
    if sys.platform != "linux" or struct.calcsize("P") != 8:
        raise ToolchainError("the initial native target requires 64-bit Linux")
    output_path.parent.mkdir(parents=True, exist_ok=True)
    runtime = Path(__file__).with_name("runtime.c")
    numeric = runtime.with_name("numeric.c")
    with tempfile.TemporaryDirectory(
        prefix=".mule-build-", dir=output_path.parent
    ) as directory:
        temporary = Path(directory)
        ir_path = temporary / "module.ll"
        binary = temporary / "program"
        ir_path.write_text(llvm)
        command = [
            compiler,
            "-O3",
            "-std=c11",
            "-Wno-override-module",
            str(ir_path),
            str(runtime),
            str(numeric),
            "-lgmp",
            "-lm",
            "-o",
            str(binary),
        ]
        result = subprocess.run(command, check=False, capture_output=True, text=True)
        if result.returncode:
            raise ToolchainError(
                f"LLVM compilation/linking failed:\n{result.stderr.strip()}"
            )
        os.replace(binary, output_path)
    if llvm_output is not None:
        llvm_path.parent.mkdir(parents=True, exist_ok=True)
        llvm_path.write_text(llvm)
    return output_path
