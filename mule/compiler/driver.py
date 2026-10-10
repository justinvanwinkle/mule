"""Build native executables from Mule-generated LLVM IR and the object runtime."""

import os
import shlex
import shutil
import struct
import subprocess
import sys
import sysconfig
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

    The initial target is the compiler host's 64-bit Linux CPython 3.14 runtime.
    Generated code calls object C APIs, never a Python source/bytecode evaluator.
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
    if sys.implementation.name != "cpython" or sys.version_info[:2] != (3, 14):
        raise ToolchainError("the initial object runtime requires a CPython 3.14 host")
    if sysconfig.get_config_var("Py_GIL_DISABLED"):
        raise ToolchainError(
            "the initial object runtime requires a GIL-enabled CPython build"
        )
    include = Path(sysconfig.get_path("include"))
    libdir = Path(sysconfig.get_config_var("LIBDIR"))
    library = sysconfig.get_config_var("LDLIBRARY")
    if not (include / "Python.h").is_file() or not (libdir / library).is_file():
        raise ToolchainError(
            "CPython 3.14 development headers and shared library are required"
        )
    output_path.parent.mkdir(parents=True, exist_ok=True)
    runtime = Path(__file__).with_name("runtime.c")
    with tempfile.TemporaryDirectory(
        prefix=".mule-build-", dir=output_path.parent
    ) as directory:
        temporary = Path(directory)
        ir_path = temporary / "module.ll"
        binary = temporary / "program"
        ir_path.write_text(llvm)
        # sys.base_prefix supplies the standard library rather than the project's
        # virtual environment. The C runtime embeds this path as initialization
        # configuration, not as program source.
        home = sys.base_prefix.replace("\\", "\\\\").replace('"', '\\"')
        command = [
            compiler,
            "-O3",
            "-std=c11",
            "-Wno-override-module",
            f'-DMULE_PYTHON_HOME="{home}"',
            f"-I{include}",
            str(ir_path),
            str(runtime),
            f"-L{libdir}",
            "-lpython3.14",
            f"-Wl,-rpath,{libdir}",
            *shlex.split(sysconfig.get_config_var("LIBS") or ""),
            *shlex.split(sysconfig.get_config_var("SYSLIBS") or ""),
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
