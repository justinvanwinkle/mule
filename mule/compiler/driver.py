"""Build native executables from Mule LLVM IR and its Rust runtime."""

import os
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path

from .llvm import emit
from .modules import compile_modules


class ToolchainError(RuntimeError):
    """The native toolchain or target runtime could not produce an artifact."""


def emit_llvm(source: str, filename: str = "<unknown>") -> str:
    return emit(compile_modules(source, filename))


def build_runtime(
    directory: Path, *, rustc: str = "rustc", crate_type: str = "staticlib"
) -> Path:
    """Build the Rust runtime without third-party Rust packages."""
    compiler = shutil.which(rustc)
    if compiler is None:
        raise ToolchainError(f"Rust compiler '{rustc}' was not found")
    if crate_type not in ("staticlib", "rlib"):
        raise ValueError("runtime crate type must be staticlib or rlib")
    directory.mkdir(parents=True, exist_ok=True)
    runtime = Path(__file__).parents[1] / "runtime/lib.rs"
    suffix = "a" if crate_type == "staticlib" else "rlib"
    library = directory / f"libmule_runtime.{suffix}"
    command = [
        compiler,
        "--edition=2024",
        "--crate-name=mule_runtime",
        f"--crate-type={crate_type}",
        "-C",
        "opt-level=3",
        "-C",
        "panic=abort",
        str(runtime),
        "-o",
        str(library),
    ]
    if crate_type == "staticlib":
        command.extend(
            ["-C", "linker-plugin-lto", "-C", "lto=fat", "-C", "codegen-units=1"]
        )
    result = subprocess.run(command, check=False, capture_output=True, text=True)
    if result.returncode:
        raise ToolchainError(
            f"Rust runtime compilation failed:\n{result.stderr.strip()}"
        )
    return library


def lto_linker(rustc: str) -> Path:
    """Use rustc's own LLD to read its bitcode with a compatible LLVM version."""
    result = subprocess.run(
        [rustc, "--print", "target-libdir"],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode:
        raise ToolchainError(
            f"Could not locate Rust's LTO linker:\n{result.stderr.strip()}"
        )
    linker = Path(result.stdout.strip()).parent / "bin/gcc-ld/ld.lld"
    if not linker.is_file():
        raise ToolchainError(f"Rust's LLVM LTO linker was not found: {linker}")
    return linker


def link_llvm(
    llvm: str, output: Path, *, clang: str = "clang", rustc: str = "rustc"
) -> Path:
    """Compile LLVM and link the Rust runtime, replacing output only on success."""
    compiler = shutil.which(clang)
    if compiler is None:
        raise ToolchainError(f"LLVM Clang compiler '{clang}' was not found")
    if sys.platform != "linux" or struct.calcsize("P") != 8:
        raise ToolchainError("the initial native target requires 64-bit Linux")
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(
        prefix=".mule-build-", dir=output.parent
    ) as directory:
        temporary = Path(directory)
        ir_path = temporary / "module.ll"
        binary = temporary / "program"
        ir_path.write_text(llvm)
        runtime = build_runtime(temporary, rustc=rustc)
        linker = lto_linker(rustc)
        command = [
            compiler,
            "-O3",
            "-flto=full",
            f"--ld-path={linker}",
            "-Wl,--lto-O3",
            "-Wl,--lto-CGO3",
            "-Wl,--threads=1",
            "-Wno-override-module",
            "-Wl,--gc-sections",
            "-Wl,--strip-debug",
            str(ir_path),
            str(runtime),
            "-lgmp",
            "-lutil",
            "-lrt",
            "-lpthread",
            "-lm",
            "-ldl",
            "-o",
            str(binary),
        ]
        result = subprocess.run(command, check=False, capture_output=True, text=True)
        if result.returncode:
            raise ToolchainError(
                f"LLVM compilation/linking failed:\n{result.stderr.strip()}"
            )
        os.replace(binary, output)
    return output


def build(
    source: str | Path,
    output: str | Path,
    *,
    llvm_output: str | Path | None = None,
    clang: str = "clang",
    rustc: str = "rustc",
) -> Path:
    """Compile a source file into a native executable, replacing output on success.

    The initial target is 64-bit Linux with Mule's Rust runtime and GMP bigints.
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
    link_llvm(llvm, output_path, clang=clang, rustc=rustc)
    if llvm_output is not None:
        llvm_path.parent.mkdir(parents=True, exist_ok=True)
        llvm_path.write_text(llvm)
    return output_path
