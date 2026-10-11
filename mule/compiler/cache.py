"""Reuse native executables built for unchanged programs.

A cache entry is keyed by the compiler, the toolchain and the main program (its
filename, source, module search path and package). Its manifest lists every
file the compilation read or probed for. An entry is reused only while each of
those files still has the recorded contents or is still absent, so an edited
module, or a new file that would shadow an import, causes a rebuild.
"""

import hashlib
import json
import os
import subprocess
import tempfile
from pathlib import Path

from .driver import ToolchainError, cache_directory, emit_llvm, link_llvm
from .inputs import recording, unchanged

CACHE_FORMAT = 1
COMPILER_SOURCES = Path(__file__).resolve().parents[1]


def toolchain_identity(command):
    """The version report of a toolchain program, so an upgrade rebuilds."""
    tool = command[0]
    try:
        result = subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError as error:
        raise ToolchainError(f"'{tool}' was not found") from error
    if result.returncode:
        raise ToolchainError(f"'{tool} --version' failed:\n{result.stderr.strip()}")
    return result.stdout


def compiler_fingerprint(clang, rustc):
    digest = hashlib.sha256()
    for path in sorted(COMPILER_SOURCES.rglob("*")):
        if path.suffix in (".py", ".rs") and path.is_file():
            digest.update(str(path.relative_to(COMPILER_SOURCES)).encode() + b"\0")
            digest.update(path.read_bytes() + b"\0")
    for command in ([clang, "--version"], [rustc, "--version", "--verbose"]):
        digest.update(toolchain_identity(command).encode() + b"\0")
    return digest.hexdigest()


def cache_key(source, filename, roots, package, clang, rustc):
    identity = {
        "format": CACHE_FORMAT,
        "compiler": compiler_fingerprint(clang, rustc),
        "filename": filename,
        "source": hashlib.sha256(source.encode()).hexdigest(),
        "roots": [str(root) for root in roots],
        "package": package,
    }
    return hashlib.sha256(json.dumps(identity, sort_keys=True).encode()).hexdigest()


def read_manifest(entry):
    try:
        return json.loads((entry / "manifest.json").read_text(encoding="utf-8"))
    except FileNotFoundError:
        return None


def cached_executable(
    source, filename, roots, package=None, *, clang="clang", rustc="rustc"
):
    """Return a native executable for the program, building it only when no
    cache entry with unchanged inputs exists."""
    entry = cache_directory() / cache_key(
        source, filename, roots, package, clang, rustc
    )
    manifest = read_manifest(entry)
    if manifest is not None:
        program = entry / manifest["program"]
        if program.is_file() and unchanged(manifest["inputs"]):
            return program
    entry.mkdir(parents=True, exist_ok=True)
    with recording() as inputs:
        llvm = emit_llvm(source, filename, roots=roots, package=package)
    # A fresh name per build: a manifest only ever names the executable built
    # from its own inputs, even when processes rebuild the same entry at once.
    program = entry / f"program-{os.urandom(8).hex()}"
    link_llvm(llvm, program, clang=clang, rustc=rustc)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=entry, prefix=".manifest-", delete=False
    ) as file:
        json.dump({"program": program.name, "inputs": inputs}, file)
    os.replace(file.name, entry / "manifest.json")
    if manifest is not None and manifest["program"] != program.name:
        (entry / manifest["program"]).unlink(missing_ok=True)
    return program
