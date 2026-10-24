"""Real builds and non-vacuous code inspection for rewrite regressions.

LLVM inspection is per-unit and pre-link, even when LTO is requested.
Inspect executable assembly for link-time transformations and Rust helpers.
Each case uses the driver's existing artifact cache and a single build worker.
"""

import argparse
import faulthandler
import json
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from difflib import get_close_matches
from functools import cached_property
from pathlib import Path

from mule.compiler.artifacts import Graph
from mule.compiler.driver import STDLIB, Build, Toolchain
from mule.compiler.modules import search_path
from test.python_host import python_host


def _checked_output(command):
    result = subprocess.run(
        command, capture_output=True, text=True, check=False, timeout=180
    )
    assert result.returncode == 0, (
        f"Command failed: {command!r}\n{result.stdout}\n{result.stderr}"
    )
    return result.stdout


def _llvm_functions(llvm):
    definitions = re.finditer(
        r'^define\b[^\n]*?@(?:"([^"\n]+)"|([^\s(]+))\([^\n]*\)[^\n]*\{\n.*?^}',
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    return [(match.group(1) or match.group(2), match.group()) for match in definitions]


@dataclass
class RewriteCase:
    source_path: Path
    executable: Path
    toolchain: Toolchain
    unit_paths: dict[str, Path]

    @property
    def llvm(self):
        """The exact main LLVM unit consumed by the native build."""
        return self.unit_paths["main"].read_text()

    @property
    def builtins_llvm(self):
        return self.unit_paths["builtins"].read_text()

    def _optimize_unit(self, unit):
        path = self.unit_paths[unit]
        # These are the actual build's compile flags, not compiler-source IR.
        # -S -emit-llvm exposes Clang's optimized per-unit result before linking.
        return _checked_output(
            [
                self.toolchain.clang,
                *self.toolchain.compile_flags(),
                "-Wno-override-module",
                "-S",
                "-emit-llvm",
                str(path),
                "-o",
                "-",
            ]
        )

    @cached_property
    def optimized_llvm(self):
        return self._optimize_unit("main")

    @cached_property
    def optimized_builtins_llvm(self):
        return self._optimize_unit("builtins")

    def unit_ir(self, unit, *, optimized=True):
        """Read a built unit; imported units use their graph step identifiers."""
        if unit == "main":
            return self.optimized_llvm if optimized else self.llvm
        if unit == "builtins":
            return self.optimized_builtins_llvm if optimized else self.builtins_llvm
        return (
            self._optimize_unit(unit)
            if optimized
            else self.unit_paths[unit].read_text()
        )

    def function_ir(self, pattern, *, optimized=True, unit="main"):
        """Select definitions by symbol regex, failing when none remain."""
        definitions = _llvm_functions(self.unit_ir(unit, optimized=optimized))
        selected = [body for name, body in definitions if re.search(pattern, name)]
        if not selected:
            nearby = get_close_matches(
                pattern, [name for name, _body in definitions], n=6, cutoff=0.1
            )
            candidates = [
                name if len(name) <= 160 else name[:157] + "..." for name in nearby
            ]
            raise AssertionError(
                f"No LLVM definition matched {pattern!r} in {unit!r}; "
                f"nearby symbols: {candidates!r}. "
                f"Full IR: case.unit_ir({unit!r}, optimized={optimized!r})"
            )
        return "\n\n".join(selected)

    @cached_property
    def assembly(self):
        """Disassemble the linked executable, including linked runtime helpers."""
        return _checked_output(
            ["objdump", "-d", "--no-show-raw-insn", str(self.executable)]
        )

    def function_assembly(self, pattern):
        """Select machine-code symbols by regex, failing on an empty selection."""
        definitions = list(
            re.finditer(r"^[0-9a-fA-F]+ <([^>]+)>:\n", self.assembly, re.MULTILINE)
        )
        selected = []
        for index, match in enumerate(definitions):
            if re.search(pattern, match.group(1)):
                end = (
                    definitions[index + 1].start()
                    if index + 1 < len(definitions)
                    else len(self.assembly)
                )
                selected.append(self.assembly[match.start() : end])
        assert selected, f"No executable symbol matched {pattern!r}"
        return "\n\n".join(selected)

    def compare(self, stdin="", args=()):
        """Compare the same source and runtime input with CPython 3.14.

        Programs must catch expected language errors and print stable observations.
        Both executions must succeed; traceback formatting is not this helper's contract.
        """
        # The real annotation module keeps marker imports executable under CPython.
        # Copy it only after native compilation, without changing the source program.
        shutil.copyfile(
            STDLIB / "mule_types.py", self.source_path.parent / "mule_types.py"
        )
        environment = os.environ.copy()
        environment.pop("PYTHONPATH", None)
        environment["LC_ALL"] = "C.UTF-8"
        commands = (
            [python_host(), str(self.source_path), *map(str, args)],
            [str(self.executable), *map(str, args)],
        )
        oracle, native = (
            subprocess.run(
                command,
                input=stdin,
                capture_output=True,
                text=True,
                check=False,
                timeout=30,
                cwd=self.source_path.parent,
                env=environment,
            )
            for command in commands
        )
        assert oracle.returncode == 0, oracle.stderr
        assert native.returncode == 0, native.stderr
        assert native.stdout == oracle.stdout, (
            f"stdin={stdin!r}, args={args!r}\n"
            f"CPython: {oracle.stdout!r}\nMule: {native.stdout!r}\n"
            f"Mule stderr: {native.stderr}"
        )
        assert native.stderr == oracle.stderr
        return native


def _build_artifacts(source_path, *, optimize, lto):
    """Run the same native graph in the isolated build process."""
    directory = source_path.parent
    source = source_path.read_text(newline="")
    toolchain = Toolchain(optimize=optimize, lto=lto)
    toolchain.require_target()
    roots = search_path(directory)
    with Graph(workers=1) as graph:
        builder = Build(graph, toolchain)
        builtins = builder.builtins()
        main = builder.main(
            graph.file(source_path), source, str(source_path), roots, None
        )
        link = builder.executable([builtins, main])
        graph.run(link)
        unit_paths = {
            unit.name: unit.path("unit.ll")
            for object_step in builder.objects.values()
            for unit in object_step.inputs
        }
        unit_paths["main"] = main.path("unit.ll")
        executable = directory / "program"
        shutil.copy2(link.path("program"), executable)
    assert executable.read_bytes().startswith(b"\x7fELF"), executable
    return {
        "executable": str(executable),
        "unit_paths": {name: str(path) for name, path in unit_paths.items()},
    }


def _run_bounded_build(command, *, context, timeout=600):
    """Bound a real build and terminate its whole compiler process group."""
    with subprocess.Popen(
        command,
        cwd=Path(__file__).resolve().parents[3],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        start_new_session=True,
    ) as process:
        try:
            output, _ = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            output, _ = process.communicate(timeout=10)
            raise AssertionError(
                f"Native build exceeded {timeout}s: {context}\n"
                f"Command: {command!r}\n"
                f"Build output:\n{output or '<no output before timeout>'}"
            ) from None
        assert process.returncode == 0, (
            f"Native build failed with status {process.returncode}: {context}\n"
            f"Command: {command!r}\nBuild output:\n{output}"
        )


def build_case(tmp_path, source, *, optimize=3, lto=False):
    """Build the supplied source through the ordinary native artifact graph.

    Defaults: generated units at O3, Rust runtime at O3, no LTO, one worker.
    Callers must mark O3 tests as integration tests. The ten-minute build limit
    accommodates cold transitive stdlib builds; it is not a speed assertion.
    """
    tmp_path.mkdir(parents=True, exist_ok=True)
    directory = Path(tempfile.mkdtemp(prefix="rewrite-", dir=tmp_path)).resolve()
    source_path = directory / "case.py"
    source_path.write_text(source)
    manifest = directory / "artifacts.json"
    toolchain = Toolchain(optimize=optimize, lto=lto)
    toolchain.require_target()
    command = [
        sys.executable,
        "-m",
        "test.mule.compiler.rewrite_support",
        str(source_path),
        str(manifest),
        "--optimize",
        str(optimize),
    ]
    if lto:
        command.append("--lto")
    _run_bounded_build(
        command, context=f"{source_path}, O{optimize}, LTO={lto}, workers=1"
    )
    artifacts = json.loads(manifest.read_text())
    return RewriteCase(
        source_path,
        Path(artifacts["executable"]),
        toolchain,
        {name: Path(path) for name, path in artifacts["unit_paths"].items()},
    )


def _main():
    parser = argparse.ArgumentParser(description="Build a rewrite regression artifact")
    parser.add_argument("source", type=Path)
    parser.add_argument("manifest", type=Path)
    parser.add_argument("--optimize", type=int, required=True)
    parser.add_argument("--lto", action="store_true")
    arguments = parser.parse_args()
    print(
        f"Building {arguments.source}, O{arguments.optimize}, "
        f"LTO={arguments.lto}, workers=1",
        flush=True,
    )
    faulthandler.dump_traceback_later(585)
    try:
        artifacts = _build_artifacts(
            arguments.source, optimize=arguments.optimize, lto=arguments.lto
        )
        arguments.manifest.write_text(json.dumps(artifacts))
    finally:
        faulthandler.cancel_dump_traceback_later()


if __name__ == "__main__":
    _main()
