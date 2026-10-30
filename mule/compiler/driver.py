"""Build native executables from Mule LLVM IR and its Rust runtime.

A build is a graph of cached steps (``artifacts``): the runtime library, the
builtins unit (the compiled stdlib protocols, with an interface file other
units compile against), one unit per program module, an object file per
unit, and the link. Each step reuses its cached outputs when Mule, its
settings and its inputs are unchanged, and runs as soon as its inputs exist.
"""

import json
import os
import shutil
import struct
import subprocess
import sys
import threading
from collections.abc import Iterable
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from functools import cache
from pathlib import Path

from mule.diagnostics import phase

from .artifacts import (
    STEP_STACK_SIZE,
    Graph,
    Node,
    Step,
    ToolchainError,
    cache_directory,
    mule_hash,
    physical_core_count,
    tool_identity,
)
from .codegen import Program, generate
from .inputs import (
    _recording,
    file_digest,
    file_state,
    read_source,
)
from .interfaces import (
    RUNTIME_BUILTINS,
    BuiltinsInterface,
    ModuleInterface,
    _finish_unit,
    _interface,
    _prepare_unit,
    _PreparedUnit,
    builtins_interface,
    compile_builtins,
    compile_unit,
    import_resolver,
    installed_builtins,
    linked_imports,
    module_interface,
    read_elf_interface,
    read_ll_interface,
)
from .modules import (
    STDLIB,
    CompiledModule,
    ModuleCompiler,
    search_path,
)

__all__ = [
    "LLVM_OPT_CHOICES",
    "RUNTIME_BUILTINS",
    "BuiltinsInterface",
    "ModuleInterface",
    "Toolchain",
    "ToolchainError",
    "build",
    "build_dynamic_module",
    "build_executable",
    "build_runtime",
    "builtins_interface",
    "builtins_object",
    "cached_builtins_interface",
    "compile_builtins",
    "compile_program",
    "compile_unit",
    "emit_llvm",
    "fasl_cache_path",
    "import_resolver",
    "installed_builtins",
    "link_llvm",
    "linked_imports",
    "module_interface",
    "normalize_llvm_opts",
    "read_elf_interface",
    "read_ll_interface",
    "resolve_recorded",
    "resolves_as_recorded",
    "write_imports",
]

LINK_LIBRARIES = ("-l:libgmp.so.10", "-lutil", "-lrt", "-lpthread", "-lm", "-ldl")
BUILTINS_SHARDS = 16
OBJECT_SHARD_THRESHOLD = 2 << 20


def build_runtime(
    directory: Path,
    *,
    rustc: str = "rustc",
    crate_type: str = "staticlib",
    lto: bool = False,
) -> Path:
    """Build the Rust runtime without third-party Rust packages.

    With ``lto`` a static library holds LLVM bitcode for whole-program
    optimization with the program; without it, native objects for a plain link.
    """
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
    if crate_type == "staticlib" and lto:
        command.extend(["-C", "linker-plugin-lto", "-C", "lto=fat"])
    result = subprocess.run(command, check=False, capture_output=True, text=True)
    if result.returncode:
        raise ToolchainError(
            f"Rust runtime compilation failed:\n{result.stderr.strip()}"
        )
    return library


@cache
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


LLVM_OPT_CHOICES = (
    "hot-cold-split",
    "hot-cold-split=<int>",
    "gvn-sink",
    "gvn-hoist",
    "merge-functions",
    "constraint-elimination",
    "unroll-and-jam",
    "attributor",
    "attributor=<mode>",
    "inline-threshold=<int>",
)
_ATTRIBUTOR_MODES = (
    "light",
    "full",
    "module",
    "module-light",
    "cgscc",
    "cgscc-light",
)
_SIMPLE_LLVM_OPTS = {
    "hot-cold-split": ("--hot-cold-split",),
    "gvn-sink": ("--enable-gvn-sink",),
    "gvn-hoist": ("--enable-gvn-hoist",),
    "merge-functions": ("--enable-merge-functions",),
    "constraint-elimination": ("--enable-constraint-elimination",),
    "unroll-and-jam": ("--enable-unroll-and-jam",),
    "attributor": ("--attributor-enable=light",),
}


def _expand_llvm_opt(spec: str) -> tuple[str, tuple[str, ...]]:
    item = spec.strip()
    if not item:
        raise ValueError(
            f"empty --opt value; expected one of {', '.join(LLVM_OPT_CHOICES)}"
        )
    if item in _SIMPLE_LLVM_OPTS:
        canonical = "attributor=light" if item == "attributor" else item
        return canonical, _SIMPLE_LLVM_OPTS[item]
    name, sep, value = item.partition("=")
    if sep:
        if name == "hot-cold-split":
            try:
                threshold = int(value)
            except ValueError as error:
                raise ValueError(
                    f"hot-cold-split threshold must be an integer, got {value!r}"
                ) from error
            return (
                f"hot-cold-split={threshold}",
                ("--hot-cold-split", f"--hotcoldsplit-threshold={threshold}"),
            )
        if name == "attributor":
            if value not in _ATTRIBUTOR_MODES:
                modes = ", ".join(_ATTRIBUTOR_MODES)
                raise ValueError(
                    f"attributor mode must be one of {modes}, got {value!r}"
                )
            return f"attributor={value}", (f"--attributor-enable={value}",)
        if name == "inline-threshold":
            try:
                threshold = int(value)
            except ValueError as error:
                raise ValueError(
                    f"inline-threshold must be an integer, got {value!r}"
                ) from error
            return f"inline-threshold={threshold}", (f"--inline-threshold={threshold}",)
    choices = ", ".join(LLVM_OPT_CHOICES)
    raise ValueError(f"unknown LLVM optimization {item!r}; expected one of {choices}")


def normalize_llvm_opts(opts: Iterable[str] | str | None) -> tuple[str, ...]:
    """Validate and normalize ``--opt`` optimization specifications."""
    if opts is None:
        return ()
    raw_items = (opts,) if isinstance(opts, str) else tuple(opts)
    normalized: list[str] = []
    for raw in raw_items:
        parts = raw.split(",")
        for part in parts:
            canonical, _ = _expand_llvm_opt(part)
            if canonical not in normalized:
                normalized.append(canonical)
    return tuple(normalized)


@dataclass(frozen=True, slots=True)
class Toolchain:
    clang: str = "clang"
    rustc: str = "rustc"
    optimize: int = 1
    lto: bool = False
    arch: str | None = None
    opt: tuple[str, ...] = ()

    def __post_init__(self):
        if self.optimize not in (0, 1, 2, 3):
            raise ValueError("optimization level must be 0, 1, 2, or 3")
        object.__setattr__(self, "optimize", int(self.optimize))
        object.__setattr__(self, "opt", normalize_llvm_opts(self.opt))

    def require_target(self) -> None:
        if sys.platform != "linux" or struct.calcsize("P") != 8:
            raise ToolchainError("the initial native target requires 64-bit Linux")
        if shutil.which(self.clang) is None:
            raise ToolchainError(f"LLVM Clang compiler '{self.clang}' was not found")

    def llvm_tool(self, name: str) -> str:
        """Find the LLVM tool ``name``, preferring clang's installation."""
        clang = shutil.which(self.clang)
        if clang is None:
            raise ToolchainError(f"LLVM Clang compiler '{self.clang}' was not found")
        sibling = Path(clang).resolve().parent / name
        if sibling.is_file():
            return str(sibling)
        found = shutil.which(name)
        if found is None:
            raise ToolchainError(f"LLVM's {name} was not found beside clang")
        return found

    def llvm_flags(self) -> tuple[str, ...]:
        flags: list[str] = []
        for spec in self.opt:
            _, expanded = _expand_llvm_opt(spec)
            for flag in expanded:
                if flag not in flags:
                    flags.append(flag)
        return tuple(flags)

    def clang_settings(self) -> dict:
        settings = {
            "clang": tool_identity(
                (self.clang, "--version"),
                tool="LLVM Clang compiler",
                failure="LLVM compilation/linking failed",
            ),
            "optimize": self.optimize,
            "lto": self.lto,
            "arch": self.arch,
        }
        if self.opt:
            settings["opt"] = list(self.llvm_flags())
        return settings

    def compile_flags(self) -> list[str]:
        flags = [f"-O{self.optimize}"]
        if self.arch is not None:
            flags.append(f"-march={self.arch}")
        if self.lto:
            flags.append("-flto=full")
        else:
            flags.append("-fPIC")
        for flag in self.llvm_flags():
            flags.extend(("-mllvm", flag))
        return flags

    def link_flags(self) -> list[str]:
        flags = [f"-O{self.optimize}"]
        if self.arch is not None:
            flags.append(f"-march={self.arch}")
        if self.lto:
            level = self.optimize
            flags.extend(
                [
                    "-flto=full",
                    f"-Wl,--lto-O{level}",
                    f"-Wl,--lto-CGO{level}",
                    "-Wl,--threads=1",
                ]
            )
            for flag in self.llvm_flags():
                flags.append(f"-Wl,-mllvm,{flag}")
        return flags


def _run(command: list[str], failure: str) -> None:
    result = subprocess.run(command, check=False, capture_output=True, text=True)
    if result.returncode:
        raise ToolchainError(f"{failure}:\n{result.stderr.strip()}")


def fasl_cache_path(
    compiler_hash: str,
    optimize: int,
    module_name: str,
    source_path: Path,
) -> Path | None:
    """The deterministic cache path of a single-module FASL shared object."""
    try:
        source_bytes = source_path.read_bytes()
    except OSError:
        return None
    payload = f"{compiler_hash}\0{optimize}\0{module_name}\0".encode() + source_bytes
    h = 0x6C62272E07BB014262B821756295C58D
    prime = 0x0000000001000000000000000000013B
    mask = (1 << 128) - 1
    for b in payload:
        h = ((h ^ b) * prime) & mask
    return cache_directory() / "fasl" / f"{h:032x}.so"


def _install_fasl_symlink(
    step: Step, descriptor: CompiledModule, optimize: int
) -> None:
    if descriptor.path is None:
        return
    dest = fasl_cache_path(mule_hash(), optimize, descriptor.name, descriptor.path)
    if dest is None:
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_name(f".{dest.name}.{os.getpid()}.{threading.get_ident()}.tmp")
    try:
        if tmp.exists() or tmp.is_symlink():
            tmp.unlink()
        os.symlink(step.path("module.so"), tmp)
        os.replace(tmp, dest)
    except OSError:
        pass


@dataclass(frozen=True, slots=True)
class _UnitSpec:
    unit_key: str
    step: Step
    descriptor: CompiledModule | None
    source: str | None
    filename: str
    roots: tuple[Path, ...] | None
    package: str | None


class Build:
    """The step graph of one executable or FASL build."""

    def __init__(
        self,
        graph: Graph,
        toolchain: Toolchain,
        *,
        fasl_mode: bool = False,
    ):
        self.graph = graph
        self.toolchain = toolchain
        self.fasl_mode = fasl_mode
        self.link: Step | None = None
        # The object steps the link combines, by step name.
        self.objects: dict[str, Step] = {}
        self._compiling: dict[str, set[str]] = {}
        self._active_modules: set[str] = set()
        self._root_units: list[_UnitSpec] = []
        self._prepared: dict[str, _PreparedUnit] = {}
        self._prepared_errors: dict[str, Exception] = {}
        self._interfaces: dict[str, ModuleInterface | None] = {}
        self._cycle_edges: set[tuple[str, str]] = set()
        self._validated: dict[tuple[str, Path], ModuleCompiler] = {}
        self._compiled_imports: dict[str, list[CompiledModule]] = {}

    def _pop_validated(self, step: Step) -> ModuleCompiler | None:
        if step.directory is None:
            return None
        with self.graph.lock:
            return self._validated.pop((step.name, step.directory), None)

    def prepare_import_graph(self) -> None:
        """Build the transitive import DAG up front so independent module units
        and their object steps schedule across all workers without blocking."""
        if not self._root_units:
            return
        interface = self.interface()
        current_hash = mule_hash()
        seen: set[str] = {spec.unit_key for spec in self._root_units}
        unit_steps: dict[str, Step] = {
            spec.unit_key: spec.step for spec in self._root_units
        }
        eager_deps: dict[str, list[str]] = {}
        cached_units: set[str] = set()
        wave: list[_UnitSpec] = list(self._root_units)

        def inspect_spec(
            spec: _UnitSpec,
        ) -> tuple[
            _UnitSpec,
            bool,
            _PreparedUnit | None,
            Exception | None,
            list[CompiledModule],
        ]:
            if spec.step.try_reuse_cache():
                assert spec.step.directory is not None
                with self.graph.lock:
                    compiler = self._validated.get(
                        (spec.step.name, spec.step.directory)
                    )
                if compiler is None:
                    compiler = import_resolver(
                        spec.filename,
                        interface,
                        spec.descriptor,
                        spec.roots,
                        spec.package,
                    )
                    resolve_recorded(compiler, spec.step.path("imports.json"))
                return spec, True, None, None, list(compiler.pending)
            try:
                prepared = _prepare_unit(
                    spec.source,
                    spec.filename,
                    interface,
                    module=spec.descriptor,
                    roots=spec.roots,
                    package=spec.package,
                )
            except Exception as error:  # noqa: BLE001
                return spec, False, None, error, []
            return spec, False, prepared, None, prepared.linked_pending

        workers = self.graph.worker_count
        pool: ThreadPoolExecutor | None = None
        previous_stack: int | None = None
        try:
            while wave:
                if workers > 1 and len(wave) > 1:
                    if pool is None:
                        previous_stack = threading.stack_size(STEP_STACK_SIZE)
                        pool = ThreadPoolExecutor(max_workers=workers)
                    results = list(pool.map(inspect_spec, wave))
                else:
                    results = [inspect_spec(spec) for spec in wave]
                next_wave: list[_UnitSpec] = []
                for spec, is_cached, prepared, error, next_modules in results:
                    if is_cached:
                        cached_units.add(spec.unit_key)
                        eager_deps[spec.unit_key] = [
                            dep.name
                            for dep in next_modules
                            if not dep.missing
                            and not dep.native
                            and dep.path is not None
                        ]
                    elif error is not None:
                        self._prepared_errors[spec.unit_key] = error
                        eager_deps[spec.unit_key] = []
                    else:
                        assert prepared is not None
                        self._prepared[spec.unit_key] = prepared
                        eager_deps[spec.unit_key] = [
                            dep.name
                            for dep in prepared.eager_modules
                            if not dep.missing
                            and not dep.native
                            and dep.path is not None
                        ]
                    for dep in next_modules:
                        if self.fasl_mode:
                            if dep.missing or dep.native or dep.path is None:
                                continue
                            dest = fasl_cache_path(
                                current_hash,
                                self.toolchain.optimize,
                                dep.name,
                                dep.path,
                            )
                            if dest is not None and dest.is_file():
                                continue
                            dep_step = self.module(dep, spec.roots, spec.package)
                            self.fasl(dep_step, dep)
                        else:
                            dep_step = self.module(dep, spec.roots, spec.package)
                            if self.link is not None:
                                self.link.add_input(self.object(dep_step))
                        unit_steps.setdefault(dep.name, dep_step)
                        if dep.name not in seen:
                            seen.add(dep.name)
                            if (
                                not dep.missing
                                and not dep.native
                                and dep.path is not None
                            ):
                                next_wave.append(
                                    _UnitSpec(
                                        dep.name,
                                        dep_step,
                                        dep,
                                        None,
                                        str(dep.path),
                                        spec.roots,
                                        spec.package,
                                    )
                                )
                            else:
                                cached_units.add(dep.name)
                                eager_deps[dep.name] = []
                wave = next_wave
        finally:
            if pool is not None:
                pool.shutdown(wait=True)
            if previous_stack is not None:
                threading.stack_size(previous_stack)

        visited: set[str] = set()
        active: set[str] = set()

        def visit(u: str) -> None:
            if u in visited:
                return
            visited.add(u)
            if u not in cached_units:
                active.add(u)
            u_step = unit_steps.get(u)
            for v in eager_deps.get(u, ()):
                if v == u or v not in unit_steps:
                    continue
                if v in active:
                    self._cycle_edges.add((u, v))
                    continue
                if v not in visited:
                    visit(v)
                if (
                    u_step is not None
                    and u not in cached_units
                    and v not in cached_units
                ):
                    u_step.add_dependency(unit_steps[v])
            active.discard(u)

        for spec in self._root_units:
            visit(spec.unit_key)
        for u in list(unit_steps):
            if u not in visited:
                visit(u)

    def resolve_import_interface(
        self, caller_name: str, dep: CompiledModule, roots, package
    ) -> ModuleInterface | None:
        if dep.missing or dep.native or dep.path is None:
            return None
        with self.graph.lock:
            if (caller_name, dep.name) in self._cycle_edges:
                return None
            if dep.name in self._interfaces:
                return self._interfaces[dep.name]
            if dep.name in self._active_modules:
                return None
            stack = [dep.name]
            visited = set()
            while stack:
                curr = stack.pop()
                if curr == caller_name:
                    return None
                if curr not in visited:
                    visited.add(curr)
                    stack.extend(self._compiling.get(curr, ()))
            self._compiling.setdefault(caller_name, set()).add(dep.name)
        try:
            if self.fasl_mode:
                dest = fasl_cache_path(
                    mule_hash(), self.toolchain.optimize, dep.name, dep.path
                )
                if dest is not None and dest.is_file():
                    res = read_elf_interface(dest)
                    if res is not None:
                        with self.graph.lock:
                            self._interfaces[dep.name] = res[0]
                        return res[0]
                mod_step = self.module(dep, roots, package)
                self.fasl(mod_step, dep)
            else:
                mod_step = self.module(dep, roots, package)
            if mod_step.directory is None:
                self.graph.resolve_now(mod_step)
            with self.graph.lock:
                if dep.name in self._interfaces:
                    return self._interfaces[dep.name]
            res = read_ll_interface(mod_step.path("unit.ll"))
            iface = None if res is None else res[0]
            with self.graph.lock:
                self._interfaces[dep.name] = iface
            return iface
        finally:
            with self.graph.lock:
                if caller_name in self._compiling:
                    self._compiling[caller_name].discard(dep.name)

    def runtime(self) -> Step:
        rustc, lto = self.toolchain.rustc, self.toolchain.lto
        sources = sorted((Path(__file__).parents[1] / "runtime").glob("*.rs"))

        def action(inputs, directory):
            output = cache_directory() / "steps" / step.key / "libmule_runtime.a"
            with phase("compile runtime -> %s", output):
                built = build_runtime(directory, rustc=rustc, lto=lto)
                if built != directory / "libmule_runtime.a":
                    os.replace(built, directory / "libmule_runtime.a")

        step = self.graph.step(
            f"runtime lto={lto}",
            lambda: Step(
                self.graph,
                f"runtime lto={lto}",
                inputs=[self.graph.file(path) for path in sources],
                settings={
                    "step": "runtime",
                    "lto": lto,
                    "rustc": tool_identity(
                        (rustc, "--version", "--verbose"),
                        tool="Rust compiler",
                        failure="Rust runtime compilation failed",
                    ),
                },
                outputs=("libmule_runtime.a",),
                action=action,
            ),
        )
        return step

    def builtins(self) -> Step:
        def action(inputs, directory):
            output = cache_directory() / "steps" / step.key / "unit.ll"
            with phase("compile unit builtins -> %s", output):
                unit, modules = compile_builtins()
                interface = builtins_interface(unit, modules)
                unit.interface_bytes = interface.to_json().encode("utf-8")
                (directory / "unit.ll").write_text(generate(unit))

        sources = sorted([*STDLIB.glob("_*.py"), *(STDLIB / "bootstrap").glob("*.py")])
        step = self.graph.step(
            "builtins",
            lambda: Step(
                self.graph,
                "builtins",
                inputs=[self.graph.file(path) for path in sources],
                settings={"step": "builtins"},
                outputs=("unit.ll",),
                action=action,
                discover=lambda step: self.prepare_import_graph(),
                validate=lambda entry: True,
            ),
        )
        return step

    def interface(self) -> BuiltinsInterface:
        builtins = self.builtins()
        if builtins.output_digests is None:
            raise RuntimeError("builtins has not produced its outputs")
        return _interface(
            builtins.path("unit.ll"),
            builtins.output_digests["unit.ll"],
        )

    def object(self, unit: Step) -> Step:
        toolchain = self.toolchain
        lto = toolchain.lto
        sharded = unit.name == "builtins" and not lto
        name = f"{unit.name} object optimize={toolchain.optimize} lto={lto}"

        def action(inputs, directory):
            output = cache_directory() / "steps" / step.key / "unit.o"
            with phase("compile object %s -> %s", unit.name, output):
                unit_ll = unit.path("unit.ll")
                use_shards = not lto and (
                    sharded or unit_ll.stat().st_size >= OBJECT_SHARD_THRESHOLD
                )
                if use_shards:
                    llvm_split = toolchain.llvm_tool("llvm-split")
                    shard_prefix = directory / "shard"
                    _run(
                        [
                            llvm_split,
                            f"-j={BUILTINS_SHARDS}",
                            f"-o={shard_prefix}",
                            str(unit_ll),
                        ],
                        "LLVM compilation/linking failed",
                    )

                    def compile_shard(index: int) -> str:
                        shard = directory / f"shard{index}"
                        obj = directory / f"shard{index}.o"
                        _run(
                            [
                                toolchain.clang,
                                *toolchain.compile_flags(),
                                "-x",
                                "ir",
                                "-Wno-override-module",
                                "-c",
                                str(shard),
                                "-o",
                                str(obj),
                            ],
                            "LLVM compilation/linking failed",
                        )
                        shard.unlink()
                        return str(obj)

                    if self.graph.worker_count == 1:
                        shard_objects = [
                            compile_shard(index) for index in range(BUILTINS_SHARDS)
                        ]
                    else:
                        workers = min(BUILTINS_SHARDS, physical_core_count())
                        with ThreadPoolExecutor(max_workers=workers) as pool:
                            shard_objects = list(
                                pool.map(compile_shard, range(BUILTINS_SHARDS))
                            )
                    _run(
                        [
                            str(lto_linker(toolchain.rustc)),
                            "-r",
                            *shard_objects,
                            "-o",
                            str(directory / "unit.o"),
                        ],
                        "LLVM compilation/linking failed",
                    )
                    _run(
                        [
                            toolchain.llvm_tool("llvm-objcopy"),
                            "--localize-hidden",
                            str(directory / "unit.o"),
                        ],
                        "LLVM compilation/linking failed",
                    )
                    for obj in shard_objects:
                        Path(obj).unlink()
                    return
                _run(
                    [
                        toolchain.clang,
                        *toolchain.compile_flags(),
                        "-Wno-override-module",
                        "-c",
                        str(unit_ll),
                        "-o",
                        str(directory / "unit.o"),
                    ],
                    "LLVM compilation/linking failed",
                )

        settings = {
            "step": "object",
            "lto": lto,
            **toolchain.clang_settings(),
        }
        if not lto:
            settings["shards"] = BUILTINS_SHARDS
            settings["shard_threshold"] = OBJECT_SHARD_THRESHOLD
        step = self.graph.step(
            name,
            lambda: Step(
                self.graph,
                name,
                inputs=[unit],
                settings=settings,
                outputs=("unit.o",),
                action=action,
            ),
        )
        self.objects[step.name] = step
        return step

    def module(self, descriptor: CompiledModule, roots, package) -> Step:
        """The unit of an imported source module, and its object, added to
        the link."""
        builtins = self.builtins()
        path = descriptor.path
        name = f"module {descriptor.name}"

        def resolver():
            return import_resolver(
                str(path), self.interface(), descriptor, roots, package
            )

        def validate(entry: Path) -> bool:
            compiler = resolver()
            if resolves_as_recorded(compiler, entry / "imports.json"):
                with self.graph.lock:
                    self._validated[name, entry] = compiler
                return True
            return False

        def action(inputs, directory):
            output = cache_directory() / "steps" / step.key / "unit.ll"
            with phase("compile unit %s -> %s", name, output):
                with self.graph.lock:
                    self._active_modules.add(descriptor.name)
                    error = self._prepared_errors.pop(descriptor.name, None)
                    prepared = self._prepared.pop(descriptor.name, None)
                try:
                    if error is not None:
                        raise error
                    if prepared is None:
                        prepared = _prepare_unit(
                            descriptor.source(),
                            str(path),
                            self.interface(),
                            module=descriptor,
                            roots=roots,
                            package=package,
                        )
                    program, imported, mod_iface = _finish_unit(
                        prepared,
                        str(path),
                        self.interface(),
                        module=descriptor,
                        resolve_import_interface=lambda dep: (
                            self.resolve_import_interface(
                                descriptor.name, dep, roots, package
                            )
                        ),
                    )
                    builtin_names = set(self.interface().modules)
                    with self.graph.lock:
                        self._interfaces[descriptor.name] = mod_iface
                        self._compiled_imports[name] = [
                            dep
                            for dep in imported
                            if not dep.missing
                            and not dep.native
                            and dep.name not in builtin_names
                        ]
                finally:
                    with self.graph.lock:
                        self._active_modules.discard(descriptor.name)
                (directory / "unit.ll").write_text(generate(program))
                write_imports(directory / "imports.json", imported)

        step = self.graph.step(
            name,
            lambda: Step(
                self.graph,
                name,
                inputs=[
                    self.graph.file(path)
                    if path is not None
                    else self.graph.text(name, ""),
                    builtins,
                ],
                settings={
                    "step": "module",
                    "name": descriptor.name,
                },
                outputs=("unit.ll", "imports.json"),
                action=action,
                discover=lambda step: self.discover(
                    step,
                    self._pop_validated(step) or resolver(),
                    roots,
                    package,
                ),
                validate=validate,
            ),
        )
        return step

    def discover(self, step: Step, resolver: ModuleCompiler, roots, package) -> None:
        """Add the units of the source modules ``step`` imports to the link,
        as they resolve on this build's search path."""
        with self.graph.lock:
            pending = self._compiled_imports.pop(step.name, None)
        if pending is None:
            if not resolver.requested:
                resolve_recorded(resolver, step.path("imports.json"))
            pending = list(resolver.pending)
        if self.fasl_mode:
            current_hash = mule_hash()
            for descriptor in pending:
                if descriptor.missing or descriptor.native or descriptor.path is None:
                    continue
                dest = fasl_cache_path(
                    current_hash,
                    self.toolchain.optimize,
                    descriptor.name,
                    descriptor.path,
                )
                if dest is not None and dest.is_file():
                    continue
                self.fasl(self.module(descriptor, roots, package), descriptor)
            return
        if self.link is None:
            return
        for descriptor in pending:
            self.link.add_input(self.object(self.module(descriptor, roots, package)))

    def main(
        self, source_node: Node, source: str, filename: str, roots, package
    ) -> Step:
        builtins = self.builtins()

        def resolver():
            return import_resolver(filename, self.interface(), None, roots, package)

        def validate(entry: Path) -> bool:
            compiler = resolver()
            if resolves_as_recorded(compiler, entry / "imports.json"):
                with self.graph.lock:
                    self._validated["__main__", entry] = compiler
                return True
            return False

        def action(inputs, directory):
            output = cache_directory() / "steps" / step.key / "unit.ll"
            with phase("compile unit __main__ %s -> %s", filename, output):
                with self.graph.lock:
                    error = self._prepared_errors.pop("__main__", None)
                    prepared = self._prepared.pop("__main__", None)
                if error is not None:
                    raise error
                if prepared is None:
                    prepared = _prepare_unit(
                        source,
                        filename,
                        self.interface(),
                        roots=roots,
                        package=package,
                    )
                program, imported, _ = _finish_unit(
                    prepared,
                    filename,
                    self.interface(),
                    resolve_import_interface=lambda dep: self.resolve_import_interface(
                        "__main__", dep, roots, package
                    ),
                )
                builtin_names = set(self.interface().modules)
                with self.graph.lock:
                    self._compiled_imports["__main__"] = [
                        dep
                        for dep in imported
                        if not dep.missing
                        and not dep.native
                        and dep.name not in builtin_names
                    ]
                if not self.toolchain.lto:
                    program.toolchain_info = (
                        sys.executable,
                        self.toolchain.clang,
                        f"{self.toolchain.rustc}\n{mule_hash()}",
                        self.toolchain.optimize,
                    )
                (directory / "unit.ll").write_text(generate(program))
                write_imports(directory / "imports.json", imported)

        step = self.graph.step(
            "__main__",
            lambda: Step(
                self.graph,
                "__main__",
                inputs=[source_node, builtins],
                settings={
                    "step": "main",
                    "package": package,
                },
                outputs=("unit.ll", "imports.json"),
                action=action,
                discover=lambda step: self.discover(
                    step,
                    self._pop_validated(step) or resolver(),
                    roots,
                    package,
                ),
                validate=validate,
            ),
        )
        self._root_units.append(
            _UnitSpec("__main__", step, None, source, filename, roots, package)
        )
        return step

    def executable(self, units: list[Step]) -> Step:
        toolchain = self.toolchain
        runtime = self.runtime()

        def action(inputs, directory):
            objects = [str(step.path("unit.o")) for step in self.objects.values()]
            runtime_lib = str(runtime.path("libmule_runtime.a"))
            link_cmd = [
                toolchain.clang,
                *toolchain.link_flags(),
                f"--ld-path={lto_linker(toolchain.rustc)}",
                "-Wno-override-module",
                "-Wl,--gc-sections",
                "-Wl,--strip-debug",
            ]
            if not toolchain.lto:
                link_cmd.append("-Wl,--export-dynamic")
            link_cmd.extend(objects)
            if not toolchain.lto:
                link_cmd.extend(
                    ["-Wl,--whole-archive", runtime_lib, "-Wl,--no-whole-archive"]
                )
            else:
                link_cmd.append(runtime_lib)
            link_cmd.extend([*LINK_LIBRARIES, "-o", str(directory / "program")])
            assert self.link is not None
            output = cache_directory() / "steps" / self.link.key / "program"
            with phase("link executable -> %s", output):
                _run(link_cmd, "LLVM compilation/linking failed")

        objects = [self.object(unit) for unit in units]
        self.link = self.graph.step(
            "link",
            lambda: Step(
                self.graph,
                "link",
                inputs=[runtime, *objects],
                settings={"step": "link", **toolchain.clang_settings()},
                outputs=("program",),
                action=action,
            ),
        )
        # Standard streams are system-module state even when the source does
        # not import sys explicitly. Protocol imports use this same image unit.
        system = ModuleCompiler("<system>", roots=(STDLIB,)).resolve("sys", None, None)
        sys_step = self.module(system, (STDLIB,), None)
        self.link.add_input(self.object(sys_step))
        if system.path is not None:
            self._root_units.append(
                _UnitSpec(
                    system.name,
                    sys_step,
                    system,
                    None,
                    str(system.path),
                    (STDLIB,),
                    None,
                )
            )
        return self.link

    def add_fasl_root(self, descriptor: CompiledModule, roots, package) -> Step:
        step = self.module(descriptor, roots, package)
        fasl_step = self.fasl(step, descriptor)
        if descriptor.path is not None:
            self._root_units.append(
                _UnitSpec(
                    descriptor.name,
                    step,
                    descriptor,
                    None,
                    str(descriptor.path),
                    roots,
                    package,
                )
            )
        return fasl_step

    def fasl(self, unit: Step, descriptor: CompiledModule) -> Step:
        """Link one module's object into its standalone FASL shared object."""
        toolchain = self.toolchain
        obj = self.object(unit)
        name = f"{unit.name} fasl optimize={toolchain.optimize}"

        def action(inputs, directory):
            output = (
                fasl_cache_path(
                    mule_hash(), toolchain.optimize, descriptor.name, descriptor.path
                )
                if descriptor.path is not None
                else None
            ) or (cache_directory() / "steps" / step.key / "module.so")
            with phase("compile fasl %s -> %s", unit.name, output):
                _run(
                    [
                        toolchain.clang,
                        "-shared",
                        *toolchain.link_flags(),
                        f"--ld-path={lto_linker(toolchain.rustc)}",
                        "-Wno-override-module",
                        "-Wl,--strip-debug",
                        str(obj.path("unit.o")),
                        "-o",
                        str(directory / "module.so"),
                    ],
                    "LLVM compilation/linking failed",
                )

        step = self.graph.step(
            name,
            lambda: Step(
                self.graph,
                name,
                inputs=[obj],
                settings={
                    "step": "fasl",
                    "module": descriptor.name,
                    **toolchain.clang_settings(),
                },
                outputs=("module.so",),
                action=action,
                discover=lambda step: _install_fasl_symlink(
                    step, descriptor, toolchain.optimize
                ),
            ),
        )
        return step


def write_imports(path: Path, modules: list[CompiledModule]) -> None:
    """Record how each import resolved: the unit's output depends on whether a
    module is native, missing, or compiled, and on the imported module's content."""
    path.write_text(
        json.dumps(
            [
                [
                    module.name,
                    module.native,
                    module.missing,
                    [file_state(p) for p in module.namespace_paths],
                    None
                    if module.path is None or not module.path.is_file()
                    else file_digest(module.path),
                ]
                for module in modules
            ]
        )
    )


def resolve_recorded(
    resolver: ModuleCompiler, path: Path
) -> list[tuple[CompiledModule, bool, bool, tuple[str | None, ...], str | None]]:
    """Resolve the recorded imports again, in their recorded order; return
    each current resolution with its recorded native, missing, locations, and digest."""
    entries = []
    for item in json.loads(path.read_text()):
        name, native, missing, locations = item[:4]
        digest = item[4] if len(item) > 4 else None
        entries.append(
            (
                resolver.resolve(name, None, None),
                native,
                missing,
                tuple(locations),
                digest,
            )
        )
    return entries


def resolves_as_recorded(resolver: ModuleCompiler, path: Path) -> bool:
    return all(
        module.native == native
        and module.missing == missing
        and tuple(file_state(p) for p in module.namespace_paths) == locations
        and (
            None
            if module.path is None or not module.path.is_file()
            else file_digest(module.path)
        )
        == digest
        for module, native, missing, locations, digest in resolve_recorded(
            resolver, path
        )
    )


def builtins_object(toolchain: Toolchain) -> Path:
    """The compiled builtins unit every main unit links with, from the cache
    when unchanged: native code without optimization, bitcode with it."""
    with Graph(1) as graph:
        build = Build(graph, toolchain)
        unit_object = build.object(build.builtins())
        graph.run(unit_object)
    return unit_object.path("unit.o")


def cached_builtins_interface() -> BuiltinsInterface:
    """The builtins interface, from the cache when unchanged."""
    token = _recording.set(None)
    try:
        with Graph(1) as graph:
            build = Build(graph, Toolchain())
            graph.run(build.builtins())
            return build.interface()
    finally:
        _recording.reset(token)


def compile_program(
    source: str,
    filename: str = "<unknown>",
    *,
    roots: tuple[Path, ...] | None = None,
    package: str | None = None,
) -> Program:
    """Compile the main module through the IR1 phases as its own unit,
    against the builtins unit.

    ``roots`` is the module search path; ``package`` is the package of a main
    module run with ``-m``, for its relative imports."""
    interface = cached_builtins_interface()
    with Graph(1) as graph:
        build = Build(graph, Toolchain())
        graph.run(build.builtins())
        program, _ = compile_unit(
            source,
            filename,
            interface,
            roots=roots,
            package=package,
            resolve_import_interface=lambda dep: build.resolve_import_interface(
                "__main__", dep, roots, package
            ),
        )
    return program


def emit_llvm(
    source: str,
    filename: str = "<unknown>",
    *,
    roots: tuple[Path, ...] | None = None,
    package: str | None = None,
) -> str:
    """The LLVM module of the main module's unit."""
    return generate(compile_program(source, filename, roots=roots, package=package))


def link_llvm(
    llvm: str,
    output: Path,
    *,
    clang: str = "clang",
    rustc: str = "rustc",
    optimize: int = 1,
    lto: bool = False,
    arch: str | None = None,
    opt: Iterable[str] | str = (),
) -> Path:
    """Compile a main unit's LLVM, link it with the builtins unit and the
    Rust runtime, and replace ``output`` only on success.

    ``optimize`` selects the generated code's level (0 through 3, default 1). ``lto``
    independently requests full LTO across the program and Rust runtime."""
    toolchain = Toolchain(clang, rustc, optimize, lto, arch, opt)
    toolchain.require_target()
    with Graph() as graph:
        build = Build(graph, toolchain)
        source = graph.text("main unit LLVM", llvm)

        def action(inputs, directory):
            (directory / "unit.ll").write_text(llvm)

        main = graph.step(
            "main unit",
            lambda: Step(
                graph,
                "main unit",
                inputs=[source],
                settings={"step": "llvm"},
                outputs=("unit.ll",),
                action=action,
            ),
        )
        link = build.executable([build.builtins(), main])
        graph.run(link)
    return _install(link.path("program"), Path(output))


def _install(artifact: Path, output: Path) -> Path:
    """Place a cached executable at ``output``, replacing it atomically."""
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.{os.getpid()}.tmp")
    shutil.copy2(artifact, temporary)
    os.replace(temporary, output)
    return output


def build_executable(
    source: str,
    filename: str,
    *,
    roots: tuple[Path, ...] | None,
    package: str | None = None,
    toolchain: Toolchain | None = None,
    source_path: Path | None = None,
    workers: int | None = None,
) -> Path:
    """The cached executable of a program: every unit built or reused, then
    linked. Returns the path inside the cache. ``workers`` bounds how many
    steps run at once."""
    toolchain = toolchain or Toolchain()
    toolchain.require_target()
    with Graph(workers) as graph:
        build = Build(graph, toolchain)
        source_node = (
            graph.file(source_path)
            if source_path is not None
            else graph.text("main source", source)
        )
        main = build.main(source_node, source, filename, roots, package)
        link = build.executable([build.builtins(), main])
        graph.run(link)
    return link.path("program")


def build_dynamic_module(
    module_name: str,
    *,
    roots: tuple[Path, ...],
    package_paths: dict[str, tuple[str, ...]] | None = None,
    toolchain: Toolchain | None = None,
    workers: int | None = None,
) -> Path | None:
    """Compile ``module_name`` (and any uncached static dependencies) into
    individual single-module FASL shared objects in the FASL cache."""
    toolchain = toolchain or Toolchain()
    toolchain.require_target()
    interface = cached_builtins_interface()
    if module_name in interface.modules:
        return None
    compiler = import_resolver(f"<{module_name}>", interface, None, roots, None)
    if package_paths:
        for pkg_name, pkg_dirs in package_paths.items():
            if pkg_name not in compiler.modules:
                compiler.modules[pkg_name] = CompiledModule(
                    pkg_name, None, "", namespace_paths=pkg_dirs
                )
    parts = module_name.split(".")
    descriptor = None
    for end in range(1, len(parts) + 1):
        descriptor = compiler.resolve(".".join(parts[:end]), None, None)
    if (
        descriptor is None
        or descriptor.missing
        or descriptor.native
        or descriptor.path is None
    ):
        return None
    target_dest = fasl_cache_path(
        mule_hash(), toolchain.optimize, descriptor.name, descriptor.path
    )
    if target_dest is not None and target_dest.is_file():
        return target_dest
    with Graph(workers) as graph:
        build = Build(graph, toolchain, fasl_mode=True)
        target = build.add_fasl_root(descriptor, compiler.roots, None)
        for desc in compiler.pending:
            if (
                desc.name == descriptor.name
                or desc.missing
                or desc.native
                or desc.path is None
            ):
                continue
            dest = fasl_cache_path(
                mule_hash(), toolchain.optimize, desc.name, desc.path
            )
            if dest is not None and dest.is_file():
                continue
            build.add_fasl_root(desc, compiler.roots, None)
        graph.run(target)
    if target_dest is not None and target_dest.is_file():
        return target_dest
    return target.path("module.so")


def build(
    source: str | Path,
    output: str | Path,
    *,
    llvm_output: str | Path | None = None,
    clang: str = "clang",
    rustc: str = "rustc",
    optimize: int = 1,
    lto: bool = False,
    arch: str | None = None,
    opt: Iterable[str] | str = (),
    workers: int | None = None,
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
    roots = search_path(source_path.resolve().parent)
    source_text = read_source(source_path)
    executable = build_executable(
        source_text,
        str(source_path),
        roots=roots,
        toolchain=Toolchain(clang, rustc, optimize, lto, arch, opt),
        source_path=source_path.resolve(),
        workers=workers,
    )
    if llvm_output is not None:
        llvm_path.parent.mkdir(parents=True, exist_ok=True)
        llvm_path.write_text(emit_llvm(source_text, str(source_path), roots=roots))
    return _install(executable, output_path)
