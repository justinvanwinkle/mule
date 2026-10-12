"""Statically link Python modules, preserving separate runtime namespaces."""

import os
import sysconfig
from collections import deque
from dataclasses import dataclass
from pathlib import Path

from .inputs import read_source, source_exists
from .main import compile_source

STDLIB = Path(__file__).resolve().parents[2] / "stdlib"
NATIVE_MODULES = {
    "_mule_native": "mule_native_module_init",
    "builtins": "mule_builtins_module_init",
}


@dataclass(frozen=True)
class CompiledModule:
    name: str
    path: Path | None
    entry: str
    native: bool = False
    # No source exists: importing it raises ModuleNotFoundError at run time,
    # where code such as ``try: import nt`` expects to handle it.
    missing: bool = False
    # The symbol prefix of a compiled source module.
    prefix: str = ""


def search_path(first):
    """Module roots in the order Python's sys.path gives them: the script's
    directory (or the working directory), PYTHONPATH, the standard library, then
    the host environment's site-packages. Mule's stdlib replaces CPython's, and
    third-party packages are compiled from the same source CPython would import."""
    extra = [
        Path(entry)
        for entry in os.environ.get("PYTHONPATH", "").split(os.pathsep)
        if entry
    ]
    site = dict.fromkeys(
        Path(sysconfig.get_path(name)) for name in ("purelib", "platlib")
    )
    return (Path(first).resolve(), *extra, STDLIB / "bootstrap", STDLIB, *site)


def find_module(name, roots):
    """The source file for a module or package named on the command line, and
    the package a `-m` main module belongs to. A package runs its `__main__`."""
    relative = Path(*name.split("."))
    for root in roots:
        package = root / relative / "__init__.py"
        if source_exists(package):
            main = root / relative / "__main__.py"
            if not source_exists(main):
                raise ImportError(
                    f"No module named {name}.__main__; '{name}' is a package and cannot be directly executed"
                )
            return main, name
        module = root / relative.with_suffix(".py")
        if source_exists(module):
            return module, name.rpartition(".")[0]
    raise ImportError(f"No module named {name}")


class ModuleCompiler:
    def __init__(
        self, filename, *, prefix="", primitives=None, roots=None, package=None
    ):
        if roots is None:
            roots = (Path(filename).resolve().parent, STDLIB / "bootstrap", STDLIB)
        self.roots = tuple(roots)
        # The package of a main module run with `-m`, for its relative imports.
        self.main_package = package
        self.prefix = prefix
        # Modules imported by a compiled protocol share its physical primitives.
        self.primitives = primitives
        self.modules = {}
        # Lambdas and facts of every imported module, in compilation order.
        self.lambdas = []
        self.facts = {}
        # Imported modules awaiting compilation. An importer needs only the
        # descriptor, so modules compile one after another rather than nested
        # inside their importer, and stack depth does not grow with import
        # chains.
        self.pending: deque[CompiledModule] = deque()

    def path_for(self, name):
        if name in NATIVE_MODULES:
            return None
        # The system module and its private decoder precede source-local modules.
        if name in ("sys", "_mule_codecs"):
            path = STDLIB / f"{name}.py"
            return path if source_exists(path) else None
        relative = Path(*name.split("."))
        parent, _, child = name.rpartition(".")
        if parent in self.modules:
            package = self.modules[parent]
            if package.path is None or package.path.name != "__init__.py":
                return None
            roots = (package.path.parent,)
            relative = Path(child)
        else:
            roots = self.roots
        return next(
            (
                candidate
                for root in roots
                for candidate in (
                    root / relative / "__init__.py",
                    root / relative.with_suffix(".py"),
                )
                if source_exists(candidate)
            ),
            None,
        )

    def package_of(self, namespace):
        """The package relative imports in a module resolve against, or None."""
        if namespace == "__main__":
            return self.main_package or None
        descriptor = self.modules.get(namespace)
        if descriptor is None or descriptor.path is None:
            return None
        if descriptor.path.name == "__init__.py":
            return descriptor.name
        return descriptor.name.rpartition(".")[0]

    def import_name(self, lowerer, node):
        if not node.level:
            return node.module
        package = self.package_of(lowerer.namespace)
        if package is None:
            return lowerer.error(
                "relative import has no known parent package", node, unsupported=True
            )
        parts = package.split(".") if package else []
        if node.level > len(parts):
            lowerer.error(
                "relative import beyond top-level package", node, unsupported=True
            )
        base = ".".join(parts[: len(parts) - node.level + 1])
        return base + "." + node.module if node.module else base

    def resolve(self, name, lowerer, node):
        if name in self.modules:
            return self.modules[name]
        if name in NATIVE_MODULES:
            descriptor = CompiledModule(name, None, NATIVE_MODULES[name], native=True)
            self.modules[name] = descriptor
            return descriptor
        path = self.path_for(name)
        if path is None:
            descriptor = CompiledModule(name, None, "", missing=True)
            self.modules[name] = descriptor
            return descriptor
        # Derived from the module's name alone, so a module's symbols do not
        # depend on which modules were compiled before it. Module names cannot
        # contain ":", so prefixes are unique.
        prefix = f"{self.prefix}mule_import_{name}:"
        descriptor = CompiledModule(name, path, prefix + "mule_module", prefix=prefix)
        # Reserve identity before conversion so circular imports share one cache.
        self.modules[name] = descriptor
        self.pending.append(descriptor)
        return descriptor

    def compile_pending(self):
        """Compile every module imported so far, and those they import."""
        while self.pending:
            descriptor = self.pending.popleft()
            compilation = compile_source(
                read_source(descriptor.path),
                str(descriptor.path),
                imports=self,
                prefix=descriptor.prefix,
                namespace=descriptor.name,
                primitives=self.primitives,
            )
            self.lambdas.extend(compilation.lambdas)
            self.facts.update(compilation.facts)

    def compile(self, source, filename):
        """Compile the main module and every module it imports; imported
        modules accumulate on the compiler. Returns the main module's
        ``Compilation``."""
        compilation = compile_source(source, filename, imports=self)
        self.compile_pending()
        return compilation


def compile_modules(
    source, filename="<unknown>", *, roots=None, package=None, compiled=None
):
    """Compile a program's modules. ``compiled`` are modules another unit
    already compiled, by name; an import of one reuses it. Returns the main
    compilation and the module compiler holding the imported modules."""
    compiler = ModuleCompiler(filename, roots=roots, package=package)
    compiler.modules.update(compiled or {})
    return compiler.compile(source, filename), compiler
