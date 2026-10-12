"""Statically link Python modules, preserving separate runtime namespaces."""

import os
import sysconfig
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
        # Lambdas and facts of every imported module, in import order.
        self.lambdas = []
        self.facts = {}

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
        prefix = f"{self.prefix}mule_import_{len(self.modules)}_"
        descriptor = CompiledModule(name, path, prefix + "mule_module")
        # Reserve identity before conversion so circular imports share one cache.
        self.modules[name] = descriptor
        compilation = compile_source(
            read_source(path),
            str(path),
            imports=self,
            prefix=prefix,
            namespace=name,
            primitives=self.primitives,
        )
        self.lambdas.extend(compilation.lambdas)
        self.facts.update(compilation.facts)
        return descriptor

    def compile(self, source, filename):
        """Compile the main module; imported modules accumulate on the
        compiler. Returns the main module's ``Compilation``."""
        return compile_source(source, filename, imports=self)


def compile_modules(source, filename="<unknown>", *, roots=None, package=None):
    """Compile a program's modules. Returns the main compilation and the
    module compiler holding the imported modules."""
    compiler = ModuleCompiler(filename, roots=roots, package=package)
    return compiler.compile(source, filename), compiler
