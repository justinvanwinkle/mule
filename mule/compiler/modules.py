"""Statically link Python modules, preserving separate runtime namespaces."""

from dataclasses import dataclass
from pathlib import Path

from . import ir
from .frontend import lower

STDLIB = Path(__file__).resolve().parents[2] / "stdlib"


@dataclass(frozen=True)
class CompiledModule:
    name: str
    path: Path
    entry: str


class ModuleCompiler:
    def __init__(self, filename):
        self.roots = (Path(filename).resolve().parent, STDLIB / "bootstrap", STDLIB)
        self.modules = {}
        self.functions = []

    def path_for(self, name):
        # Built-in implementation sources precede user modules. In particular,
        # source-local code must not acquire the private OS primitive bindings.
        if name in ("sys", "_mule_os"):
            path = STDLIB / f"{name}.py"
            return path if path.is_file() else None
        relative = Path(*name.split("."))
        parent, _, child = name.rpartition(".")
        if parent in self.modules:
            package = self.modules[parent]
            if package.path.name != "__init__.py":
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
                if candidate.is_file()
            ),
            None,
        )

    def import_name(self, lowerer, node):
        if not node.level:
            return node.module
        descriptor = self.modules.get(lowerer.namespace)
        if descriptor is None:
            lowerer.error(
                "relative import has no known parent package", node, unsupported=True
            )
        package = (
            descriptor.name
            if descriptor.path.name == "__init__.py"
            else descriptor.name.rpartition(".")[0]
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
        path = self.path_for(name)
        if path is None:
            lowerer.error(
                f"no compilable Python module named '{name}'", node, unsupported=True
            )
        prefix = f"mule_import_{len(self.modules)}_"
        descriptor = CompiledModule(name, path, prefix + "mule_module")
        # Reserve identity before lowering so circular imports share one cache.
        self.modules[name] = descriptor
        module = lower(
            path.read_text(encoding="utf-8"),
            str(path),
            imports=self,
            prefix=prefix,
            namespace=name,
        )
        self.functions.extend(module.functions)
        return descriptor

    def compile(self, source, filename):
        main = lower(source, filename, imports=self)
        module = ir.Module([*main.functions, *self.functions], main.entry)
        ir.verify(module)
        return module


def compile_modules(source, filename="<unknown>"):
    return ModuleCompiler(filename).compile(source, filename)
