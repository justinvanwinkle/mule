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

    def resolve(self, name, lowerer, node):
        if name in self.modules:
            return self.modules[name]
        relative = Path(*name.split("."))
        path = next(
            (
                candidate
                for root in self.roots
                for candidate in (
                    root / relative / "__init__.py",
                    root / relative.with_suffix(".py"),
                )
                if candidate.is_file()
            ),
            None,
        )
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
