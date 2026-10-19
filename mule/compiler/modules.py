"""Statically link Python modules, preserving separate runtime namespaces."""

import os
import sys
import tomllib
from collections import deque
from dataclasses import dataclass
from pathlib import Path

from mule import ast

from .ctype import UNKNOWN, ValueType
from .inputs import directory_entries, directory_exists, read_source, source_exists
from .ir1opt import constant_literal
from .ir1tran_lambda import lambda_blocks
from .ir1util import class_method_definitions, known_calls, made_lambda
from .locall import _ImportBindingStores, _PROTOCOL_PREFIXES, global_bindings
from .main import Compilation, compile_toplevel, ir1_phases
from .node import Combination, GlobalVar, LeafVisitor, NodeVisitor, Ref, linked

STDLIB = Path(__file__).resolve().parents[2] / "stdlib"
NATIVE_MODULES = {
    "_mule_native": "mule_native_module_init",
    "_posix": "mule_posix_module_init",
    "_time": "mule_time_module_init",
    "builtins": "mule_builtins_module_init",
}


class _BuiltinNameLiteral(ast.Visitor):
    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node: ast.TextLiteral):
        return node.value


class _GlobalVarName(NodeVisitor, LeafVisitor):
    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(self)

    def visit_GlobalVar(self, leaf: GlobalVar):
        return leaf.name


class _ImportedModuleName(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if node.fun_info is not None and node.fun_info.name == "%import":
            return node.info.module_name
        if node.fun_info is not None and node.fun_info.name == "%import_module":
            return node.info
        return None


_BUILTIN_NAME_LITERAL = _BuiltinNameLiteral()
_GLOBAL_VAR_NAME = _GlobalVarName()
_IMPORTED_MODULE_NAME = _ImportedModuleName()


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
    # Namespace packages have no source body and may span several search roots.
    namespace_paths: tuple[str, ...] = ()

    @property
    def package_paths(self):
        if self.namespace_paths:
            return self.namespace_paths
        if self.path is not None and self.path.name == "__init__.py":
            return (str(self.path.parent),)
        return ()

    def source(self):
        return "" if self.path is None else read_source(self.path)

    def import_info(self):
        from .ir1tran import ImportInfo

        return ImportInfo(
            self.name,
            self.entry,
            None if self.path is None else str(self.path),
            self.native,
            self.missing,
            self.namespace_paths,
        )


def search_path(first):
    """Module roots in the order Python's sys.path gives them: the script's
    directory (or the working directory), PYTHONPATH, the standard library, then
    the interpreter's configured source paths. Mule's stdlib takes precedence
    over host standard-library sources. Runtime additions to sys.path participate
    in resolution, including when the compiler itself runs under Mule."""
    extra = [
        Path(entry)
        for entry in os.environ.get("PYTHONPATH", "").split(os.pathsep)
        if entry
    ]
    roots = (Path(first), *extra, STDLIB / "bootstrap", STDLIB, *map(Path, sys.path))
    return tuple(dict.fromkeys(root.resolve() for root in roots))


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
        self.modules: dict[str, CompiledModule] = {}
        # Lambdas and facts of every imported module, in compilation order.
        self.lambdas = []
        self.facts = {}
        self.exported_types: dict[tuple[str, str], ValueType] = {}
        self.storage_methods: dict[tuple[str, str], ValueType] = {}
        self.builtin_return_types: dict[str, ValueType] = {}
        # Imported modules awaiting compilation. An importer needs only the
        # descriptor, so modules compile one after another rather than nested
        # inside their importer, and stack depth does not grow with import
        # chains.
        self.pending: deque[CompiledModule] = deque()
        # Every module name resolved, in first-request order.
        self.requested: dict[str, None] = {}
        # Names resolved without an import statement: literal-import scans,
        # project patterns and builtin requirements. They stay linked even
        # when no import of them survives optimization.
        self.scanned: set[str] = set()
        self.project_modules = ()

    def configure_project(self):
        """Read explicit image dependencies from the nearest project file."""
        for directory in (self.roots[0], *self.roots[0].parents):
            if directory != self.roots[0] and self.roots[0].relative_to(
                directory
            ).parts[0] in ("examples", "conformance", "corpus", "old", "perf"):
                continue
            project = directory / "pyproject.toml"
            if not source_exists(project):
                continue
            tool = tomllib.loads(read_source(project)).get("tool", {})
            settings = tool.get("mule", {})
            names = settings.get("include_modules", [])
            patterns = []
            try:
                for name in names:
                    package = name.endswith(".*")
                    prefix = name.removesuffix(".*") if package else name
                    if not prefix or not all(
                        part.isidentifier() for part in prefix.split(".")
                    ):
                        raise ValueError(f"invalid module dependency {name!r}")
                    patterns.append(prefix + "." if package else prefix)
            except (AttributeError, TypeError) as error:
                raise ValueError(
                    "tool.mule.include_modules must contain module names or package.* patterns"
                ) from error
            self.project_modules = tuple(patterns)
            if self.project_modules:
                pytest_paths = (
                    tool.get("pytest", {}).get("ini_options", {}).get("pythonpath", [])
                )
                project_roots = tuple(
                    (directory / entry).resolve() for entry in (".", *pytest_paths)
                )
                self.roots = tuple(
                    dict.fromkeys((self.roots[0], *project_roots, *self.roots[1:]))
                )
            return

    def path_for(self, name):
        return self.location_for(name)[0]

    def location_for(self, name):
        if name in NATIVE_MODULES or name == "__main__":
            return None, ()
        # The system module and its private decoder precede source-local modules.
        if name in (
            "sys",
            "_mule_codecs",
            "_sysconfigdata__linux_x86_64-linux-gnu",
        ):
            path = STDLIB / f"{name}.py"
            return (path, ()) if source_exists(path) else (None, ())
        relative = Path(*name.split("."))
        parent, _, child = name.rpartition(".")
        if parent in self.modules:
            package = self.modules[parent]
            if not package.package_paths:
                return None, ()
            roots = tuple(Path(path) for path in package.package_paths)
            relative = Path(child)
        else:
            roots = self.roots
        namespace_paths = []
        for root in roots:
            for candidate in (
                root / relative / "__init__.py",
                root / relative.with_suffix(".py"),
            ):
                if source_exists(candidate):
                    return candidate, ()
            directory = root / relative
            if directory_exists(directory):
                namespace_paths.append(str(directory))
        return None, tuple(namespace_paths)

    def package_of(self, namespace):
        """The package relative imports in a module resolve against, or None."""
        if namespace == "__main__":
            return self.main_package or None
        descriptor = self.modules.get(namespace)
        if descriptor is None:
            return None
        if descriptor.package_paths:
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
        # What a unit's output depends on from its imports: how each resolved.
        self.requested.setdefault(name, None)
        if lowerer is None:
            self.scanned.add(name)
        if name in self.modules:
            return self.modules[name]
        if name in NATIVE_MODULES:
            descriptor = CompiledModule(name, None, NATIVE_MODULES[name], native=True)
            self.modules[name] = descriptor
            return descriptor
        path, namespace_paths = self.location_for(name)
        if path is None and not namespace_paths:
            descriptor = CompiledModule(name, None, "", missing=True)
            self.modules[name] = descriptor
            return descriptor
        # Derived from the module's name alone, so a module's symbols do not
        # depend on which modules were compiled before it. Module names cannot
        # contain ":", so prefixes are unique.
        protocol_module = self.primitives is None or name.startswith("_")
        unit_prefix = self.prefix if protocol_module else ""
        prefix = f"{unit_prefix}mule_import_{name}:"
        descriptor = CompiledModule(
            name,
            path,
            prefix + "mule_module",
            prefix=prefix,
            namespace_paths=namespace_paths,
        )
        # Reserve identity before conversion so circular imports share one cache.
        self.modules[name] = descriptor
        if protocol_module:
            self.pending.append(descriptor)
        if name == "sysconfig":
            self.resolve("_sysconfigdata__linux_x86_64-linux-gnu", None, None)
        return descriptor

    def include_patterns(self, patterns):
        for pattern in patterns:
            name = pattern.removesuffix(".")
            if not name or not all(part.isidentifier() for part in name.split(".")):
                continue
            parts = name.split(".")
            for end in range(1, len(parts) + 1):
                descriptor = self.resolve(".".join(parts[:end]), None, None)
            if not pattern.endswith(".") or not descriptor.package_paths:
                continue
            for location in descriptor.package_paths:
                directory = Path(location)
                for entry in directory_entries(directory):
                    path = directory / entry
                    child = path.stem if path.suffix == ".py" else path.name
                    if child == "__init__" or not child.isidentifier():
                        continue
                    if path.is_file() and path.suffix != ".py":
                        continue
                    candidate = name + "." + child
                    file, locations = self.location_for(candidate)
                    if file is not None or locations:
                        self.resolve(candidate, None, None)

    def record_types(self, namespace: str, compilation: Compilation) -> None:
        facts = compilation.facts
        if facts is None:
            return
        for pub_name, lam in (
            global_bindings([compilation.entry]).get(namespace, {}).items()
        ):
            if lam in facts:
                ret_type = facts[lam].return_type
                if ret_type is not UNKNOWN:
                    self.exported_types[namespace, pub_name] = ret_type
        class_names = {
            linked(call.info, call, "class definition").identity: linked(
                call.info, call, "class definition"
            ).public_name
            for call in known_calls(compilation.entry, "%make_class")
        }
        for class_id, methods in class_method_definitions(compilation.entry).items():
            class_name = class_names.get(class_id, class_id)
            for method_name, target in methods.items():
                if target in facts:
                    ret_type = facts[target].return_type
                    if ret_type is not UNKNOWN:
                        self.storage_methods[class_name, method_name] = ret_type
        import_bindings: dict[str, tuple[str, str]] = {}
        imp_visitor = _ImportBindingStores(import_bindings)
        for block in lambda_blocks(compilation.entry):
            for node in block.nodes():
                node.accept(imp_visitor)
        for call in known_calls(compilation.entry, "_install_builtin"):
            if len(call.args) != 2:
                continue
            literal = constant_literal(call.args[0])
            builtin_name = (
                None if literal is None else literal.accept(_BUILTIN_NAME_LITERAL)
            )
            target_name = (
                call.args[1].use().accept(_GLOBAL_VAR_NAME)
                if len(call.args[1].uses) == 1
                else None
            )
            if builtin_name is None or target_name is None:
                continue
            ret_type = self.exported_types.get((namespace, target_name), UNKNOWN)
            if ret_type is UNKNOWN and target_name in import_bindings:
                ret_type = self.exported_types.get(
                    import_bindings[target_name], UNKNOWN
                )
            if ret_type is not UNKNOWN:
                self.builtin_return_types[builtin_name] = ret_type

    def compile_pending(self):
        """Compile every module imported so far, and those they import."""
        if (
            not self.storage_methods
            and self.primitives is None
            and not self.prefix.startswith(_PROTOCOL_PREFIXES)
        ):
            from .sequence_protocol import stdlib_type_summary

            exported, methods, builtins = stdlib_type_summary()
            self.exported_types.update(exported)
            self.storage_methods.update(methods)
            self.builtin_return_types.update(builtins)
        pending_compilations: list[tuple[CompiledModule, Compilation]] = []
        while self.pending:
            descriptor = self.pending.popleft()
            compilation = compile_toplevel(
                descriptor.source(),
                str(descriptor.path),
                imports=self,
                prefix=descriptor.prefix,
                namespace=descriptor.name,
                primitives=self.primitives,
            )
            pending_compilations.append((descriptor, compilation))
        if not pending_compilations:
            return
        by_name = {desc.name: (desc, comp) for desc, comp in pending_compilations}
        deps: dict[str, list[str]] = {}
        for desc, comp in pending_compilations:
            mod_deps: list[str] = []
            for lam in comp.lambdas:
                for block in lambda_blocks(lam):
                    for node in block.nodes():
                        imp_name = node.accept(_IMPORTED_MODULE_NAME)
                        if (
                            imp_name is not None
                            and imp_name in by_name
                            and imp_name != desc.name
                            and imp_name not in mod_deps
                        ):
                            mod_deps.append(imp_name)
            deps[desc.name] = mod_deps
        visited: set[str] = set()
        ordered: list[tuple[CompiledModule, Compilation]] = []

        def visit(name: str) -> None:
            if name in visited:
                return
            visited.add(name)
            for dep in deps.get(name, ()):
                visit(dep)
            ordered.append(by_name[name])

        for desc, _ in pending_compilations:
            visit(desc.name)
        for descriptor, compilation in ordered:
            compilation.imported_function_types = self.exported_types
            compilation.storage_methods = self.storage_methods
            compilation.builtin_return_types = self.builtin_return_types
            ir1_phases(compilation)
            if compilation.facts is None:
                raise RuntimeError(f"{descriptor.path} was not analyzed")
            self.record_types(descriptor.name, compilation)
        for descriptor, compilation in pending_compilations:
            self.lambdas.extend(compilation.lambdas)
            self.facts.update(compilation.facts)

    def compile(self, source, filename):
        """Compile the main module and every module it imports; imported
        modules accumulate on the compiler. Returns the main module's
        ``Compilation``."""
        if (
            not self.storage_methods
            and self.primitives is None
            and not self.prefix.startswith(_PROTOCOL_PREFIXES)
        ):
            from .sequence_protocol import stdlib_type_summary

            exported, methods, builtins = stdlib_type_summary()
            self.exported_types.update(exported)
            self.storage_methods.update(methods)
            self.builtin_return_types.update(builtins)
        compilation = compile_toplevel(source, filename, imports=self)
        self.compile_pending()
        compilation.imported_function_types = self.exported_types
        compilation.storage_methods = self.storage_methods
        compilation.builtin_return_types = self.builtin_return_types
        ir1_phases(compilation)
        self.record_types("__main__", compilation)
        return compilation


def compile_modules(
    source,
    filename="<unknown>",
    *,
    roots=None,
    package=None,
    compiled: dict[str, CompiledModule] | None = None,
):
    """Compile a program's modules. ``compiled`` are modules another unit
    already compiled, by name; an import of one reuses it. Returns the main
    compilation and the module compiler holding the imported modules."""
    compiler = ModuleCompiler(filename, roots=roots, package=package)
    if compiled is not None:
        compiler.modules.update(compiled)
    return compiler.compile(source, filename), compiler
