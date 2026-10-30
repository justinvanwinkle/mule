"""Statically link Python modules, preserving separate runtime namespaces."""

import os
import sys
import tomllib
from collections import deque
from dataclasses import dataclass
from pathlib import Path

from mule import ast

from .constraint import BOOTSTRAP_BUILTIN_TARGETS, BOOTSTRAP_CLASS_TARGETS
from .ctype import (
    ANY_CLASS_TYPE,
    BUILTIN_CLASS_TYPES,
    DICT_TYPE,
    EMPTY_DICT_TYPE,
    EMPTY_FROZENSET_TYPE,
    EMPTY_LIST_TYPE,
    EMPTY_SET_TYPE,
    FROZENSET_TYPE,
    LIST_TYPE,
    SET_TYPE,
    STORAGE_CLASS_TYPES,
    UNKNOWN,
    InstanceType,
    ValueType,
)
from .fndb import CLASS_LOAD, IMPORT
from .inputs import directory_entries, directory_exists, read_source, source_exists
from .ir1opt import constant_literal
from .ir1tran_lambda import lambda_blocks
from .ir1util import class_method_definitions, known_calls, made_function
from .locall import (
    _CLASS_BASE_NAMES,
    _ImportBindingStores,
    collect_global_constants,
    global_bindings,
)
from .main import Compilation, compile_toplevel, ir1_phases
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    GlobalVar,
    Lambda,
    LeafVisitor,
    NodeVisitor,
    Ref,
    linked,
)
from .protocols import NATIVE_MODULE_EXPORTS, NATIVE_MODULE_TARGETS

STDLIB = Path(__file__).resolve().parents[2] / "stdlib"


def is_library_path(path: Path) -> bool:
    return path.is_relative_to(STDLIB) or "site-packages" in path.parts


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
        if node.fun_info is IMPORT:
            return linked(node.info, node, "info").module_name
        return None


_BUILTIN_NAME_LITERAL = _BuiltinNameLiteral()
_GLOBAL_VAR_NAME = _GlobalVarName()
_IMPORTED_MODULE_NAME = _ImportedModuleName()


class _SingleAssignmentValue(NodeVisitor, LeafVisitor):
    """Follow an IR value without guessing through a mutable binding."""

    def __init__(self):
        self.visited = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf):
        if leaf in self.visited or len(leaf.sets) != 1:
            return None
        self.visited.add(leaf)
        value = leaf.sets[0].value
        return value.use().accept(self) if value is not None else None


class _ClassMethodConstructor(_SingleAssignmentValue):
    def __init__(self, imports, shadowed):
        super().__init__()
        self.imports = imports
        self.shadowed = shadowed

    def visit_GlobalVar(self, leaf):
        return self.imports.get(leaf.name) == ("_descriptors", "classmethod")

    def visit_Combination(self, node):
        if (
            node.fun_info is CLASS_LOAD
            and (node.info.identity, node.info.name) not in self.shadowed
        ):
            return self.imports.get(node.info.name) == (
                "_descriptors",
                "classmethod",
            )
        return False


class _ClassMethodFunction(_SingleAssignmentValue):
    def __init__(self, imports, shadowed):
        super().__init__()
        self.imports = imports
        self.shadowed = shadowed

    def visit_Combination(self, node):
        if (
            node.kind in (CallKind.FULL, CallKind.GUARDED)
            and len(node.args) == 1
            and not node.info.keywords
            and node.fun.use().accept(
                _ClassMethodConstructor(self.imports, self.shadowed)
            )
        ):
            return made_function(node.args[0].use())
        return None


def _protocol_class_methods(compilation):
    """Recover canonical descriptor constructors in compiler-owned protocols.

    These class bodies execute during bootstrap, before builtin call targets
    are published. Arbitrary decorators and class-local shadowing retain the
    ordinary unknown result. Inherited descriptors need their own receiver
    derivation and are deliberately not copied into subclass summaries.
    """
    if not compilation.entry.is_protocol:
        return {}
    imports = {}
    visitor = _ImportBindingStores(imports)
    for block in lambda_blocks(compilation.entry):
        for node in block.nodes():
            node.accept(visitor)
    stores = list(known_calls(compilation.entry, "%class_store"))
    shadowed = {(node.info.identity, node.info.name) for node in stores}
    methods = {}
    for node in stores:
        target = node.args[1].use().accept(_ClassMethodFunction(imports, shadowed))
        if target is not None:
            methods.setdefault(node.info.identity, {})[node.info.name] = target
    return methods


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


def builtin_installs(compilation: Compilation, namespace: str):
    """Each ``_install_builtin(name, function)`` call in ``compilation``'s
    top level, as the builtin name and the export keys that may define the
    function: the installing module's own binding, then the module it was
    imported from."""
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
        keys = ((namespace, target_name),)
        if target_name in import_bindings:
            keys += (import_bindings[target_name],)
        yield builtin_name, keys


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
        self.exported_types: dict[tuple[str, str], ValueType] = dict(
            NATIVE_MODULE_EXPORTS
        )
        self.exported_targets: dict[
            tuple[str, str], Lambda | ClassConstructorTarget | ForeignCallTarget
        ] = dict(NATIVE_MODULE_TARGETS)
        self.storage_methods: dict[tuple[str, str], ValueType] = {}
        self.builtin_return_types: dict[str, ValueType] = {}
        self.builtin_targets: dict[
            str, Lambda | ClassConstructorTarget | ForeignCallTarget
        ] = dict(BOOTSTRAP_BUILTIN_TARGETS)
        # Builtins an installing module binds, by name, to the export keys
        # that define them; see ``expect_builtins``.
        self.expected_builtins: dict[str, tuple[tuple[str, str], ...]] = {}
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
            pytest_paths = (
                tool.get("pytest", {}).get("ini_options", {}).get("pythonpath", [])
            )
            if self.project_modules or pytest_paths:
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
        elif name == "encodings":
            for codec in (
                "encodings.aliases",
                "encodings.utf_8",
                "encodings.latin_1",
                "encodings.ascii",
            ):
                self.resolve(codec, None, None)
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
                if is_library_path(directory):
                    continue
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
            self.exported_targets[namespace, pub_name] = lam
            if lam in facts:
                ret_type = facts[lam].return_type
                if ret_type == EMPTY_LIST_TYPE:
                    ret_type = LIST_TYPE
                elif ret_type == EMPTY_DICT_TYPE:
                    ret_type = DICT_TYPE
                elif ret_type == EMPTY_SET_TYPE:
                    ret_type = SET_TYPE
                elif ret_type == EMPTY_FROZENSET_TYPE:
                    ret_type = FROZENSET_TYPE
                if ret_type is not UNKNOWN:
                    self.exported_types[namespace, pub_name] = ret_type
        class_names: dict[str, str] = {}
        class_bases: dict[str, tuple[str, ...]] = {}
        for call in known_calls(compilation.entry, "%make_class"):
            info = linked(call.info, call, "class definition")
            class_names[info.identity] = info.public_name
            class_bases[info.public_name] = (
                call.args[0].uses[0].accept(_CLASS_BASE_NAMES)
                if call.args and len(call.args[0].uses) == 1
                else ()
            )
        method_defs = class_method_definitions(compilation.entry)
        class_methods = _protocol_class_methods(compilation)
        class_fields = compilation.locall_state.get("class_fields", {})
        class_layouts = compilation.locall_state.get("class_layouts", {})
        all_class_ids = list(dict.fromkeys([*class_names, *method_defs, *class_fields]))
        for class_id in all_class_ids:
            class_name = class_names.get(class_id, class_id)
            methods = method_defs.get(class_id, {})
            for method_name, target in methods.items():
                if target in facts:
                    recv = (
                        SET_TYPE
                        if class_id == "mule_dict_set"
                        else (
                            FROZENSET_TYPE
                            if class_id == "mule_dict_frozenset"
                            else InstanceType(class_id)
                        )
                    )
                    ret_type = facts[target].method_return_type(recv)
                    if ret_type == EMPTY_LIST_TYPE:
                        ret_type = LIST_TYPE
                    elif ret_type == EMPTY_DICT_TYPE:
                        ret_type = DICT_TYPE
                    elif ret_type == EMPTY_SET_TYPE:
                        ret_type = SET_TYPE
                    elif ret_type == EMPTY_FROZENSET_TYPE:
                        ret_type = FROZENSET_TYPE
                    self.storage_methods[class_name, method_name] = ret_type
                    self.storage_methods[class_id, method_name] = ret_type
            for field_name, field_type in class_fields.get(class_id, {}).items():
                if field_type is not UNKNOWN:
                    self.storage_methods[class_name, field_name] = field_type
                    self.storage_methods[class_id, field_name] = field_type
            bases = class_bases.get(class_name, ())
            for base_name in bases:
                for (owner_name, method_name), ret_type in list(
                    self.storage_methods.items()
                ):
                    if owner_name == base_name:
                        if method_name.startswith("$class:"):
                            continue
                        self.storage_methods.setdefault(
                            (class_name, method_name), ret_type
                        )
            is_protocol = compilation.entry.is_protocol
            base_target = (
                BOOTSTRAP_CLASS_TARGETS.get(class_name) if is_protocol else None
            )
            if base_target is not None:
                base_type = base_target.base_type
                cls_type = base_target.cls_type
                records_type_origin = base_target.records_type_origin
            else:
                cls_type = (
                    BUILTIN_CLASS_TYPES.get(class_name, ANY_CLASS_TYPE)
                    if is_protocol
                    else ANY_CLASS_TYPE
                )
                constructed_type = (
                    UNKNOWN if namespace.startswith("$mule") else InstanceType(class_id)
                )
                base_type = (
                    STORAGE_CLASS_TYPES.get(class_name, constructed_type)
                    if is_protocol
                    else constructed_type
                )
                records_type_origin = False
            for method_name, target in class_methods.get(class_id, {}).items():
                result = target.derive_return_type(
                    (UNKNOWN,) * (len(target.vars) - 1),
                    receiver_type=cls_type,
                )
                if result is not UNKNOWN:
                    self.storage_methods[class_name, "$class:" + method_name] = result
                    self.storage_methods[class_id, "$class:" + method_name] = result
            cls_target = ClassConstructorTarget(
                base_type=base_type,
                cls_type=cls_type,
                new_target=methods.get("__new__"),
                init_target=methods.get("__init__"),
                records_type_origin=records_type_origin,
                layout_type=class_layouts.get(class_id, UNKNOWN),
            )
            self.exported_targets[namespace, class_name] = cls_target
        for leaf, vtype in collect_global_constants(compilation.lambdas).items():
            self.exported_types.setdefault((namespace, leaf.name), vtype)
        import_bindings: dict[str, tuple[str, str]] = {}
        reexports = _ImportBindingStores(import_bindings)
        for block in lambda_blocks(compilation.entry):
            for node in block.nodes():
                node.accept(reexports)
        for local_name, src_key in import_bindings.items():
            if src_key in self.exported_targets:
                self.exported_targets.setdefault(
                    (namespace, local_name), self.exported_targets[src_key]
                )
            if src_key in self.exported_types:
                self.exported_types.setdefault(
                    (namespace, local_name), self.exported_types[src_key]
                )
        for builtin_name, keys in builtin_installs(compilation, namespace):
            self.install_builtin(builtin_name, keys)
        # Builtins installed by a module analyzed later but defined here are
        # known from this point on, to the modules analyzed after this one.
        for builtin_name, keys in self.expected_builtins.items():
            if keys[-1][0] == namespace:
                self.install_builtin(builtin_name, keys)

    def expect_builtins(self, compilation: Compilation, namespace: str) -> None:
        """Record the builtins ``compilation`` installs, before the modules
        defining them are analyzed, so each becomes a known call target as
        soon as its defining module has been analyzed."""
        self.expected_builtins.update(builtin_installs(compilation, namespace))

    def install_builtin(self, builtin_name: str, keys) -> None:
        """Bind builtin ``builtin_name`` to the first recorded export among
        ``keys``, the installing module's binding and then its import source."""
        target_obj = next(
            (t for key in keys if (t := self.exported_targets.get(key)) is not None),
            None,
        )
        if target_obj is not None:
            self.builtin_targets[builtin_name] = target_obj
        ret_type = next(
            (
                t
                for key in keys
                if (t := self.exported_types.get(key, UNKNOWN)) is not UNKNOWN
            ),
            UNKNOWN,
        )
        if ret_type is UNKNOWN and target_obj is not None:
            ret_type = target_obj.summary_return_type()
        if ret_type is UNKNOWN:
            ret_type = self.storage_methods.get((keys[0][1], "__new__"), UNKNOWN)
        if ret_type is not UNKNOWN:
            self.builtin_return_types[builtin_name] = ret_type

    def compile_pending(self):
        """Compile every module imported so far, and those they import."""
        if self.primitives is None and (
            not self.storage_methods or not self.exported_targets
        ):
            from .driver import cached_builtins_interface

            interface = cached_builtins_interface()
            if not self.storage_methods:
                self.exported_types.update(interface.exported_types)
                self.storage_methods.update(interface.storage_methods)
                self.builtin_return_types.update(interface.builtin_return_types)
            if not self.exported_targets:
                self.exported_targets.update(interface.exported_targets)
                self.builtin_targets.update(interface.builtin_targets)
        pending_compilations: list[tuple[CompiledModule, Compilation]] = []
        while self.pending:
            descriptor = self.pending.popleft()
            filename = (
                descriptor.path.name
                if self.primitives is not None and descriptor.path is not None
                else str(descriptor.path)
            )
            compilation = compile_toplevel(
                descriptor.source(),
                filename,
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
            compilation.imported_targets = self.exported_targets
            compilation.storage_methods = self.storage_methods
            compilation.builtin_return_types = self.builtin_return_types
            compilation.builtin_targets = self.builtin_targets
            ir1_phases(compilation)
            if compilation.facts is None:
                raise RuntimeError(f"{descriptor.path} was not analyzed")
            self.record_types(descriptor.name, compilation)
        for descriptor, compilation in pending_compilations:
            self.lambdas.extend(compilation.lambdas)
            if compilation.facts is not None:
                self.facts.update(compilation.facts)

    def compile(self, source, filename):
        """Compile the main module and every module it imports; imported
        modules accumulate on the compiler. Returns the main module's
        ``Compilation``."""
        if self.primitives is None and (
            not self.storage_methods or not self.exported_targets
        ):
            from .driver import cached_builtins_interface

            interface = cached_builtins_interface()
            if not self.storage_methods:
                self.exported_types.update(interface.exported_types)
                self.storage_methods.update(interface.storage_methods)
                self.builtin_return_types.update(interface.builtin_return_types)
            if not self.exported_targets:
                self.exported_targets.update(interface.exported_targets)
                self.builtin_targets.update(interface.builtin_targets)
        compilation = compile_toplevel(source, filename, imports=self)
        self.compile_pending()
        compilation.imported_function_types = self.exported_types
        compilation.imported_targets = self.exported_targets
        compilation.storage_methods = self.storage_methods
        compilation.builtin_return_types = self.builtin_return_types
        compilation.builtin_targets = self.builtin_targets
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
