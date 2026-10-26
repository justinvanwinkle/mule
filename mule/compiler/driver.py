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
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict, dataclass
from functools import cache
from pathlib import Path

from mule import ast
from mule.diagnostics import phase

from . import (
    argument_protocol,
    format_protocol,
    native_protocol,
    print_protocol,
    sequence_protocol,
)
from .artifacts import (
    Graph,
    Node,
    Step,
    ToolchainError,
    cache_directory,
    mule_hash,
    physical_core_count,
    tool_identity,
)
from .codegen import ForeignFunction, Program, generate
from .ctype import UNKNOWN, ValueType, value_type_from_dict
from .dfo import find_dfo
from .inputs import file_digest, read_source
from .ir1opt import constant_literal, ir1_optimize_until_done
from .ir1tran_lambda import lambda_blocks
from .ir1util import class_method_definitions, known_calls
from .main import compile_toplevel, ir1_phases
from .modules import (
    STDLIB,
    CompiledModule,
    ModuleCompiler,
    is_library_path,
    search_path,
)
from .node import (
    CallKind,
    ClassConstructorTarget,
    Combination,
    ForeignCallTarget,
    FunctionalKind,
    LeafVisitor,
    NodeVisitor,
    Signature,
    linked,
)

__all__ = [
    "ToolchainError",
    "build",
    "build_runtime",
    "builtins_object",
    "compile_program",
    "emit_llvm",
    "link_llvm",
    "read_elf_interface",
]

LINK_LIBRARIES = ("-lgmp", "-lutil", "-lrt", "-lpthread", "-lm", "-ldl")
BUILTINS_SHARDS = 16


@dataclass(slots=True)
class BuiltinsInterface:
    """What units compiled against the builtins unit need from it: its
    functions by name, the methods programs may predict, the protocol entry
    points, the modules it compiled, and the names its protocols install as
    builtins."""

    functions: dict[str, ForeignFunction]
    methods: dict[str, list[str]]
    sequence_functions: dict[str, str]
    entries: dict[str, str | None]
    modules: dict[str, CompiledModule]
    builtins: list[str]
    exported_types: dict[tuple[str, str], ValueType]
    storage_methods: dict[tuple[str, str], ValueType]
    builtin_return_types: dict[str, ValueType]

    def to_json(self) -> str:
        return json.dumps(
            {
                "functions": {
                    name: {
                        "parameters": list(function.parameters),
                        "kinds": [kind.name for kind in function.signature.kinds],
                        "defaults": list(function.signature.default_parameters),
                        "variadic": function.signature.variadic,
                        "closure": function.takes_closure,
                        "return_type": function.return_type.to_dict(),
                    }
                    for name, function in self.functions.items()
                },
                "methods": self.methods,
                "sequence_functions": self.sequence_functions,
                "entries": self.entries,
                "builtins": self.builtins,
                "exported_types": {
                    f"{mod}:{fn}": vtype.to_dict()
                    for (mod, fn), vtype in self.exported_types.items()
                },
                "storage_methods": {
                    f"{cls}.{meth}": vtype.to_dict()
                    for (cls, meth), vtype in self.storage_methods.items()
                },
                "builtin_return_types": {
                    name: vtype.to_dict()
                    for name, vtype in self.builtin_return_types.items()
                },
                "modules": {
                    name: {
                        **asdict(module),
                        "path": None if module.path is None else str(module.path),
                    }
                    for name, module in self.modules.items()
                },
            },
            sort_keys=True,
            indent=0,
        )

    @classmethod
    def from_json(cls, text: str) -> BuiltinsInterface:
        data = json.loads(text)
        functions = {
            name: ForeignFunction(
                name,
                tuple(entry["parameters"]),
                Signature(
                    kinds=tuple(ast.ParameterKind[kind] for kind in entry["kinds"]),
                    default_parameters=tuple(entry["defaults"]),
                    variadic=entry["variadic"],
                ),
                entry["closure"],
                value_type_from_dict(entry["return_type"])
                if "return_type" in entry
                else UNKNOWN,
            )
            for name, entry in data["functions"].items()
        }
        modules = {
            name: CompiledModule(
                entry["name"],
                None if entry["path"] is None else Path(entry["path"]),
                entry["entry"],
                native=entry["native"],
                missing=entry["missing"],
                prefix=entry["prefix"],
                namespace_paths=tuple(entry["namespace_paths"]),
            )
            for name, entry in data["modules"].items()
        }
        exported_types = {
            tuple(key.split(":", 1)): value_type_from_dict(val)
            for key, val in data.get("exported_types", {}).items()
        }
        storage_methods = {
            tuple(key.split(".", 1)): value_type_from_dict(val)
            for key, val in data.get("storage_methods", {}).items()
        }
        builtin_return_types = {
            key: value_type_from_dict(val)
            for key, val in data.get("builtin_return_types", {}).items()
        }
        return cls(
            functions,
            data["methods"],
            data["sequence_functions"],
            data["entries"],
            modules,
            data["builtins"],
            exported_types,
            storage_methods,
            builtin_return_types,
        )


@dataclass(slots=True)
class ModuleInterface:
    """Compile-time metadata embedded in a compiled module's ``.mule.interface``
    ELF section: its functions, method predictions, exported return/value types,
    exported call targets, class storage methods, and direct module dependencies."""

    functions: dict[str, ForeignFunction]
    methods: dict[str, list[str]]
    exported_types: dict[tuple[str, str], ValueType]
    exported_targets: dict[tuple[str, str], ForeignCallTarget]
    storage_methods: dict[tuple[str, str], ValueType]
    dependencies: list[str]

    def to_json(self) -> str:
        serialized_targets = {}
        for (mod, name), target in self.exported_targets.items():
            entry = target.to_interface_entry()
            if entry is not None:
                serialized_targets[f"{mod}:{name}"] = entry
        return json.dumps(
            {
                "functions": {
                    name: {
                        "parameters": list(function.parameters),
                        "kinds": [kind.name for kind in function.signature.kinds],
                        "defaults": list(function.signature.default_parameters),
                        "variadic": function.signature.variadic,
                        "closure": function.takes_closure,
                        "return_type": function.return_type.to_dict(),
                        "namespace": function.namespace,
                    }
                    for name, function in self.functions.items()
                },
                "methods": self.methods,
                "exported_types": {
                    f"{mod}:{fn}": vtype.to_dict()
                    for (mod, fn), vtype in self.exported_types.items()
                },
                "exported_targets": serialized_targets,
                "storage_methods": {
                    f"{cls}.{meth}": vtype.to_dict()
                    for (cls, meth), vtype in self.storage_methods.items()
                },
                "dependencies": self.dependencies,
            },
            sort_keys=True,
            indent=0,
        )

    @classmethod
    def from_json(cls, text: str) -> ModuleInterface:
        data = json.loads(text)
        functions = {
            name: ForeignFunction(
                name,
                tuple(entry["parameters"]),
                Signature(
                    kinds=tuple(ast.ParameterKind[kind] for kind in entry["kinds"]),
                    default_parameters=tuple(entry["defaults"]),
                    variadic=entry["variadic"],
                ),
                entry["closure"],
                value_type_from_dict(entry["return_type"])
                if "return_type" in entry
                else UNKNOWN,
                entry.get("namespace", ""),
            )
            for name, entry in data.get("functions", {}).items()
        }
        exported_types = {
            tuple(key.split(":", 1)): value_type_from_dict(val)
            for key, val in data.get("exported_types", {}).items()
        }
        exported_targets: dict[tuple[str, str], ForeignCallTarget] = {}
        for key, entry in data.get("exported_targets", {}).items():
            mod_name, pub_name = key.split(":", 1)
            kind = entry.get("kind")
            if kind == "function":
                fn_name = entry.get("name")
                if fn_name in functions:
                    exported_targets[mod_name, pub_name] = functions[fn_name]
            elif kind == "class":
                new_name = entry.get("new_target")
                init_name = entry.get("init_target")
                exported_targets[mod_name, pub_name] = ClassConstructorTarget(
                    base_type=value_type_from_dict(entry["base_type"]),
                    cls_type=value_type_from_dict(entry["cls_type"]),
                    new_target=functions.get(new_name) if new_name else None,
                    init_target=functions.get(init_name) if init_name else None,
                    records_type_origin=entry.get("records_type_origin", False),
                )
        storage_methods = {
            tuple(key.split(".", 1)): value_type_from_dict(val)
            for key, val in data.get("storage_methods", {}).items()
        }
        return cls(
            functions,
            data.get("methods", {}),
            exported_types,
            exported_targets,
            storage_methods,
            list(data.get("dependencies", ())),
        )


def module_interface(
    module_name: str,
    compilation,
    compiler: ModuleCompiler,
    dependencies: list[str],
    base_storage_methods: dict[tuple[str, str], ValueType] | None = None,
) -> ModuleInterface:
    facts = compilation.facts or {}
    functions: dict[str, ForeignFunction] = {
        lambda_.name: ForeignFunction(
            lambda_.name,
            lambda_.parameters,
            lambda_.signature,
            lambda_.takes_closure,
            facts[lambda_].return_type if lambda_ in facts else UNKNOWN,
            lambda_.namespace,
        )
        for lambda_ in compilation.lambdas
        if lambda_.kind is not FunctionalKind.TOPLEVEL
    }
    methods: dict[str, list[str]] = {}
    for lambda_ in compilation.lambdas:
        for definitions in class_method_definitions(lambda_).values():
            for method_name, target in definitions.items():
                targets = methods.setdefault(method_name, [])
                if target.name not in targets:
                    targets.append(target.name)
    exported_types = {
        (mod, name): vtype
        for (mod, name), vtype in compiler.exported_types.items()
        if mod == module_name
    }
    exported_targets: dict[tuple[str, str], ForeignCallTarget] = {}
    for (mod, name), target in compiler.exported_targets.items():
        if mod != module_name:
            continue
        fn = target.as_function()
        if fn is not None:
            if fn.name not in functions:
                lam = target.as_lambda()
                if lam is not None:
                    functions[fn.name] = ForeignFunction(
                        lam.name,
                        lam.parameters,
                        lam.signature,
                        lam.takes_closure,
                        facts[lam].return_type if lam in facts else UNKNOWN,
                        lam.namespace,
                    )
                else:
                    functions[fn.name] = fn
            exported_targets[mod, name] = functions[fn.name]
            targets = methods.setdefault(name, [])
            if fn.name not in targets:
                targets.append(fn.name)
        else:
            if target.to_interface_entry() is None:
                continue
            for member in target.interface_members():
                if member.name not in functions:
                    lam = member.as_lambda()
                    if lam is not None:
                        functions[lam.name] = ForeignFunction(
                            lam.name,
                            lam.parameters,
                            lam.signature,
                            lam.takes_closure,
                            facts[lam].return_type if lam in facts else UNKNOWN,
                            lam.namespace,
                        )
                    else:
                        functions[member.name] = member
            exported_targets[mod, name] = target
    storage_methods = (
        dict(compiler.storage_methods)
        if base_storage_methods is None
        else {
            key: val
            for key, val in compiler.storage_methods.items()
            if key not in base_storage_methods
        }
    )
    return ModuleInterface(
        functions,
        methods,
        exported_types,
        exported_targets,
        storage_methods,
        dependencies,
    )


@cache
def _cached_elf_interface(path: Path, state: str) -> tuple[ModuleInterface, str] | None:
    try:
        data = path.read_bytes()
    except OSError:
        return None
    if len(data) < 64 or data[:4] != b"\x7fELF" or data[4] != 2 or data[5] != 1:
        return None
    e_shoff, _, _, _, _, e_shentsize, e_shnum, e_shstrndx = struct.unpack_from(
        "<QIHHHHHH", data, 40
    )
    if (
        e_shoff == 0
        or e_shnum == 0
        or e_shentsize < 64
        or e_shstrndx >= e_shnum
        or e_shoff + e_shnum * e_shentsize > len(data)
    ):
        return None
    shstr_hdr = e_shoff + e_shstrndx * e_shentsize
    shstr_offset, shstr_size = struct.unpack_from("<QQ", data, shstr_hdr + 24)
    if shstr_offset + shstr_size > len(data):
        return None
    for idx in range(e_shnum):
        sh_hdr = e_shoff + idx * e_shentsize
        (sh_name,) = struct.unpack_from("<I", data, sh_hdr)
        name_start = shstr_offset + sh_name
        if name_start >= shstr_offset + shstr_size:
            continue
        name_end = data.find(b"\x00", name_start, shstr_offset + shstr_size)
        if name_end < 0:
            continue
        if data[name_start:name_end] == b".mule.interface":
            sh_offset, sh_size = struct.unpack_from("<QQ", data, sh_hdr + 24)
            if sh_offset + sh_size > len(data):
                return None
            raw = data[sh_offset : sh_offset + sh_size].rstrip(b"\x00")
            try:
                text = raw.decode("utf-8")
                return ModuleInterface.from_json(text), text
            except ValueError, KeyError, TypeError:
                return None
    return None


def read_elf_interface(path: Path) -> tuple[ModuleInterface, str] | None:
    """Read and parse the ``.mule.interface`` section from an ELF64 ``.so`` or ``.o``."""
    if not path.is_file():
        return None
    return _cached_elf_interface(path, file_digest(path))


@cache
def _cached_ll_interface(path: Path, state: str) -> tuple[ModuleInterface, str] | None:
    marker = ', section ".mule.interface", align 1'
    try:
        text = path.read_text()
    except OSError:
        return None
    pos = text.find(marker)
    if pos < 0:
        return None
    line_start = text.rfind("\n", 0, pos) + 1
    line = text[line_start:pos]
    c_start = line.find(' c"')
    if c_start < 0 or not line.endswith('\\00"'):
        return None
    hex_str = line[c_start + 3 : -4].replace("\\", "")
    try:
        raw = bytes.fromhex(hex_str).decode("utf-8")
        return ModuleInterface.from_json(raw), raw
    except ValueError, KeyError, TypeError:
        return None


def read_ll_interface(path: Path) -> tuple[ModuleInterface, str] | None:
    """Read and parse the ``.mule.interface`` constant from an LLVM IR ``.ll`` file."""
    if not path.is_file():
        return None
    return _cached_ll_interface(path, file_digest(path))


def compile_builtins() -> tuple[Program, dict[str, CompiledModule]]:
    """Compile the stdlib protocols every program runs on, through the IR1
    phases, as the exporting ``builtins`` unit."""
    unit = Program([], None, {}, unit="builtins", exports=True, bootstrap=True)
    unit.print_entry = print_protocol.prepare(unit)
    unit.protocol_entry, unit.sequence_functions, modules = sequence_protocol.prepare(
        unit
    )
    unit.argument_constructor, unit.argument_binder = argument_protocol.prepare(unit)
    unit.layouts = native_protocol.prepare(unit)
    unit.format_entry = format_protocol.prepare(unit)
    return unit, modules


_BUILTIN_METHOD_PRIORITY = (
    "fn_ListStorage.",
    "fn_TextStorage.",
    "fn_dict.",
    "fn_set.",
    "fn__SetBase.",
    "fn_TupleStorage.",
    "fn_BytesStorage.",
    "fn_bytearray.",
)


def _builtin_method_rank(target_name: str) -> int:
    for index, marker in enumerate(_BUILTIN_METHOD_PRIORITY):
        if marker in target_name:
            return index
    return len(_BUILTIN_METHOD_PRIORITY)


def builtins_interface(unit: Program, modules) -> BuiltinsInterface:
    functions = {
        lambda_.name: ForeignFunction(
            lambda_.name,
            lambda_.parameters,
            lambda_.signature,
            lambda_.takes_closure,
            unit.facts[lambda_].return_type if lambda_ in unit.facts else UNKNOWN,
        )
        for lambda_ in unit.lambdas
    }
    methods: dict[str, list[str]] = {}
    for lambda_ in unit.lambdas:
        for definitions in class_method_definitions(lambda_).values():
            for method_name, target in definitions.items():
                targets = methods.setdefault(method_name, [])
                if target.name not in targets:
                    targets.append(target.name)
    for targets in methods.values():
        targets.sort(key=_builtin_method_rank)

    def name(lambda_):
        return None if lambda_ is None else lambda_.name

    return BuiltinsInterface(
        functions,
        methods,
        {key: lambda_.name for key, lambda_ in unit.sequence_functions.items()},
        {
            "print": name(unit.print_entry),
            "format": name(unit.format_entry),
            "protocol": name(unit.protocol_entry),
            "argument_constructor": name(unit.argument_constructor),
            "argument_binder": name(unit.argument_binder),
        },
        dict(modules),
        installed_builtins(unit),
        dict(unit.exported_types),
        dict(unit.storage_methods),
        dict(unit.builtin_return_types),
    )


# Builtins the runtime defines itself (``builtins::namespace`` in
# ``mule/runtime/builtins.rs``). Linking verifies every name a unit assumed is
# a builtin, so a drift from that list fails at startup.
RUNTIME_BUILTINS = ("abs", "bool", "float", "input", "int", "len", "list", "tuple")


def installed_builtins(unit: Program) -> list[str]:
    """The builtins every later unit can rely on being defined: the runtime's
    own and those the builtins unit's module bodies install by literal name."""
    names = set(RUNTIME_BUILTINS)
    for lambda_ in unit.lambdas:
        if lambda_.kind is not FunctionalKind.TOPLEVEL:
            continue
        for call in known_calls(lambda_, "_install_builtin"):
            literal = constant_literal(call.args[0])
            name = None if literal is None else literal.accept(_TextValue())
            if name is not None:
                names.add(name)
    return sorted(names)


class _TextValue(ast.Visitor):
    """A text literal's value, or None for any other literal."""

    def visit_Node(self, node):
        return None

    def visit_TextLiteral(self, node):
        return node.value


class _ContainsBuiltinText(ast.Visitor):
    """Whether a constant literal is or contains one of the target builtin names."""

    def __init__(self, names: tuple[str, ...]):
        self._names = names

    def visit_Node(self, node):
        return False

    def visit_TextLiteral(self, node):
        return node.value in self._names

    def visit_Tuple(self, node):
        return any(element.accept(self) for element in node.elements)


class _ReferencesGlobal(NodeVisitor, LeafVisitor):
    """Whether a node or leaf references one of the named global bindings."""

    def __init__(self, *names: str):
        self._names = names

    def visit_Node(self, node):
        return False

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return False

    def visit_GlobalVar(self, leaf):
        return leaf.name in self._names


class _ReferencesBuiltin(_ReferencesGlobal):
    """Whether a node or leaf can reach one of the named builtins."""

    def __init__(self, *names: str):
        super().__init__(*names)
        self._text = _ContainsBuiltinText(names)

    def visit_Combination(self, node):
        return (
            node.kind is CallKind.KNOWN
            and linked(node.fun_info, node, "known function").name == "%getattr"
            and linked(node.info, node, "attribute").name in self._names
        )

    def visit_Constant(self, leaf):
        return leaf.value.accept(self._text)


def compile_unit(
    source: str,
    filename: str,
    interface: BuiltinsInterface,
    *,
    module: CompiledModule | None = None,
    roots: tuple[Path, ...] | None = None,
    package: str | None = None,
    resolve_import_interface=None,
) -> tuple[Program, list[CompiledModule]]:
    """Compile one module as its own unit against the builtins interface:
    the main module when ``module`` is None. Returns the unit and how each
    module it imports resolved, in request order."""
    compiler = import_resolver(filename, interface, module, roots, package)
    if module is None:
        compiler.include_patterns(compiler.project_modules)
        compilation = compile_toplevel(source, filename, imports=compiler)
        unit_name = "__main__"
    else:
        compilation = compile_toplevel(
            source,
            filename,
            imports=compiler,
            prefix=module.prefix,
            namespace=module.name,
        )
        unit_name = module.name

    imported_interfaces: dict[str, ModuleInterface] = {}
    if resolve_import_interface is not None:
        for component in compilation.components:
            ir1_optimize_until_done(component)
            if component.reanalyze:
                find_dfo(component)
        eager_names = {m.name for m in linked_imports(compilation, compiler, module)}
        for dep in list(compiler.pending):
            if dep.missing or dep.native or dep.path is None:
                continue
            if dep.name not in eager_names:
                continue
            dep_iface = resolve_import_interface(dep)
            if dep_iface is not None:
                imported_interfaces[dep.name] = dep_iface
                compiler.exported_types.update(dep_iface.exported_types)
                compiler.exported_targets.update(dep_iface.exported_targets)
                compiler.storage_methods.update(dep_iface.storage_methods)

    compilation.imported_function_types = compiler.exported_types
    compilation.imported_targets = compiler.exported_targets
    compilation.storage_methods = compiler.storage_methods
    compilation.builtin_return_types = compiler.builtin_return_types
    compilation.builtin_targets = compiler.builtin_targets
    ir1_phases(compilation)
    if module is not None:
        compiler.record_types(module.name, compilation)

    nodes = [
        node
        for lambda_ in compilation.lambdas
        if lambda_.component is not None
        for block in lambda_.component.blocks()
        for node in block.nodes()
    ]
    open_check = _ReferencesBuiltin("open")
    if any(node.accept(open_check) for node in nodes):
        compiler.resolve("io", None, None)
    builtins_check = _ReferencesGlobal("builtins")
    compile_global = _ReferencesGlobal("compile", "eval", "exec")
    compile_attr = _ReferencesBuiltin("compile", "eval", "exec")
    if any(node.accept(compile_global) for node in nodes) or (
        any(node.accept(builtins_check) for node in nodes)
        and any(node.accept(compile_attr) for node in nodes)
    ):
        compiler.resolve("ast", None, None)
    linked = linked_imports(compilation, compiler, module)
    linked_names = {m.name for m in linked}

    functions = dict(interface.functions)
    methods = {method: list(names) for method, names in interface.methods.items()}
    for dep_name, dep_iface in imported_interfaces.items():
        if dep_name not in linked_names:
            continue
        functions.update(dep_iface.functions)
        for method, names in dep_iface.methods.items():
            target_list = methods.setdefault(method, [])
            for name in names:
                if name not in target_list:
                    target_list.append(name)

    def entry(role):
        name = interface.entries[role]
        return None if name is None else functions[name]

    if compilation.facts is None:
        raise RuntimeError(f"{filename} was not analyzed")
    program = Program(
        compilation.lambdas,
        compilation.entry if module is None else None,
        compilation.facts,
        print_entry=entry("print"),
        format_entry=entry("format"),
        protocol_entry=entry("protocol"),
        sequence_functions={
            key: functions[name] for key, name in interface.sequence_functions.items()
        },
        argument_constructor=entry("argument_constructor"),
        argument_binder=entry("argument_binder"),
        builtin_names=frozenset(interface.builtins),
        external=list(functions.values()),
        external_methods={
            method: tuple(functions[name] for name in names if name in functions)
            for method, names in methods.items()
        },
        unit=unit_name,
        exports=module is not None,
        dependencies=["builtins"],
        search_paths=tuple(str(root) for root in compiler.roots),
    )
    if module is not None:
        program.module_import = module.import_info()
    argument_protocol.bind_signatures(program.lambdas)
    program.layouts = native_protocol.prepare(program)
    if module is not None:
        used_deps = [m.name for m in linked if m.name in imported_interfaces]
        mod_iface = module_interface(
            module.name,
            compilation,
            compiler,
            used_deps,
            interface.storage_methods,
        )
        program.interface_bytes = mod_iface.to_json().encode("utf-8")
    return program, linked


class _ImportedModule(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Combination(self, node: Combination):
        if (
            node.fun_info is not None
            and node.fun_info.name == "%import"
            and node.info is not None
        ):
            return node.info.module_name
        return None


_IMPORTED_MODULE = _ImportedModule()


def linked_imports(
    compilation, compiler: ModuleCompiler, module: CompiledModule | None = None
) -> list[CompiledModule]:
    """The modules a unit links: those whose import statements survive IR1
    optimization, which deletes imports in branches proven dead, such as an
    imported module's ``if __name__ == "__main__":``, plus the names scans
    and builtin requirements resolved without a statement. For stdlib modules,
    only module-toplevel imports are linked ahead of time; function-local
    stdlib imports are compiled on demand if reached at runtime."""
    if module is None:
        return [compiler.modules[name] for name in compiler.requested]
    is_library = module.path is not None and is_library_path(module.path)
    imported = set(compiler.scanned)
    for lambda_ in compilation.lambdas:
        if is_library and lambda_.kind is not FunctionalKind.TOPLEVEL:
            continue
        for block in lambda_blocks(lambda_):
            for node in block.nodes():
                module_name = node.accept(_IMPORTED_MODULE)
                if module_name is not None:
                    imported.add(module_name)
    return [compiler.modules[name] for name in compiler.requested if name in imported]


def import_resolver(
    filename: str,
    interface: BuiltinsInterface,
    module: CompiledModule | None,
    roots: tuple[Path, ...] | None,
    package: str | None,
) -> ModuleCompiler:
    """Resolves a unit's imports as its compilation does: the builtins unit's
    modules are already compiled, and a module unit knows its own package."""
    compiler = ModuleCompiler(filename, roots=roots, package=package)
    compiler.configure_project()
    compiler.modules.update(interface.modules)
    compiler.exported_types.update(interface.exported_types)
    compiler.storage_methods.update(interface.storage_methods)
    compiler.builtin_return_types.update(interface.builtin_return_types)
    if module is not None:
        compiler.modules[module.name] = module
    return compiler


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


@dataclass(frozen=True, slots=True)
class Toolchain:
    clang: str = "clang"
    rustc: str = "rustc"
    optimize: int = 1
    lto: bool = False

    def __post_init__(self):
        if self.optimize not in (0, 1, 2, 3):
            raise ValueError("optimization level must be 0, 1, 2, or 3")
        object.__setattr__(self, "optimize", int(self.optimize))

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

    def clang_settings(self) -> dict:
        return {
            "clang": tool_identity(
                (self.clang, "--version"),
                tool="LLVM Clang compiler",
                failure="LLVM compilation/linking failed",
            ),
            "optimize": self.optimize,
            "lto": self.lto,
        }

    def compile_flags(self) -> list[str]:
        flags = [f"-O{self.optimize}"]
        if self.lto:
            flags.append("-flto=full")
        else:
            flags.append("-fPIC")
        return flags

    def link_flags(self) -> list[str]:
        flags = [f"-O{self.optimize}"]
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
        resolved = source_path.resolve()
        st = resolved.stat()
    except OSError:
        return None
    payload = (
        f"{compiler_hash}\0{optimize}\0{module_name}\0{resolved}\0"
        f"{st.st_dev}:{st.st_ino}:{st.st_size}:{st.st_mtime_ns}:{st.st_ctime_ns}"
    ).encode()
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

    def resolve_import_interface(
        self, caller_name: str, dep: CompiledModule, roots, package
    ) -> ModuleInterface | None:
        if dep.missing or dep.native or dep.path is None:
            return None
        with self.graph.lock:
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
                        return res[0]
                fasl_step = self.fasl(self.module(dep, roots, package), dep)
                self.graph.resolve_now(fasl_step)
                res = read_elf_interface(fasl_step.path("module.so"))
                return None if res is None else res[0]
            mod_step = self.module(dep, roots, package)
            if self.toolchain.lto:
                self.graph.resolve_now(mod_step)
                res = read_ll_interface(mod_step.path("unit.ll"))
                return None if res is None else res[0]
            obj_step = self.object(mod_step)
            self.graph.resolve_now(obj_step)
            res = read_elf_interface(obj_step.path("unit.o"))
            return None if res is None else res[0]
        finally:
            with self.graph.lock:
                if caller_name in self._compiling:
                    self._compiling[caller_name].discard(dep.name)

    def runtime(self) -> Step:
        rustc, lto = self.toolchain.rustc, self.toolchain.lto
        sources = sorted((Path(__file__).parents[1] / "runtime").glob("*.rs"))

        def action(inputs, directory):
            built = build_runtime(directory, rustc=rustc, lto=lto)
            if built != directory / "libmule_runtime.a":
                os.replace(built, directory / "libmule_runtime.a")

        return self.graph.step(
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

    def builtins(self) -> Step:
        def action(inputs, directory):
            unit, modules = compile_builtins()
            interface = builtins_interface(unit, modules)
            (directory / "unit.ll").write_text(generate(unit))
            (directory / "interface.json").write_text(interface.to_json())

        return self.graph.step(
            "builtins",
            lambda: Step(
                self.graph,
                "builtins",
                inputs=[],
                settings={"step": "builtins", "stdlib": str(STDLIB.resolve())},
                outputs=("unit.ll", "interface.json"),
                action=action,
            ),
        )

    def interface(self) -> BuiltinsInterface:
        builtins = self.builtins()
        if builtins.output_digests is None:
            raise RuntimeError("builtins has not produced its outputs")
        return _interface(
            builtins.path("interface.json"),
            builtins.output_digests["interface.json"],
        )

    def object(self, unit: Step) -> Step:
        toolchain = self.toolchain
        lto = toolchain.lto
        sharded = unit.name == "builtins" and not lto
        name = f"{unit.name} object optimize={toolchain.optimize} lto={lto}"

        def action(inputs, directory):
            with phase("compile object %s", unit.name):
                if sharded:
                    llvm_split = toolchain.llvm_tool("llvm-split")
                    shard_prefix = directory / "shard"
                    _run(
                        [
                            llvm_split,
                            f"-j={BUILTINS_SHARDS}",
                            f"-o={shard_prefix}",
                            str(unit.path("unit.ll")),
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
                    for obj in shard_objects:
                        Path(obj).unlink()
                    return
                _run(
                    [
                        toolchain.clang,
                        *toolchain.compile_flags(),
                        "-Wno-override-module",
                        "-c",
                        str(unit.path("unit.ll")),
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
        if sharded:
            settings["shards"] = BUILTINS_SHARDS
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
        name = f"module {descriptor.name} {path}"
        validated: dict[Path, ModuleCompiler] = {}

        def resolver():
            return import_resolver(
                str(path), self.interface(), descriptor, roots, package
            )

        def validate(entry: Path) -> bool:
            compiler = resolver()
            if resolves_as_recorded(compiler, entry / "imports.json"):
                validated[entry] = compiler
                return True
            return False

        def action(inputs, directory):
            with self.graph.lock:
                self._active_modules.add(descriptor.name)
            try:
                program, imported = compile_unit(
                    descriptor.source(),
                    str(path),
                    self.interface(),
                    module=descriptor,
                    roots=roots,
                    package=package,
                    resolve_import_interface=lambda dep: self.resolve_import_interface(
                        descriptor.name, dep, roots, package
                    ),
                )
            finally:
                with self.graph.lock:
                    self._active_modules.discard(descriptor.name)
            (directory / "unit.ll").write_text(generate(program))
            write_imports(directory / "imports.json", imported)

        # The unit depends on its search path only through how its imports
        # resolve, so the key omits the path and a cached unit is reused
        # wherever each import resolves the same way.
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
                    "namespace_paths": descriptor.namespace_paths,
                },
                outputs=("unit.ll", "imports.json"),
                action=action,
                discover=lambda step: self.discover(
                    step,
                    validated.pop(step.directory, None) or resolver(),
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
        if not resolver.requested:
            resolve_recorded(resolver, step.path("imports.json"))
        if self.fasl_mode:
            current_hash = mule_hash()
            for descriptor in resolver.pending:
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
        for descriptor in resolver.pending:
            self.link.add_input(self.object(self.module(descriptor, roots, package)))

    def main(
        self, source_node: Node, source: str, filename: str, roots, package
    ) -> Step:
        builtins = self.builtins()
        validated: dict[Path, ModuleCompiler] = {}

        def resolver():
            return import_resolver(filename, self.interface(), None, roots, package)

        def validate(entry: Path) -> bool:
            compiler = resolver()
            if resolves_as_recorded(compiler, entry / "imports.json"):
                validated[entry] = compiler
                return True
            return False

        def action(inputs, directory):
            program, imported = compile_unit(
                source,
                filename,
                self.interface(),
                roots=roots,
                package=package,
                resolve_import_interface=lambda dep: self.resolve_import_interface(
                    "__main__", dep, roots, package
                ),
            )
            if not self.toolchain.lto:
                program.toolchain_info = (
                    sys.executable,
                    self.toolchain.clang,
                    f"{self.toolchain.rustc}\n{mule_hash()}",
                    self.toolchain.optimize,
                )
            (directory / "unit.ll").write_text(generate(program))
            write_imports(directory / "imports.json", imported)

        return self.graph.step(
            "__main__",
            lambda: Step(
                self.graph,
                "__main__",
                inputs=[source_node, builtins],
                settings={
                    "step": "main",
                    "filename": filename,
                    "roots": None if roots is None else [str(root) for root in roots],
                    "package": package,
                },
                outputs=("unit.ll", "imports.json"),
                action=action,
                discover=lambda step: self.discover(
                    step,
                    validated.pop(step.directory, None) or resolver(),
                    roots,
                    package,
                ),
                validate=validate,
            ),
        )

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
        self.link.add_input(self.object(self.module(system, (STDLIB,), None)))
        return self.link

    def fasl(self, unit: Step, descriptor: CompiledModule) -> Step:
        """Link one module's object into its standalone FASL shared object."""
        toolchain = self.toolchain
        obj = self.object(unit)
        name = f"{unit.name} fasl optimize={toolchain.optimize}"

        def action(inputs, directory):
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

        return self.graph.step(
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


@cache
def _interface(path: Path, state: str) -> BuiltinsInterface:
    return BuiltinsInterface.from_json(path.read_text())


def load_interface(path: Path) -> BuiltinsInterface:
    """The builtins interface in ``path``, read once per content."""
    return _interface(path, file_digest(path))


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
                    module.namespace_paths,
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
) -> list[tuple[CompiledModule, bool, bool, tuple[str, ...], str | None]]:
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
        and module.namespace_paths == locations
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
    with Graph(1) as graph:
        build = Build(graph, Toolchain())
        graph.run(build.builtins())
        return build.interface()


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
) -> Path:
    """Compile a main unit's LLVM, link it with the builtins unit and the
    Rust runtime, and replace ``output`` only on success.

    ``optimize`` selects the generated code's level (0 through 3, default 1). ``lto``
    independently requests full LTO across the program and Rust runtime."""
    toolchain = Toolchain(clang, rustc, optimize, lto)
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
        target = build.fasl(build.module(descriptor, compiler.roots, None), descriptor)
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
            build.fasl(build.module(desc, compiler.roots, None), desc)
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
        toolchain=Toolchain(clang, rustc, optimize, lto),
        source_path=source_path.resolve(),
        workers=workers,
    )
    if llvm_output is not None:
        llvm_path.parent.mkdir(parents=True, exist_ok=True)
        llvm_path.write_text(emit_llvm(source_text, str(source_path), roots=roots))
    return _install(executable, output_path)
