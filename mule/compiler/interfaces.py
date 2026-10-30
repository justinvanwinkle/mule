"""Module and builtins interface serialization, ELF/LLVM section extraction, and unit compilation."""

import json
import struct
from dataclasses import asdict, dataclass, field
from functools import cache
from pathlib import Path

from mule import ast

from . import (
    argument_protocol,
    format_protocol,
    native_protocol,
    print_protocol,
    sequence_protocol,
)
from .codegen import ForeignFunction, Program
from .ctype import UNKNOWN, ValueType, value_type_from_dict
from .dfo import find_dfo
from .escape import EscapeFacts, analyze_escapes
from .inputs import (
    file_digest,
    recording,
    replay_recorded,
)
from .ir1opt import constant_literal, ir1_optimize_until_done
from .ir1tran_lambda import lambda_blocks
from .ir1util import class_method_definitions, known_calls
from .main import (
    Compilation,
    compile_toplevel,
    ir1_phases,
)
from .modules import (
    STDLIB,
    CompiledModule,
    ModuleCompiler,
    is_library_path,
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


def _decode_interface_target(
    entry: dict, functions: dict[str, ForeignFunction]
) -> ForeignFunction | ClassConstructorTarget | None:
    kind = entry.get("kind")
    if kind == "function":
        fn_name = entry.get("name")
        if fn_name in functions:
            return functions[fn_name]
        return None
    if kind == "class":
        new_name = entry.get("new_target")
        init_name = entry.get("init_target")
        return ClassConstructorTarget(
            base_type=value_type_from_dict(entry["base_type"]),
            cls_type=value_type_from_dict(entry["cls_type"]),
            new_target=functions.get(new_name) if new_name else None,
            init_target=functions.get(init_name) if init_name else None,
            records_type_origin=entry.get("records_type_origin", False),
            layout_type=value_type_from_dict(entry["layout_type"]),
        )
    return None


@dataclass(slots=True)
class BuiltinsInterface:
    """What units compiled against the builtins unit need from it: its
    functions by name, the methods programs may predict, the protocol entry
    points, the modules it compiled, the names its protocols install as
    builtins, and the function or class target each builtin or export name is
    bound to."""

    functions: dict[str, ForeignFunction]
    methods: dict[str, list[str]]
    sequence_functions: dict[str, str]
    entries: dict[str, str | None]
    modules: dict[str, CompiledModule]
    builtins: list[str]
    exported_types: dict[tuple[str, str], ValueType]
    storage_methods: dict[tuple[str, str], ValueType]
    builtin_return_types: dict[str, ValueType]
    builtin_functions: dict[str, str]
    exported_targets: dict[tuple[str, str], ForeignCallTarget] = field(
        default_factory=dict
    )
    builtin_targets: dict[str, ForeignCallTarget] = field(default_factory=dict)

    def to_json(self) -> str:
        serialized_exports = {}
        for (mod, name), target in self.exported_targets.items():
            entry = target.to_interface_entry()
            if entry is not None:
                serialized_exports[f"{mod}:{name}"] = entry
        serialized_builtins = {}
        for name, target in self.builtin_targets.items():
            entry = target.to_interface_entry()
            if entry is not None:
                serialized_builtins[name] = entry
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
                        "entry_parameter_types": None
                        if function.entry_parameter_types is None
                        else [
                            fact.to_dict() for fact in function.entry_parameter_types
                        ],
                        "borrowed_parameters": sorted(function.borrowed_parameters),
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
                "exported_targets": serialized_exports,
                "storage_methods": {
                    f"{cls}.{meth}": vtype.to_dict()
                    for (cls, meth), vtype in self.storage_methods.items()
                },
                "builtin_return_types": {
                    name: vtype.to_dict()
                    for name, vtype in self.builtin_return_types.items()
                },
                "builtin_functions": self.builtin_functions,
                "builtin_targets": serialized_builtins,
                "modules": {
                    name: {
                        **asdict(module),
                        "path": None if module.path is None else module.path.name,
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
                entry_parameter_types=None
                if entry.get("entry_parameter_types") is None
                else tuple(
                    value_type_from_dict(fact)
                    for fact in entry["entry_parameter_types"]
                ),
                borrowed_parameters=frozenset(entry.get("borrowed_parameters", ())),
            )
            for name, entry in data["functions"].items()
        }
        modules = {
            name: CompiledModule(
                entry["name"],
                None
                if entry["path"] is None
                else (
                    (STDLIB / "bootstrap" / entry["path"])
                    if (STDLIB / "bootstrap" / entry["path"]).is_file()
                    else (STDLIB / entry["path"])
                ),
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
        from .constraint import BOOTSTRAP_BUILTIN_TARGETS
        from .protocols import NATIVE_MODULE_TARGETS

        exported_targets: dict[tuple[str, str], ForeignCallTarget] = dict(
            NATIVE_MODULE_TARGETS
        )
        for mod in modules.values():
            if mod.prefix:
                fn_prefix = f"{mod.prefix}mule_fn_"
                for fn_name, fn in functions.items():
                    if fn_name.startswith(fn_prefix):
                        pub_name = fn_name[len(fn_prefix) :]
                        if "." not in pub_name:
                            exported_targets[mod.name, pub_name] = fn
        for key, entry in data.get("exported_targets", {}).items():
            mod_name, pub_name = key.split(":", 1)
            decoded = _decode_interface_target(entry, functions)
            if decoded is not None:
                exported_targets[mod_name, pub_name] = decoded
        builtin_targets: dict[str, ForeignCallTarget] = dict(BOOTSTRAP_BUILTIN_TARGETS)
        for b_name, fn_name in data.get("builtin_functions", {}).items():
            if fn_name in functions:
                builtin_targets[b_name] = functions[fn_name]
        for b_name, entry in data.get("builtin_targets", {}).items():
            decoded = _decode_interface_target(entry, functions)
            if decoded is not None:
                builtin_targets[b_name] = decoded
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
            data["builtin_functions"],
            exported_targets,
            builtin_targets,
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
                        "entry_parameter_types": None
                        if function.entry_parameter_types is None
                        else [
                            fact.to_dict() for fact in function.entry_parameter_types
                        ],
                        "borrowed_parameters": sorted(function.borrowed_parameters),
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
                entry_parameter_types=None
                if entry.get("entry_parameter_types") is None
                else tuple(
                    value_type_from_dict(fact)
                    for fact in entry["entry_parameter_types"]
                ),
                borrowed_parameters=frozenset(entry.get("borrowed_parameters", ())),
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
            decoded = _decode_interface_target(entry, functions)
            if decoded is not None:
                exported_targets[mod_name, pub_name] = decoded
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


def _exported_borrowed_parameters(lambda_, escapes: EscapeFacts) -> frozenset[int]:
    if not lambda_.supports_escape_summary():
        return frozenset()
    return frozenset(
        index
        for index, var in enumerate(lambda_.vars)
        if not var.indirect
        and not var.sets
        and (lambda_.is_protocol or not escapes.parameter_escapes(lambda_, index))
    )


def module_interface(
    module_name: str,
    compilation,
    compiler: ModuleCompiler,
    dependencies: list[str],
    base_storage_methods: dict[tuple[str, str], ValueType] | None = None,
) -> ModuleInterface:
    facts = compilation.facts or {}
    escapes = analyze_escapes(compilation.lambdas, expand=False)
    functions: dict[str, ForeignFunction] = {
        lambda_.name: ForeignFunction(
            lambda_.name,
            lambda_.parameters,
            lambda_.signature,
            lambda_.takes_closure,
            facts[lambda_].return_type if lambda_ in facts else UNKNOWN,
            lambda_.namespace,
            entry_parameter_types=lambda_.entry_parameter_types,
            borrowed_parameters=_exported_borrowed_parameters(lambda_, escapes),
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
                        entry_parameter_types=lam.entry_parameter_types,
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
                            entry_parameter_types=lam.entry_parameter_types,
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


def _extract_ll_interface_json(path: Path) -> str | None:
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
        return bytes.fromhex(hex_str).decode("utf-8")
    except ValueError:
        return None


@cache
def _cached_ll_interface(path: Path, state: str) -> tuple[ModuleInterface, str] | None:
    raw = _extract_ll_interface_json(path)
    if raw is None:
        return None
    try:
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
    escapes = analyze_escapes(unit.lambdas, expand=False)
    functions = {
        lambda_.name: ForeignFunction(
            lambda_.name,
            lambda_.parameters,
            lambda_.signature,
            lambda_.takes_closure,
            unit.facts[lambda_].return_type if lambda_ in unit.facts else UNKNOWN,
            entry_parameter_types=lambda_.entry_parameter_types,
            borrowed_parameters=_exported_borrowed_parameters(lambda_, escapes),
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

    exported_targets: dict[tuple[str, str], ForeignCallTarget] = {}
    for (mod, pub_name), target in unit.exported_targets.items():
        fn = target.as_function()
        if fn is not None and fn.name in functions:
            exported_targets[mod, pub_name] = functions[fn.name]
        elif target.to_interface_entry() is not None:
            exported_targets[mod, pub_name] = target
    for mod in modules.values():
        if mod.prefix:
            fn_prefix = f"{mod.prefix}mule_fn_"
            for fn_name, fn in functions.items():
                if fn_name.startswith(fn_prefix):
                    pub_name = fn_name[len(fn_prefix) :]
                    if "." not in pub_name:
                        exported_targets.setdefault((mod.name, pub_name), fn)
    builtin_targets: dict[str, ForeignCallTarget] = {}
    for b_name, target in unit.builtin_targets.items():
        fn = target.as_function()
        if fn is not None and fn.name in functions:
            builtin_targets[b_name] = functions[fn.name]
        elif target.to_interface_entry() is not None:
            builtin_targets[b_name] = target

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
        {
            name: lambda_.name
            for name, target in unit.builtin_targets.items()
            if (lambda_ := target.as_lambda()) is not None
        },
        exported_targets,
        builtin_targets,
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


@dataclass(slots=True)
class _PreparedUnit:
    compilation: Compilation
    compiler: ModuleCompiler
    unit_name: str
    eager_modules: list[CompiledModule]
    linked_pending: list[CompiledModule]
    recorded: dict[str, str | None]


def _resolve_implicit_runtime_imports(
    compilation: Compilation, compiler: ModuleCompiler
) -> None:
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


def _prepare_unit(
    source: str | None,
    filename: str,
    interface: BuiltinsInterface,
    *,
    module: CompiledModule | None = None,
    roots: tuple[Path, ...] | None = None,
    package: str | None = None,
) -> _PreparedUnit:
    with recording() as recorded:
        if source is None:
            assert module is not None
            source = module.source()
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
        for component in compilation.components:
            ir1_optimize_until_done(component)
            if component.reanalyze:
                find_dfo(component)
        eager_modules = linked_imports(compilation, compiler, module)
        _resolve_implicit_runtime_imports(compilation, compiler)
        linked = linked_imports(compilation, compiler, module)
        pending_names = {dep.name for dep in compiler.pending}
        linked_pending = [dep for dep in linked if dep.name in pending_names]
    return _PreparedUnit(
        compilation=compilation,
        compiler=compiler,
        unit_name=unit_name,
        eager_modules=eager_modules,
        linked_pending=linked_pending,
        recorded=recorded,
    )


def _finish_unit(
    prepared: _PreparedUnit,
    filename: str,
    interface: BuiltinsInterface,
    *,
    module: CompiledModule | None = None,
    resolve_import_interface=None,
) -> tuple[Program, list[CompiledModule], ModuleInterface | None]:
    replay_recorded(prepared.recorded)
    compilation = prepared.compilation
    compiler = prepared.compiler
    unit_name = prepared.unit_name
    imported_interfaces: dict[str, ModuleInterface] = {}
    if resolve_import_interface is not None:
        eager_names = {m.name for m in prepared.eager_modules}
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

    _resolve_implicit_runtime_imports(compilation, compiler)
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
    mod_iface: ModuleInterface | None = None
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
    return program, linked, mod_iface


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
    prepared = _prepare_unit(
        source,
        filename,
        interface,
        module=module,
        roots=roots,
        package=package,
    )
    program, linked, _ = _finish_unit(
        prepared,
        filename,
        interface,
        module=module,
        resolve_import_interface=resolve_import_interface,
    )
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
    compiler.exported_targets.update(interface.exported_targets)
    compiler.storage_methods.update(interface.storage_methods)
    compiler.builtin_return_types.update(interface.builtin_return_types)
    compiler.builtin_targets.update(interface.builtin_targets)
    if module is not None:
        compiler.modules[module.name] = module
    return compiler


@cache
def _interface(path: Path, state: str) -> BuiltinsInterface:
    raw = _extract_ll_interface_json(path)
    if raw is None:
        raise RuntimeError(f"missing .mule.interface in {path}")
    return BuiltinsInterface.from_json(raw)
