"""Compile sequence/exception language rules and bind physical storage operations."""

from .ctype import UNKNOWN, ValueType
from .fndb import PROTOCOLS
from .inputs import _recording, read_source
from .ir1util import known_calls, made_lambda
from .main import compile_toplevel, ir1_phases
from .modules import STDLIB, ModuleCompiler
from .node import (
    ClassConstructorTarget,
    ForeignCallTarget,
    Lambda,
    linked,
    resolve_implementation_type,
)
from .protocols import PRIMITIVES, add_lambdas

_CACHED_STDLIB_TYPES: (
    tuple[
        dict[tuple[str, str], ValueType],
        dict[tuple[str, str], ValueType],
        dict[str, ValueType],
    ]
    | None
) = None

_CACHED_STDLIB_TARGETS: (
    tuple[
        dict[tuple[str, str], Lambda | ClassConstructorTarget | ForeignCallTarget],
        dict[str, Lambda | ClassConstructorTarget | ForeignCallTarget],
    ]
    | None
) = None


def exports(entry) -> dict:
    """Functions the module defines, by public name."""
    return {
        linked(call.info, call, "function definition").public_name: made_lambda(call)
        for call in known_calls(entry, "%make_function")
    }


def protocol_argument_types(name: str, builtin_targets) -> tuple[ValueType, ...] | None:
    """Resolve a declared internal entry domain through its implementation owner.

    Missing or ambiguous layout metadata cannot establish a physical receiver
    proof. Unrestricted arguments need no implementation identity.
    """
    info = PROTOCOLS.get(name)
    if info is None or info.argument_types is None:
        return None
    arguments = []
    for domain in info.argument_types:
        if domain is UNKNOWN:
            arguments.append(UNKNOWN)
            continue
        implementation = resolve_implementation_type(domain, builtin_targets.values())
        if implementation is UNKNOWN:
            return None
        arguments.append(implementation)
    return tuple(arguments)


def _populate_in_memory_stdlib_summaries() -> None:
    token = _recording.set(None)
    try:
        from . import print_protocol
        from .codegen import Program

        unit = Program([], None, {}, unit="builtins", exports=True, bootstrap=True)
        print_protocol.prepare(unit)
        prepare(unit)
    finally:
        _recording.reset(token)


def stdlib_type_summary() -> tuple[
    dict[tuple[str, str], ValueType],
    dict[tuple[str, str], ValueType],
    dict[str, ValueType],
]:
    """Return cached ``(exported_types, storage_methods, builtin_return_types)``
    inferred from the compiled stdlib sequence protocol."""
    global _CACHED_STDLIB_TYPES
    if _CACHED_STDLIB_TYPES is not None:
        return _CACHED_STDLIB_TYPES
    token = _recording.set(None)
    try:
        from .driver import cached_builtins_interface

        interface = cached_builtins_interface()
        _CACHED_STDLIB_TYPES = (
            dict(interface.exported_types),
            dict(interface.storage_methods),
            dict(interface.builtin_return_types),
        )
        return _CACHED_STDLIB_TYPES
    finally:
        _recording.reset(token)


def stdlib_target_summary() -> tuple[
    dict[tuple[str, str], Lambda | ClassConstructorTarget | ForeignCallTarget],
    dict[str, Lambda | ClassConstructorTarget | ForeignCallTarget],
]:
    """Return cached ``(exported_targets, builtin_targets)`` from the compiled
    stdlib protocols."""
    if _CACHED_STDLIB_TARGETS is None:
        _populate_in_memory_stdlib_summaries()
    assert _CACHED_STDLIB_TARGETS is not None
    return _CACHED_STDLIB_TARGETS


def prepare(program):
    """Compile the sequence protocols. Every program needs them: they own the
    method tables of the builtin storage types, including ``str``. Returns
    the protocol module's entry lambda, its exported functions by name, and
    the modules it imported, by name."""
    global _CACHED_STDLIB_TYPES, _CACHED_STDLIB_TARGETS
    path = STDLIB / "_sequences.py"
    imports = ModuleCompiler(
        path.name,
        prefix="mule_sequences_",
        primitives=PRIMITIVES,
        roots=(STDLIB / "bootstrap", STDLIB),
    )
    compilation = compile_toplevel(
        read_source(path),
        path.name,
        imports=imports,
        prefix="mule_sequences_",
        namespace="$mule.sequences",
        primitives=PRIMITIVES,
    )
    imports.expect_builtins(compilation, "$mule.sequences")
    imports.compile_pending()
    compilation.imported_function_types = imports.exported_types
    compilation.imported_targets = imports.exported_targets
    compilation.storage_methods = imports.storage_methods
    compilation.builtin_return_types = imports.builtin_return_types
    compilation.builtin_targets = imports.builtin_targets
    functions = exports(compilation.entry)
    for name, info in PROTOCOLS.items():
        if info.argument_types is None:
            continue
        function = functions.get(name)
        if function is None:
            raise ValueError(f"declared compiled protocol {name!r} has no definition")
        function.entry_parameter_types = protocol_argument_types(
            name, imports.builtin_targets
        )
    ir1_phases(compilation)
    imports.record_types("$mule.sequences", compilation)
    program.exported_types.update(imports.exported_types)
    program.exported_targets.update(imports.exported_targets)
    program.storage_methods.update(imports.storage_methods)
    program.builtin_return_types.update(imports.builtin_return_types)
    program.builtin_targets.update(imports.builtin_targets)
    _CACHED_STDLIB_TYPES = (
        dict(program.exported_types),
        dict(program.storage_methods),
        dict(program.builtin_return_types),
    )
    _CACHED_STDLIB_TARGETS = (
        dict(program.exported_targets),
        dict(program.builtin_targets),
    )
    add_lambdas(program, compilation.lambdas, compilation.facts)
    add_lambdas(program, imports.lambdas, imports.facts)
    return (
        compilation.entry,
        exports(compilation.entry),
        {
            name: module
            for name, module in imports.modules.items()
            if module.native or name.startswith("_")
        },
    )
