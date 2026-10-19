"""Compile sequence/exception language rules and bind physical storage operations."""

from pathlib import Path

from .ctype import ValueType
from .inputs import _recording, read_source
from .ir1util import known_calls, made_lambda
from .main import compile_toplevel, ir1_phases
from .modules import ModuleCompiler
from .node import linked
from .protocols import PRIMITIVES, add_lambdas

_CACHED_STDLIB_TYPES: (
    tuple[
        dict[tuple[str, str], ValueType],
        dict[tuple[str, str], ValueType],
        dict[str, ValueType],
    ]
    | None
) = None


def exports(entry) -> dict:
    """Functions the module defines, by public name."""
    return {
        linked(call.info, call, "function definition").public_name: made_lambda(call)
        for call in known_calls(entry, "%make_function")
    }


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


def prepare(program):
    """Compile the sequence protocols. Every program needs them: they own the
    method tables of the builtin storage types, including ``str``. Returns
    the protocol module's entry lambda, its exported functions by name, and
    the modules it imported, by name."""
    global _CACHED_STDLIB_TYPES
    path = Path(__file__).resolve().parents[2] / "stdlib/_sequences.py"
    imports = ModuleCompiler(str(path), prefix="mule_sequences_", primitives=PRIMITIVES)
    compilation = compile_toplevel(
        read_source(path),
        str(path),
        imports=imports,
        prefix="mule_sequences_",
        namespace="$mule.sequences",
        primitives=PRIMITIVES,
    )
    imports.compile_pending()
    compilation.imported_function_types = imports.exported_types
    compilation.storage_methods = imports.storage_methods
    compilation.builtin_return_types = imports.builtin_return_types
    ir1_phases(compilation)
    imports.record_types("$mule.sequences", compilation)
    _CACHED_STDLIB_TYPES = (
        dict(imports.exported_types),
        dict(imports.storage_methods),
        dict(imports.builtin_return_types),
    )
    program.exported_types = dict(imports.exported_types)
    program.storage_methods = dict(imports.storage_methods)
    program.builtin_return_types = dict(imports.builtin_return_types)
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
