"""Compile print's ending and conversion rules over physical stdout writes."""

from pathlib import Path

from .format_protocol import TextAccess
from .frontend import lower
from .protocols import Bindings, CompiledCall


def prepare(module):
    # Builtins remain available through the namespace even without a direct load.
    path = Path(__file__).resolve().parents[2] / "stdlib/_print.py"
    lowered = lower(
        path.read_text(), str(path), prefix="mule_print_", namespace="$mule.print"
    )
    primitives = {
        "_storage_length": TextAccess("mule_storage_length"),
        "_storage_read": TextAccess("mule_storage_read"),
        "_text_storage": TextAccess("mule_text_storage"),
        "_stdout_write": TextAccess("mule_stdout_write"),
        "_install_builtin": TextAccess("mule_install_builtin"),
        "_string_protocol": CompiledCall("mule_format_string"),
    }
    calls, references = {}, set()
    for function in lowered.functions:
        collector = Bindings(primitives)
        for block in function.blocks:
            for operation in block.instructions:
                operation.accept(collector)
        calls.update(
            ((function.name, result), primitive)
            for result, primitive in collector.calls.items()
        )
        references.update((function.name, result) for result in collector.references)
    module.functions.extend(lowered.functions)
    return lowered.entry, calls, references
