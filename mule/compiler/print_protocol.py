"""Compile print's ending and conversion rules over physical stdout writes."""

from pathlib import Path

from .inputs import read_source
from .modules import ModuleCompiler
from .protocols import PRIMITIVES, add_lambdas, compile_protocol


def prepare(program):
    """Compile the print protocol. Builtins remain available through the
    namespace even without a direct load, so it is always compiled."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_print.py"
    imports = ModuleCompiler(str(path), prefix="mule_print_", primitives=PRIMITIVES)
    compilation = compile_protocol(
        read_source(path),
        str(path),
        prefix="mule_print_",
        namespace="$mule.print",
        imports=imports,
    )
    imports.record_types("$mule.print", compilation)
    program.exported_types.update(imports.exported_types)
    program.exported_targets.update(imports.exported_targets)
    program.builtin_return_types.update(imports.builtin_return_types)
    program.builtin_targets.update(imports.builtin_targets)
    add_lambdas(program, compilation.lambdas, compilation.facts)
    return compilation.entry
