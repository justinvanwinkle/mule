"""Compile Python formatting rules and bind their physical primitives."""

from pathlib import Path

from .inputs import read_source
from .ir1util import known_calls, made_lambda
from .modules import ModuleCompiler
from .node import linked
from .protocols import PRIMITIVES, add_lambdas, compile_protocol

ROLES = (
    "string",
    "representation",
    "format_value",
    "decimal_integer",
    "percent_decimal",
    "modulo",
    "ascii_representation",
    "_hex",
)


def prepare(program):
    """Compile the formatting protocol. Its functions are named by role so
    templates can call them directly (``mule_format_<role>``)."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_format.py"
    imports = ModuleCompiler(str(path), prefix="mule_format_", primitives=PRIMITIVES)
    compilation = compile_protocol(
        read_source(path),
        str(path),
        prefix="mule_format_",
        namespace="$mule.format",
        imports=imports,
    )
    functions = {
        linked(call.info, call, "function definition").public_name: made_lambda(call)
        for call in known_calls(compilation.entry, "%make_function")
    }
    for role in ROLES:
        functions[role].name = "mule_format_" + role
    add_lambdas(program, compilation.lambdas, compilation.facts)
    return compilation.entry
