"""Compile Python formatting rules and bind their physical primitives."""

from .inputs import read_source
from .ir1util import known_calls, made_lambda
from .modules import STDLIB, ModuleCompiler
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
    path = STDLIB / "_format.py"
    imports = ModuleCompiler(
        path.name,
        prefix="mule_format_",
        primitives=PRIMITIVES,
        roots=(STDLIB / "bootstrap", STDLIB),
    )
    compilation = compile_protocol(
        read_source(path),
        path.name,
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
