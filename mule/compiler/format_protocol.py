"""Compile Python formatting rules and bind their physical primitives."""

from pathlib import Path

from .protocols import add_lambdas, compile_protocol

ROLES = (
    "string",
    "representation",
    "align_string",
    "format_value",
    "decimal_integer",
    "percent_decimal",
    "modulo",
)


def prepare(program):
    """Compile the formatting protocol. Its functions are named by role so
    templates can call them directly (``mule_format_<role>``)."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_format.py"
    compilation = compile_protocol(
        path.read_text(), str(path), prefix="mule_format_", namespace="$mule.format"
    )
    functions = compilation.lambdas[1:]
    for function, role in zip(functions, ROLES, strict=True):
        function.name = "mule_format_" + role
    add_lambdas(program, functions, compilation.facts)
