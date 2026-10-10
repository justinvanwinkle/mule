"""Compile print's ending and conversion rules over physical stdout writes."""

from pathlib import Path

from .protocols import add_lambdas, compile_protocol


def prepare(program):
    """Compile the print protocol. Builtins remain available through the
    namespace even without a direct load, so it is always compiled."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_print.py"
    compilation = compile_protocol(
        path.read_text(), str(path), prefix="mule_print_", namespace="$mule.print"
    )
    add_lambdas(program, compilation.lambdas, compilation.facts)
    return compilation.entry
