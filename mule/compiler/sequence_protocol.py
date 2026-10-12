"""Compile sequence/exception language rules and bind physical storage operations."""

from pathlib import Path

from .inputs import read_source
from .ir1util import known_calls, made_lambda
from .modules import ModuleCompiler
from .node import linked
from .protocols import PRIMITIVES, add_lambdas, compile_protocol


def exports(entry) -> dict:
    """Functions the module defines, by public name."""
    return {
        linked(call.info, call, "function definition").public_name: made_lambda(call)
        for call in known_calls(entry, "%make_function")
    }


def prepare(program):
    """Compile the sequence protocols. Every program needs them: they own the
    method tables of the builtin storage types, including ``str``. Returns
    the protocol module's entry lambda, its exported functions by name, and
    the modules it imported, by name."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_sequences.py"
    imports = ModuleCompiler(str(path), prefix="mule_sequences_", primitives=PRIMITIVES)
    compilation = compile_protocol(
        read_source(path),
        str(path),
        imports=imports,
        prefix="mule_sequences_",
        namespace="$mule.sequences",
    )
    imports.compile_pending()
    add_lambdas(program, compilation.lambdas, compilation.facts)
    add_lambdas(program, imports.lambdas, imports.facts)
    return compilation.entry, exports(compilation.entry), imports.modules
