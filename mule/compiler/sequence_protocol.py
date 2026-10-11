"""Compile sequence/exception language rules and bind physical storage operations."""

from pathlib import Path

from .modules import ModuleCompiler
from .node import CallKind, Combination
from .protocols import PRIMITIVES, add_lambdas, compile_protocol


def exports(entry) -> dict:
    """Functions the module defines, by public name."""
    functions = {}
    for block in entry.component.blocks():
        for node in block.nodes():
            if (
                isinstance(node, Combination)
                and node.kind is CallKind.KNOWN
                and node.fun_info.name == "%make_function"
            ):
                functions[node.info.public_name] = node.args[0].use().leaf
    return functions


def prepare(program):
    """Compile the sequence protocols. Every program needs them: they own the
    method tables of the builtin storage types, including ``str``. Returns
    the protocol module's entry lambda and its exported functions by name."""
    path = Path(__file__).resolve().parents[2] / "stdlib/_sequences.py"
    imports = ModuleCompiler(str(path), prefix="mule_sequences_", primitives=PRIMITIVES)
    compilation = compile_protocol(
        path.read_text(),
        str(path),
        imports=imports,
        prefix="mule_sequences_",
        namespace="$mule.sequences",
    )
    add_lambdas(program, compilation.lambdas, compilation.facts)
    add_lambdas(program, imports.lambdas, imports.facts)
    return compilation.entry, exports(compilation.entry)
