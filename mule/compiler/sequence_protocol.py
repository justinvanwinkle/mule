"""Compile sequence/exception language rules and bind physical storage operations."""

from pathlib import Path

from .modules import ModuleCompiler
from .node import CallKind, Combination, GlobalVar, NodeVisitor, Ref
from .protocols import add_lambdas, compile_protocol

BUILTIN_NAMES = frozenset(
    {
        "range",
        "enumerate",
        "iter",
        "next",
        "list",
        "tuple",
        "chr",
        "min",
        "max",
        "BaseException",
        "Exception",
        "StopIteration",
        "IndexError",
        "KeyError",
        "ValueError",
        "TypeError",
        "RuntimeError",
        "ArithmeticError",
        "LookupError",
        "NameError",
        "UnboundLocalError",
        "ZeroDivisionError",
        "OverflowError",
        "AssertionError",
        "MemoryError",
        "SystemError",
        "AttributeError",
        "OSError",
        "EOFError",
        "RecursionError",
        "UnicodeError",
        "UnicodeEncodeError",
        "UnicodeDecodeError",
    }
)
_PROTOCOL_KNOWNS = frozenset(
    {"%raise", "%reraise", "%catch_exception", "%build_list", "%build_tuple"}
)


class NeedsProtocols(NodeVisitor):
    def __init__(self):
        self.needed = False

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and (
            node.fun_info.protocol is not None or node.fun_info.name in _PROTOCOL_KNOWNS
        ):
            self.needed = True

    def visit_Ref(self, node):
        if isinstance(node.leaf, GlobalVar) and node.leaf.name in BUILTIN_NAMES:
            self.needed = True


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
    """Compile the sequence protocols when the program needs them. Returns
    the protocol module's entry lambda and its exported functions by name."""
    visitor = NeedsProtocols()
    for lambda_ in program.lambdas:
        for block in lambda_.component.blocks():
            for node in block.nodes():
                node.accept(visitor)
    if not visitor.needed:
        return None, {}
    path = Path(__file__).resolve().parents[2] / "stdlib/_sequences.py"
    imports = ModuleCompiler(str(path), prefix="mule_sequences_")
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


_ = Ref
