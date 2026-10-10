"""Public conversion entry: Python source to the control-flow IR.

Conversion builds IR1 (``main.compile_toplevel``) and lowers it through the
temporary bridge to the IR that analysis, representation selection, and LLVM
emission still consume.
"""

from . import ir, ir1bridge
from .ir1tran import UnsupportedSyntax
from .main import compile_toplevel, ir1_phases

__all__ = ["UnsupportedSyntax", "lower"]


def lower(
    source: str,
    filename: str = "<unknown>",
    *,
    imports=None,
    prefix="",
    namespace="__main__",
) -> ir.Module:
    compilation = compile_toplevel(
        source, filename, imports=imports, prefix=prefix, namespace=namespace
    )
    ir1_phases(compilation)
    module = ir1bridge.lower_module(compilation)
    if imports is None:
        ir.verify(module)
    return module
