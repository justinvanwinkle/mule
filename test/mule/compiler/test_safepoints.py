"""Safepoints follow the IR1 loop graph and native GC registration protocol."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.codegen import compile_component
from mule.compiler.ctype import UNKNOWN
from mule.compiler.ltn import EntryContext
from mule.compiler.main import compile_source


@pytest.mark.parametrize("debug", (0, 1, 3))
def test_entry_and_nested_loop_polls_follow_analyzed_headers(debug):
    compilation = compile_source(
        f"__debug_level__ = {debug}\n"
        "def count(n):\n"
        " total = 0\n"
        " while n:\n"
        "  m = n\n"
        "  while m:\n"
        "   total += m\n"
        "   m -= 1\n"
        "  n -= 1\n"
        " return total\n"
    )
    function = compilation.lambdas[1]
    context = EntryContext(None, parameters=tuple(UNKNOWN for _ in function.vars))
    ir2 = compile_component(function, context)
    loop_headers = {
        block for block in ir2.blocks if block.block.loop.is_header(block.block)
    }
    assert len(loop_headers) == 2
    polls = [
        (block, index)
        for block in ir2.blocks
        for index, vop in enumerate(block.vops)
        if vop.info.name == "insert-safepoint"
    ]
    assert len(polls) == 3
    assert {block for block, _ in polls} == loop_headers | {ir2.blocks[0]}
    assert all(index == 0 for block, index in polls if block in loop_headers)
    entry_names = [vop.info.name for vop in ir2.blocks[0].vops]
    assert entry_names.index("move-argument") < entry_names.index("insert-safepoint")


def test_straight_line_function_has_entry_poll_and_no_extra_polls():
    llvm = emit_llvm("def identity(value):\n return value\n", "identity.py")
    # Module body and the function's general compiled body each poll once.
    assert llvm.count("call void @mule_safepoint()") == 2
