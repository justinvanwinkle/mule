"""IR1 conversion builds a consistent flow graph with SBCL's invariants."""

import pytest

from mule.compiler import emit_llvm
from mule.compiler.debug import InconsistentIR1, check_ir1_consistency
from mule.compiler.ir1util import block_exception_target, unlink_blocks
from mule.compiler.main import compile_toplevel
from mule.compiler.node import (
    Bind,
    CallKind,
    Combination,
    Exit,
    FunctionalKind,
    If,
    Ref,
    Return,
    Set,
)

SOURCES = [
    "x = 1\n",
    "print((1 if True else 2) if False else 3)\n",
    "print(1 and (2 or 3), 0 or (4 and 5))\n",
    "if True:\n if False:\n  print(1)\n else:\n  print(2)\n",
    "while False:\n while True:\n  break\n",
    "def f(x):\n if x:\n  return x\n return None\n",
    "def f(x):\n return\n x = 2\n",
    "def f(*args):\n for a in args:\n  print(a)\n else:\n  pass\n",
    "for i in range(3):\n if i == 1:\n  continue\n print(i)\n",
    "try:\n x = 1\nexcept ValueError as error:\n print(error)\nexcept Exception:\n raise\nelse:\n pass\n",
    "a = 1 < 2 < 3\nif 1 < 2 < 3:\n pass\n",
    "class C:\n def m(self, v):\n  self.x = v\n  return self.x\nC().m(1)\n",
    "assert 1 == 1, 'message'\nx = [1, (2, 3)]\nx[0] += 1\nprint(f'{x!r:>4}')\n",
    "def g(a, b=2, *, c):\n return (y := a + b) + c + y\n",
]


@pytest.mark.parametrize("source", SOURCES)
def test_conversion_produces_consistent_ir1(source):
    compilation = compile_toplevel(source)
    check_ir1_consistency(compilation.components)
    for component in compilation.components:
        for block in component.blocks():
            nodes = list(block.nodes())
            assert nodes, "blocks are never empty"
            assert block.last is nodes[-1]
            # Every node in a block shares one error destination, so the
            # destination is a property of the block.
            targets = {node.lexenv.cleanup for node in nodes}
            assert len(targets) == 1


@pytest.mark.parametrize("source", SOURCES)
def test_code_generation_accepts_every_converted_construct(source):
    assert "define i32 @main" in emit_llvm(source)


def test_each_lambda_has_its_own_component_with_bind_first():
    compilation = compile_toplevel("def f(x):\n return x\ndef g():\n return f(1)\n")
    assert [lambda_.name for lambda_ in compilation.lambdas] == [
        "mule_module",
        "mule_fn_1",
        "mule_fn_2",
    ]
    assert compilation.entry.kind is FunctionalKind.TOPLEVEL
    for lambda_ in compilation.lambdas:
        component = lambda_.component
        assert component.lambdas == [lambda_]
        first = next(iter(component.blocks()))
        assert isinstance(first.start.next, Bind)
        assert first.start.next.lambda_ is lambda_


def test_every_return_delivers_to_the_single_return_node():
    compilation = compile_toplevel("def f(x):\n if x:\n  return 1\n return 2\n")
    function = compilation.lambdas[1]
    returned = function.return_
    assert isinstance(returned, Return)
    assert len(returned.result.uses) == 2
    assert all(isinstance(use, Ref) for use in returned.result.uses)
    assert returned.block.succ == [function.component.tail]


def test_return_node_is_deleted_when_no_path_returns():
    compilation = compile_toplevel("def f():\n raise ValueError('x')\n")
    function = compilation.lambdas[1]
    assert function.return_ is None
    last = next(iter(function.component.blocks())).last
    assert isinstance(last, Combination)
    assert last.fun_info.name == "%raise"
    assert last.block.succ == []


def test_handler_covers_only_the_try_body():
    compilation = compile_toplevel(
        "def f():\n x = 1 // 0\n try:\n  y = 2\n except ZeroDivisionError:\n  pass\n"
    )
    function = compilation.lambdas[1]
    blocks = list(function.component.blocks())
    bind_block = blocks[0]
    division = next(
        node
        for node in bind_block.nodes()
        if isinstance(node, Combination) and node.fun_info.name == "%floor_divide"
    )
    assert division.block is bind_block
    assert block_exception_target(bind_block) is None
    body_blocks = [
        block
        for block in blocks
        if any(isinstance(node, Set) and node.var.name == "y" for node in block.nodes())
    ]
    assert body_blocks, "the try body assigns y"
    target = block_exception_target(body_blocks[0])
    assert target is not None
    dispatch = next(node for node in target.nodes() if isinstance(node, Combination))
    assert dispatch.fun_info.name == "%catch_exception"


def test_known_calls_reference_the_function_database():
    compilation = compile_toplevel("x = 1 + 2\n")
    calls = [
        node
        for block in compilation.entry.component.blocks()
        for node in block.nodes()
        if isinstance(node, Combination)
    ]
    assert [call.kind for call in calls] == [CallKind.KNOWN]
    assert calls[0].fun_info.name == "%add"
    assert calls[0].fun.use().leaf.fun_info is calls[0].fun_info
    assert [arg.use().leaf.value.value for arg in calls[0].args] == [1, 2]


def test_branches_end_blocks_and_fillers_only_fill_empty_bodies():
    compilation = compile_toplevel("if x:\n pass\nelse:\n y = 1\n")
    blocks = list(compilation.entry.component.blocks())
    branch = next(block for block in blocks if isinstance(block.last, If))
    assert branch.last.consequent in branch.succ
    assert branch.last.alternative in branch.succ
    fillers = [
        node
        for block in blocks
        for node in block.nodes()
        if isinstance(node, Exit) and node.entry is None
    ]
    assert len(fillers) == 1
    assert fillers[0].block is branch.last.consequent


def test_consistency_check_rejects_broken_links():
    compilation = compile_toplevel("if x:\n y = 1\nelse:\n y = 2\n")
    blocks = list(compilation.entry.component.blocks())
    branch = next(block for block in blocks if isinstance(block.last, If))
    unlink_blocks(branch, branch.last.consequent)
    with pytest.raises(InconsistentIR1):
        check_ir1_consistency(compilation.components)
