"""Authored flow graphs verify dominators, loop nesting, and error edges."""

import pytest

from mule.compiler.dfo import find_dfo, walk_successors
from mule.compiler.ir1util import (
    ctran_starts_block,
    link_blocks,
    link_node_to_previous_ctran,
)
from mule.compiler.loop import analyze, lowest_common_dominator
from mule.compiler.main import compile_source
from mule.compiler.node import (
    Component,
    Ctran,
    Lambda,
    Lexenv,
    NaturalLoop,
    Node,
    StrangeLoop,
)
from mule.compiler.policy import OptimizationPolicy


def graph(names, edges):
    component = Component("authored graph")
    owner = Lambda("graph", "graph", policy=OptimizationPolicy(), namespace="authored")
    lexenv = Lexenv({}, owner)
    blocks = {"head": component.head, "tail": component.tail}
    for name in names:
        ctran = Ctran()
        block = ctran_starts_block(ctran, component)
        node = Node(lexenv, None)
        link_node_to_previous_ctran(node, ctran)
        block.last = node
        blocks[name] = block
    for before, after in edges:
        link_blocks(blocks[before], blocks[after])
    find_dfo(component)
    analyze(component)
    return component, blocks


def test_diamond_immediate_dominators_and_shared_ancestor():
    component, blocks = graph(
        ("entry", "left", "right", "join"),
        (
            ("head", "entry"),
            ("entry", "left"),
            ("entry", "right"),
            ("left", "join"),
            ("right", "join"),
            ("join", "tail"),
        ),
    )
    assert blocks["entry"].dominator is component.head
    assert blocks["left"].dominator is blocks["entry"]
    assert blocks["right"].dominator is blocks["entry"]
    assert blocks["join"].dominator is blocks["entry"]
    assert component.tail.dominator is blocks["join"]
    assert component.head.dominator is None
    assert lowest_common_dominator(blocks["left"], blocks["right"]) is blocks["entry"]
    assert component.outer_loop.inferiors == []
    assert set(component.outer_loop.blocks) == set(blocks.values())


def test_natural_loop_membership_exits_and_nesting():
    component, blocks = graph(
        ("entry", "outer", "inner", "body", "latch", "exit"),
        (
            ("head", "entry"),
            ("entry", "outer"),
            ("outer", "inner"),
            ("outer", "exit"),
            ("inner", "body"),
            ("inner", "latch"),
            ("body", "inner"),
            ("latch", "outer"),
            ("exit", "tail"),
        ),
    )
    (outer,) = component.outer_loop.inferiors
    (inner,) = outer.inferiors
    assert isinstance(outer, NaturalLoop)
    assert isinstance(inner, NaturalLoop)
    assert outer.head is blocks["outer"]
    assert inner.head is blocks["inner"]
    assert outer.depth == 1
    assert inner.depth == 2
    assert set(outer.blocks) == {blocks["outer"], blocks["latch"]}
    assert set(inner.blocks) == {blocks["inner"], blocks["body"]}
    assert outer.exits == [blocks["outer"]]
    assert inner.exits == [blocks["inner"]]
    assert inner.superior is outer
    assert outer.superior is component.outer_loop
    assert blocks["exit"].loop is component.outer_loop


def test_irreducible_cycle_has_a_strange_loop_with_its_own_exit():
    component, blocks = graph(
        ("entry", "left", "right", "exit"),
        (
            ("head", "entry"),
            ("entry", "left"),
            ("entry", "right"),
            ("left", "right"),
            ("right", "left"),
            ("right", "exit"),
            ("exit", "tail"),
        ),
    )
    (loop,) = component.outer_loop.inferiors
    assert isinstance(loop, StrangeLoop)
    assert set(loop.blocks) == {blocks["left"], blocks["right"]}
    assert loop.exits == [blocks["right"]]
    assert loop.depth == 1
    assert blocks["left"].dominator is blocks["entry"]
    assert blocks["right"].dominator is blocks["entry"]


def test_loop_without_an_exit_belongs_to_the_outer_region():
    component, blocks = graph(
        ("entry", "body"),
        (("head", "entry"), ("entry", "body"), ("body", "body")),
    )
    (loop,) = component.outer_loop.inferiors
    assert loop.head is blocks["body"]
    assert loop.exits == []
    assert loop.depth == 1
    assert blocks["entry"].loop is component.outer_loop
    assert component.tail.dominator is None


def reachable_without(component, excluded):
    pending = [component.head]
    seen = set()
    while pending:
        block = pending.pop()
        if block is excluded or block in seen:
            continue
        seen.add(block)
        successors = (
            block.succ
            if block in (component.head, component.tail)
            else list(walk_successors(block))
        )
        pending.extend(successors)
    return seen


@pytest.mark.parametrize("seed", range(32))
def test_dominators_match_independent_vertex_removal_reachability(seed):
    names = ("a", "b", "c", "d")
    edges = {("head", "a"), ("a", "b"), ("b", "c"), ("c", "d"), ("d", "tail")}
    bits = seed * 1103515245 + 12345
    for index, (before, after) in enumerate(
        (before, after) for before in names for after in names
    ):
        if bits & (1 << index):
            edges.add((before, after))
    component, blocks = graph(names, sorted(edges))
    all_blocks = set(blocks.values())
    reachable = {
        excluded: reachable_without(component, excluded) for excluded in all_blocks
    }
    for block in all_blocks:
        expected = {
            excluded for excluded in all_blocks if block not in reachable[excluded]
        }
        actual = set()
        cursor = block
        while cursor is not None:
            assert cursor not in actual, "dominator ancestry contains a cycle"
            actual.add(cursor)
            cursor = cursor.dominator
        assert actual == expected
    regions = [component.outer_loop]
    assigned = []
    while regions:
        loop = regions.pop()
        assigned.extend(loop.blocks)
        regions.extend(loop.inferiors)
    assert len(assigned) == len(set(assigned))
    assert set(assigned) == all_blocks


@pytest.mark.parametrize(
    "source",
    [
        "while value:\n value -= 1\n",
        "while value:\n while other:\n  other -= 1\n value -= 1\n",
        "while True:\n pass\n",
        "while value:\n try:\n  value -= 1\n except ValueError:\n  break\n",
        "while value:\n try:\n  value -= 1\n finally:\n  other = 1\n",
    ],
)
def test_lowered_loops_partition_blocks_and_include_exception_edges(source):
    compilation = compile_source(source)
    for component in compilation.components:
        regions = [component.outer_loop]
        blocks = []
        while regions:
            loop = regions.pop()
            blocks.extend(loop.blocks)
            for inner in loop.inferiors:
                assert inner.superior is loop
                assert inner.depth == loop.depth + 1
            regions.extend(loop.inferiors)
        assert len(blocks) == len(set(blocks))
        assert set(blocks) == {component.head, *component.blocks(), component.tail}
