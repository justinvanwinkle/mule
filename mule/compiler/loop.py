"""Dominators and loop nesting, following SBCL's compiler/loop.lisp."""

from .dfo import find_dfo, walk_successors
from .node import NaturalLoop, OuterLoop, StrangeLoop


class _Graph:
    """The component's normal and exceptional control-flow edges."""

    def __init__(self, component):
        self.blocks = [component.head, *component.blocks(), component.tail]
        owned = set(self.blocks)
        self.successors = {
            component.head: tuple(component.head.succ),
            component.tail: tuple(component.tail.succ),
        }
        self.successors.update(
            {
                block: tuple(
                    dict.fromkeys(
                        successor
                        for successor in walk_successors(block)
                        if successor in owned
                    )
                )
                for block in self.blocks[1:-1]
            }
        )
        for block in self.blocks[1:-1]:
            if not self.successors[block]:
                self.successors[block] = (component.tail,)
        self.predecessors = {block: [] for block in self.blocks}
        for block, successors in self.successors.items():
            for successor in successors:
                self.predecessors[successor].append(block)


def lowest_common_dominator(left, right):
    while left is not right:
        if left.number < right.number:
            left = left.dominator
        else:
            right = right.dominator
        if left is None or right is None:
            raise ValueError("blocks do not share a dominator tree")
    return left


def find_dominators(component):
    if component.reanalyze:
        find_dfo(component)
    graph = _Graph(component)
    head = component.head
    for block in graph.blocks:
        block.dominator = None
    head.dominator = head
    while True:
        changed = False
        for block in graph.blocks[1:]:
            dominator = None
            for predecessor in graph.predecessors[block]:
                if predecessor.dominator is None:
                    continue
                dominator = (
                    predecessor
                    if dominator is None
                    else lowest_common_dominator(predecessor, dominator)
                )
            if block.dominator is not dominator:
                block.dominator = dominator
                changed = True
        if not changed:
            break
    head.dominator = None


def _note_loop_head(head, graph):
    tails = [
        predecessor
        for predecessor in graph.predecessors[head]
        if predecessor.number <= head.number
    ]
    if all(tail.dominated_by(head) for tail in tails):
        return NaturalLoop(head, tails)
    return StrangeLoop(head, tails, graph.successors)


def _find_blocks_from_here(start, loop, graph):
    pending = [start]
    while pending:
        block = pending.pop()
        if block.loop is loop or not loop.accepts(block):
            continue
        inner = block.loop
        if inner is None:
            block.loop = loop
            loop.blocks.append(block)
            pending.extend(graph.predecessors[block])
        elif inner.superior is None:
            loop.attach(inner)
            pending.extend(inner.predecessor_seeds(graph.predecessors))
        elif inner.superior is not loop:
            pending.append(inner.superior.head)


def _find_loop_blocks(loop, graph):
    for tail in loop.tails:
        _find_blocks_from_here(tail, loop, graph)
    for seed in loop.completion_seeds(graph.predecessors):
        _find_blocks_from_here(seed, loop, graph)
    exits = []
    candidates = [*loop.blocks]
    for inner in loop.inferiors:
        candidates.extend(inner.exits)
    for block in candidates:
        if any(successor.loop is None for successor in graph.successors[block]):
            exits.append(block)
    loop.exits = list(dict.fromkeys(exits))


def loop_analyze(component):
    graph = _Graph(component)
    outer = OuterLoop(component.head, (component.tail,))
    component.outer_loop = outer
    for block in graph.blocks:
        block.loop = None
    for block in reversed(graph.blocks[1:-1]):
        if not any(
            predecessor.number <= block.number
            for predecessor in graph.predecessors[block]
        ):
            continue
        loop = _note_loop_head(block, graph)
        _find_loop_blocks(loop, graph)
        if not loop.exits:
            outer.attach(loop)
    _find_loop_blocks(outer, graph)
    outer.assign_depths()


def analyze(component):
    find_dominators(component)
    loop_analyze(component)
