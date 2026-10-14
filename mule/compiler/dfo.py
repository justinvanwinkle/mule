"""Depth-first ordering and reachability, following ``src/compiler/dfo.lisp``.

``find_dfo`` walks a component from its head, reorders the block chain into
depth-first order, numbers the blocks, and deletes unreachable blocks.
``find_initial_dfo`` splits the initial component produced by conversion into
one component per lambda; local-call analysis joins components again when a
call links two lambdas.

Python adaptation: a block's error destination (the handler dispatch block
designated by its cleanup) counts as a successor for reachability, since no
normal edge leads into a handler. SBCL reaches its non-local-exit entry blocks
from the component head after environment analysis instead.
"""

from __future__ import annotations

from .ir1util import (
    add_to_dfo,
    block_exception_target,
    delete_block,
    link_blocks,
    remove_from_dfo,
    unlink_blocks,
)
from .node import Block, Component, ComponentKind, FunctionalKind, linked


def walk_successors(block: Block):
    """Normal successors followed by the error destination, if any."""
    yield from block.succ
    target = block_exception_target(block)
    if target is not None:
        yield target


def clear_flags(component: Component) -> None:
    component.head.flag = True
    component.tail.flag = True
    for block in component.blocks():
        block.flag = None


def find_dfo(component: Component) -> None:
    clear_flags(component)
    component.reanalyze = False
    head = component.head
    while True:
        unvisited = next(
            (entry for entry in head.succ if not entry.flag and not entry.delete_p),
            None,
        )
        if unvisited is None:
            break
        _find_dfo_aux(unvisited, head)
    number = 1
    component.tail.number = number
    block = linked(component.tail.prev, component.tail, "previous block")
    while block is not head:
        previous = block.prev
        if block.flag:
            number += 1
            block.number = number
        else:
            delete_block(block)
        block = linked(previous, block, "previous block")
    component.head.number = number + 1
    for block in list(component.delete_blocks):
        if block.component is not None:
            delete_block(block)
    component.delete_blocks = []


def _find_dfo_aux(block: Block, head: Block) -> None:
    """Depth-first walk inserting each finished block right after ``head``,
    which yields depth-first order once every successor has been placed."""
    stack = [(block, walk_successors(block))]
    block.flag = True
    while stack:
        current, successors = stack[-1]
        successor = next(successors, None)
        if successor is None:
            stack.pop()
            remove_from_dfo(current)
            add_to_dfo(current, head)
            continue
        if successor.flag or successor.delete_p or successor.start is None:
            continue
        successor.flag = True
        stack.append((successor, walk_successors(successor)))


def find_initial_dfo(initial: Component, lambdas) -> list[Component]:
    """Move each lambda's blocks out of the initial component into a component
    of its own, then compute that component's depth-first order."""
    components = []
    for lambda_ in lambdas:
        component = Component(lambda_.name)
        component.kind = (
            ComponentKind.TOPLEVEL
            if lambda_.kind is FunctionalKind.TOPLEVEL
            else ComponentKind.NORMAL
        )
        component.lambdas = [lambda_]
        lambda_.component = component
        bind_block = lambda_.bind.block
        unlink_blocks(initial.head, bind_block)
        link_blocks(component.head, bind_block)
        for block in _reachable(bind_block):
            if initial.tail in block.succ:
                unlink_blocks(block, initial.tail)
                link_blocks(block, component.tail)
            remove_from_dfo(block)
            last = linked(component.tail.prev, component.tail, "previous block")
            add_to_dfo(block, last)
            block.component = component
        find_dfo(component)
        components.append(component)
    for block in list(initial.blocks()):
        delete_block(block)
    initial.kind = ComponentKind.DELETED
    return components


def _reachable(start: Block) -> list[Block]:
    seen = []
    visited = set()
    pending = [start]
    while pending:
        block = pending.pop()
        if id(block) in visited or block.start is None:
            continue
        visited.add(id(block))
        seen.append(block)
        pending.extend(walk_successors(block))
    return seen
