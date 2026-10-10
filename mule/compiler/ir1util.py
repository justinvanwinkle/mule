"""IR1 graph construction and surgery, following ``src/compiler/ir1util.lisp``
and the linking functions at the top of ``ir1tran.lisp``.

Every mutation here maintains the reciprocal invariants that
``debug.check_ir1_consistency`` verifies: ctran/node linkage, lvar use/dest
links, leaf reference lists, block predecessor/successor lists, and the
depth-first block chain hanging off each component's head and tail.
"""

from __future__ import annotations

from .node import (
    BasicVar,
    Block,
    Cleanup,
    CleanupKind,
    Combination,
    Component,
    Ctran,
    CtranKind,
    Exit,
    If,
    Lambda,
    Leaf,
    Lvar,
    Node,
    Ref,
    Return,
    Set,
    ValuedNode,
)


class IR1Error(Exception):
    """An internal consistency violation while building or rewriting IR1."""


def aver(condition, message="IR1 invariant violated"):
    if not condition:
        raise IR1Error(message)


# Node and ctran linkage


def link_node_to_previous_ctran(node: Node, ctran: Ctran) -> None:
    aver(ctran.next is None, f"{ctran!r} already leads to {ctran.next!r}")
    ctran.next = node
    node.prev = ctran


def use_ctran(node: Node, ctran: Ctran) -> None:
    """Make ``ctran`` what ``node`` evaluates next. An unused ctran joins the
    node's block; a block-start ctran ends the node's block and links it."""
    if ctran.kind is CtranKind.UNUSED:
        block = node.prev.block
        ctran.block = block
        ctran.kind = CtranKind.INSIDE_BLOCK
        ctran.use = node
        node.next = ctran
    else:
        _use_block_start(node, ctran)


def _use_block_start(node: Node, ctran: Ctran) -> None:
    aver(ctran.kind is CtranKind.BLOCK_START)
    block = ctran.block
    node_block = node.prev.block
    aver(node_block.last is None, f"{node_block!r} has already ended")
    node_block.last = node
    aver(not node_block.succ, f"{node_block!r} already has successors")
    node_block.succ = [block]
    aver(node_block not in block.pred, f"{node_block!r} already precedes {block!r}")
    block.pred.append(node_block)


def use_lvar(node: ValuedNode, lvar: Lvar | None) -> None:
    aver(node.lvar is None, f"{node!r} already delivers to {node.lvar!r}")
    if lvar is not None:
        node.lvar = lvar
        aver(node not in lvar.uses)
        lvar.uses.append(node)
        reoptimize_lvar(lvar)


def use_continuation(node: ValuedNode, ctran: Ctran, lvar: Lvar | None) -> None:
    use_ctran(node, ctran)
    use_lvar(node, lvar)


def insert_node_before(old: Node, new: Node) -> None:
    prev = old.prev
    temp = Ctran()
    prev.next = None
    link_node_to_previous_ctran(new, prev)
    use_ctran(new, temp)
    link_node_to_previous_ctran(old, temp)


def insert_node_after(old: Node, new: Node) -> None:
    following = old.next
    if following is not None:
        insert_node_before(following.next, new)
    else:
        ctran = Ctran()
        link_node_to_previous_ctran(new, ctran)
        old.block.last = new
        use_ctran(old, ctran)


def reoptimize_lvar(lvar: Lvar | None) -> None:
    """Invalidate cached facts about ``lvar`` and request reoptimization of
    its destination, its block, and its component (``ir1opt.lisp:326``)."""
    if lvar is None:
        return
    lvar.derived_type = None
    dest = lvar.dest
    if dest is None:
        return
    lvar.reoptimize = True
    dest.reoptimize = True
    prev = dest.prev
    if prev is None:
        return
    block = prev.block
    if isinstance(dest, ValuedNode) and dest.lvar is not None:
        test = dest.lvar.dest
        if isinstance(test, If) and test.prev is not None:
            test.reoptimize = True
    block.reoptimize = True
    component = block.component
    if component is not None and component.reoptimize is not True:
        component.reoptimize = "maybe"


# Blocks


def ctran_starts_block(ctran: Ctran, component: Component) -> Block:
    """Make ``ctran`` head a new block inserted before the component's
    ``last_block`` in the DFO chain, or return the block it already heads."""
    if ctran.kind is CtranKind.BLOCK_START:
        return ctran.block
    aver(ctran.kind is CtranKind.UNUSED and ctran.block is None)
    following = component.last_block
    previous = following.prev
    block = Block(ctran, component=component)
    block.next = following
    block.prev = previous
    following.prev = block
    previous.next = block
    ctran.block = block
    ctran.kind = CtranKind.BLOCK_START
    aver(ctran.use is None)
    return block


def ensure_block_start(ctran: Ctran, component: Component) -> None:
    if ctran.kind is CtranKind.BLOCK_START:
        return
    if ctran.kind is CtranKind.UNUSED:
        ctran_starts_block(ctran, component)
    else:
        node_ends_block(ctran.use)


def link_blocks(block1: Block, block2: Block) -> None:
    aver(block2 not in block1.succ, f"{block2!r} already succeeds {block1!r}")
    block1.succ.append(block2)
    block2.pred.append(block1)


def unlink_blocks(block1: Block, block2: Block) -> None:
    aver(block2 in block1.succ, f"{block2!r} does not succeed {block1!r}")
    block1.succ.remove(block2)
    block2.pred.remove(block1)


def change_block_successor(block: Block, old: Block, new: Block) -> None:
    """Swing the edge ``block -> old`` to ``block -> new``, fixing an IF."""
    unlink_blocks(block, old)
    last = block.last
    if isinstance(last, If):
        if last.consequent is old:
            last.consequent = new
        if last.alternative is old:
            last.alternative = new
        if last.consequent is last.alternative and new in block.succ:
            return
    if new not in block.succ:
        link_blocks(block, new)


def add_to_dfo(block: Block, after: Block) -> None:
    following = after.next
    block.prev = after
    block.next = following
    after.next = block
    following.prev = block
    block.component = after.component


def remove_from_dfo(block: Block) -> None:
    block.prev.next = block.next
    block.next.prev = block.prev
    block.next = block.prev = None


def node_ends_block(node: Node) -> Block | None:
    """Split ``node``'s block after ``node``; the remainder becomes a new block
    immediately following it in the DFO. Returns the new block if one is made."""
    block = node.block
    start = node.next
    last = block.last
    if last is node:
        return None
    aver(start.kind is CtranKind.INSIDE_BLOCK and not block.delete_p)
    successors = list(block.succ)
    new_block = Block(start, component=block.component)
    new_block.last = last
    new_block.type_check = block.type_check
    start.kind = CtranKind.BLOCK_START
    start.use = None
    block.last = node
    node.next = None
    for successor in successors:
        successor.pred[successor.pred.index(block)] = new_block
    new_block.succ = successors
    block.succ = []
    link_blocks(block, new_block)
    add_to_dfo(new_block, block)
    block.component.reanalyze = True
    ctran = start
    while ctran is not None:
        ctran.block = new_block
        following = ctran.next
        ctran = following.next if following is not None else None
    return new_block


def join_blocks_if_possible(component: Component) -> None:
    for block in list(component.blocks()):
        while len(block.succ) == 1 and join_successor_if_possible(block):
            pass


def join_successor_if_possible(block: Block) -> bool:
    following = block.succ[0]
    if following.start is None:
        return False
    if (
        len(following.pred) > 1
        or following is block
        or block_end_cleanup(block) is not block_start_cleanup(following)
        or block_home_lambda(block) is not block_home_lambda(following)
        or block.type_check != following.type_check
    ):
        return False
    join_blocks(block, following)
    return True


def join_blocks(block1: Block, block2: Block) -> None:
    last1 = block1.last
    last2 = block2.last
    successors = list(block2.succ)
    start2 = block2.start
    ctran = start2
    while ctran is not None:
        ctran.block = block1
        following = ctran.next
        ctran = following.next if following is not None else None
    unlink_blocks(block1, block2)
    for successor in successors:
        unlink_blocks(block2, successor)
        link_blocks(block1, successor)
    start2.kind = CtranKind.INSIDE_BLOCK
    last1.next = start2
    start2.use = last1
    block1.last = last2
    block1.reoptimize |= block2.reoptimize
    block1.flush_p |= block2.flush_p
    block1.type_check |= block2.type_check
    remove_from_dfo(block2)
    block2.component = None


# Context queries


def block_start_cleanup(block: Block) -> Cleanup | None:
    return block.start.next.lexenv.cleanup


def block_end_cleanup(block: Block) -> Cleanup | None:
    return block.last.lexenv.cleanup


def block_home_lambda(block: Block) -> Lambda:
    return block.last.lexenv.lambda_


def node_enclosing_cleanup(node: Node) -> Cleanup | None:
    return node.lexenv.cleanup


def block_exception_target(block: Block) -> Block | None:
    """The block that receives an error raised while ``block`` executes.

    Python adaptation of cleanup lookup: the innermost handler cleanup in
    effect at the block's first node designates the dispatch block. A block
    whose first node is a handler's own dispatch belongs to the region outside
    that handler, which is why the query reads the start node's lexenv.
    """
    cleanup = block_start_cleanup(block)
    while cleanup is not None:
        if cleanup.target is not None:
            return cleanup.target
        cleanup = cleanup.parent
    return None


# Lvars and leaves


def delete_lvar_use(node: ValuedNode) -> None:
    lvar = node.lvar
    if lvar is None:
        return
    lvar.uses.remove(node)
    node.lvar = None
    reoptimize_lvar(lvar)


def add_lvar_use(node: ValuedNode, lvar: Lvar) -> None:
    aver(node.lvar is None)
    node.lvar = lvar
    lvar.uses.append(node)
    reoptimize_lvar(lvar)


def substitute_lvar(new: Lvar, old: Lvar) -> None:
    """Replace ``old`` with ``new`` as the operand of ``old``'s destination."""
    dest = old.dest
    aver(dest is not None, f"{old!r} has no destination to substitute in")
    dest.accept(_SubstituteOperand(new, old))
    new.dest = dest
    reoptimize_lvar(new)


class _SubstituteOperand:
    def __init__(self, new, old):
        self.new, self.old = new, old

    def swap(self, value):
        return self.new if value is self.old else value

    def visit_Node(self, node):
        raise IR1Error(f"{node!r} has no lvar operands")

    def visit_If(self, node):
        node.test = self.swap(node.test)

    def visit_Set(self, node):
        node.value = self.swap(node.value)

    def visit_Combination(self, node):
        node.fun = self.swap(node.fun)
        node.args = [self.swap(arg) for arg in node.args]

    def visit_Return(self, node):
        node.result = self.swap(node.result)

    def visit_Cast(self, node):
        node.value = self.swap(node.value)

    def visit_Exit(self, node):
        node.value = self.swap(node.value)


def flush_node(node: ValuedNode) -> None:
    """Record that ``node``'s value is no longer used; the node itself stays
    until dead-code flushing decides whether it may be deleted."""
    block = node.prev.block
    component = block.component
    if component is not None:
        component.reoptimize = True
    block.flush_p = True
    node.lvar = None


def flush_dest(lvar: Lvar | None) -> None:
    if lvar is None:
        return
    lvar.dest = None
    for use in list(lvar.uses):
        flush_node(use)
    lvar.uses = []


def delete_ref(ref: Ref) -> None:
    leaf = ref.leaf
    leaf.refs.remove(ref)
    if (
        isinstance(leaf, Lambda)
        and not leaf.refs
        and leaf.kind.name not in ("OPTIONAL", "DELETED", "TOPLEVEL")
    ):
        leaf.component.reanalyze = True


def delete_set(node: Set) -> None:
    node.var.sets.remove(node)


def unlink_node(node: Node) -> bool:
    """Remove ``node`` from its block. Returns True when the block was deleted
    because it held only this node."""
    if isinstance(node, ValuedNode):
        delete_lvar_use(node)
    ctran = node.next
    following = ctran.next if ctran is not None else None
    prev = node.prev
    block = prev.block
    prev_kind = prev.kind
    last = block.last
    if prev_kind is CtranKind.INSIDE_BLOCK or (
        prev_kind is CtranKind.BLOCK_START and node is not last
    ):
        if node is last:
            block.last = prev.use
            prev.use.next = None
        else:
            prev.next = following
            following.prev = prev
            if isinstance(following, If):
                reoptimize_lvar(following.test)
        node.prev = None
        return False
    aver(prev_kind is CtranKind.BLOCK_START and node is last)
    successors = block.succ
    aver(len(successors) == 1, "only a straight-line block can lose its last node")
    following_block = successors[0]
    if following_block is block:
        exit_node = Exit(node.lexenv, node.source)
        prev.next = None
        link_node_to_previous_ctran(exit_node, prev)
        block.last = exit_node
        node.prev = None
        return False
    aver(block_start_cleanup(block) is block_end_cleanup(block))
    unlink_blocks(block, following_block)
    for pred in list(block.pred):
        change_block_successor(pred, block, following_block)
    if block.delete_p:
        block.component.delete_blocks.remove(block)
    remove_from_dfo(block)
    block.delete_p = True
    node.prev = None
    return True


def delete_block_lazily(block: Block) -> None:
    if not block.delete_p:
        block.delete_p = True
        block.component.delete_blocks.append(block)


def mark_for_deletion(block: Block) -> None:
    component = block.component
    head = component.head

    def helper(current):
        delete_block_lazily(current)
        for pred in current.pred:
            if not pred.delete_p and pred is not head:
                helper(pred)

    if not block.delete_p:
        helper(block)
        component.reanalyze = True


def delete_block(block: Block) -> None:
    """Unlink ``block`` from the graph and release its nodes' semantic links."""
    if block.component is None:
        return
    block.delete_p = True
    for pred in list(block.pred):
        unlink_blocks(pred, block)
    for succ in list(block.succ):
        unlink_blocks(block, succ)
    for node in list(block.nodes()):
        node.accept(_DeleteNode())
        if isinstance(node, ValuedNode):
            delete_lvar_use(node)
        node.prev = None
    remove_from_dfo(block)
    block.component = None


class _DeleteNode:
    """Release references a deleted node holds in leaves and operand lvars."""

    def visit_Node(self, node):
        pass

    def visit_Ref(self, node):
        delete_ref(node)

    def visit_If(self, node):
        flush_dest(node.test)

    def visit_Set(self, node):
        delete_set(node)
        flush_dest(node.value)

    def visit_Combination(self, node):
        flush_dest(node.fun)
        for arg in node.args:
            flush_dest(arg)

    def visit_Return(self, node):
        flush_dest(node.result)
        if node.lambda_.return_ is node:
            node.lambda_.return_ = None

    def visit_Cast(self, node):
        flush_dest(node.value)

    def visit_Exit(self, node):
        flush_dest(node.value)
        if node.entry is not None:
            node.entry.exits.remove(node)

    def visit_Bind(self, node):
        if node.lambda_ is not None and node.lambda_.bind is node:
            node.lambda_.bind = None

    def visit_Entry(self, node):
        home = node.home_lambda
        if node in home.entries:
            home.entries.remove(node)

    def visit_Enclose(self, node):
        pass


def flush_combination(node: Combination) -> None:
    """Delete an unused flushable call: its operands' producers are flushed
    (and may be deleted in turn) and the node is unlinked."""
    flush_dest(node.fun)
    for arg in node.args:
        flush_dest(arg)
    unlink_node(node)


def insert_cast_before(next_node: Node, lvar: Lvar, cast_factory) -> Lvar:
    """Interpose a cast between ``lvar``'s producers and its destination
    ``next_node``. The producers move to a fresh lvar feeding the cast made by
    ``cast_factory(value_lvar)``; the cast then delivers to ``lvar``."""
    aver(lvar.dest is next_node, f"{lvar!r} is not consumed by {next_node!r}")
    value = Lvar()
    for use in list(lvar.uses):
        lvar.uses.remove(use)
        use.lvar = None
        add_lvar_use(use, value)
    cast = cast_factory(value)
    value.dest = cast
    insert_node_before(next_node, cast)
    use_lvar(cast, lvar)
    return lvar


def make_lexenv_cleanup(lexenv, kind: CleanupKind, **fields) -> Cleanup:
    cleanup = Cleanup(kind, **fields)
    cleanup.parent = lexenv.cleanup
    return cleanup


def leaf_refs_in_lambda(leaf: Leaf, lambda_: Lambda) -> list[Ref]:
    return [ref for ref in leaf.refs if ref.home_lambda is lambda_]


def var_sets_in_lambda(var: BasicVar, lambda_: Lambda) -> list[Set]:
    return [node for node in var.sets if node.home_lambda is lambda_]


def lambda_block(lambda_: Lambda) -> Block:
    return lambda_.bind.block


def return_block(node: Return) -> Block:
    return node.block
