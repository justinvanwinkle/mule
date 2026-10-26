"""IR1 graph construction and surgery, following ``src/compiler/ir1util.lisp``
and the linking functions at the top of ``ir1tran.lisp``.

Every mutation here maintains the reciprocal invariants that
``debug.check_ir1_consistency`` verifies: ctran/node linkage, lvar use/dest
links, leaf reference lists, block predecessor/successor lists, and the
depth-first block chain hanging off each component's head and tail.
"""

from __future__ import annotations

from collections.abc import Iterator
from typing import TYPE_CHECKING, overload

from .ctype import UNKNOWN
from .node import (
    BasicVar,
    Bind,
    Block,
    CallKind,
    Cast,
    Cleanup,
    CleanupKind,
    Combination,
    Component,
    Ctran,
    CtranKind,
    Enclose,
    Entry,
    Exit,
    GlobalVar,
    If,
    KnownFunction,
    Lambda,
    LambdaVar,
    Leaf,
    LeafVisitor,
    Lexenv,
    LoopTarget,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    Return,
    Set,
    ValuedNode,
    linked,
)

if TYPE_CHECKING:
    from .knownfun import FunInfo


class IR1Error(Exception):
    """An internal consistency violation while building or rewriting IR1."""


def aver(condition, message="IR1 invariant violated"):
    if not condition:
        raise IR1Error(message)


# Node and ctran linkage


def link_node_to_previous_ctran(node: Node, ctran: Ctran) -> None:
    if ctran.next is not None:
        raise IR1Error(f"{ctran!r} already leads to {ctran.next!r}")
    ctran.next = node
    node.prev = ctran


def use_ctran(node: Node, ctran: Ctran) -> None:
    """Make ``ctran`` what ``node`` evaluates next. An unused ctran joins the
    node's block; a block-start ctran ends the node's block and links it."""
    if ctran.kind is CtranKind.UNUSED:
        block = node.block
        ctran.block = block
        ctran.kind = CtranKind.INSIDE_BLOCK
        ctran.use = node
        node.next = ctran
    else:
        _use_block_start(node, ctran)


def _use_block_start(node: Node, ctran: Ctran) -> None:
    aver(ctran.kind is CtranKind.BLOCK_START)
    block = linked(ctran.block, ctran, "block")
    node_block = node.block
    if node_block.last is not None:
        raise IR1Error(f"{node_block!r} has already ended")
    node_block.last = node
    if node_block.succ:
        raise IR1Error(f"{node_block!r} already has successors")
    node_block.succ = [block]
    if node_block in block.pred:
        raise IR1Error(f"{node_block!r} already precedes {block!r}")
    block.pred.append(node_block)


def use_lvar(node: ValuedNode, lvar: Lvar | None) -> None:
    if node.lvar is not None:
        raise IR1Error(f"{node!r} already delivers to {node.lvar!r}")
    if lvar is not None:
        node.lvar = lvar
        aver(node not in lvar.uses)
        lvar.uses.append(node)
        reoptimize_lvar(lvar)


def use_continuation(node: ValuedNode, ctran: Ctran, lvar: Lvar | None) -> None:
    use_ctran(node, ctran)
    use_lvar(node, lvar)


def insert_node_before(old: Node, new: Node) -> None:
    prev = linked(old.prev, old, "preceding control position")
    temp = Ctran()
    prev.next = None
    link_node_to_previous_ctran(new, prev)
    use_ctran(new, temp)
    link_node_to_previous_ctran(old, temp)


def insert_node_after(old: Node, new: Node) -> None:
    following = old.next
    if following is not None:
        insert_node_before(linked(following.next, following, "next node"), new)
    else:
        ctran = Ctran()
        link_node_to_previous_ctran(new, ctran)
        old.block.last = new
        use_ctran(old, ctran)


class _ReoptimizeLinkedBranch(NodeVisitor):
    """A branch consuming a reoptimized node's value is reoptimized too."""

    def visit_If(self, node):
        if node.prev is not None:
            node.reoptimize = True


_REOPTIMIZE_LINKED_BRANCH = _ReoptimizeLinkedBranch()


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
    result = dest.value_lvar
    if result is not None and result.dest is not None:
        result.dest.accept(_REOPTIMIZE_LINKED_BRANCH)
    block = linked(prev.block, prev, "block")
    block.reoptimize = True
    component = block.component
    if component is not None and component.reoptimize is not True:
        component.reoptimize = "maybe"


# Blocks


def ctran_starts_block(ctran: Ctran, component: Component) -> Block:
    """Make ``ctran`` head a new block inserted before the component's
    ``last_block`` in the DFO chain, or return the block it already heads."""
    if ctran.kind is CtranKind.BLOCK_START:
        return linked(ctran.block, ctran, "block")
    aver(ctran.kind is CtranKind.UNUSED and ctran.block is None)
    following = component.last_block
    previous = linked(following.prev, following, "previous block")
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
        node_ends_block(linked(ctran.use, ctran, "use"))


def link_blocks(block1: Block, block2: Block) -> None:
    if block2 in block1.succ:
        raise IR1Error(f"{block2!r} already succeeds {block1!r}")
    block1.succ.append(block2)
    block2.pred.append(block1)


def unlink_blocks(block1: Block, block2: Block) -> None:
    if block2 not in block1.succ:
        raise IR1Error(f"{block2!r} does not succeed {block1!r}")
    block1.succ.remove(block2)
    block2.pred.remove(block1)


def change_block_successor(block: Block, old: Block, new: Block) -> None:
    """Swing the edge ``block -> old`` to ``block -> new``, fixing an IF."""
    unlink_blocks(block, old)
    last = block.last
    if last is not None and last.accept(_RetargetBranch(block, old, new)):
        return
    if new not in block.succ:
        link_blocks(block, new)


class _RetargetBranch(NodeVisitor):
    """Point a block's final branch at ``new`` instead of ``old``. Returns
    whether the branch already reaches ``new`` along an existing edge."""

    def __init__(self, block: Block, old: Block, new: Block):
        self.block = block
        self.old = old
        self.new = new

    def visit_Node(self, node):
        return False

    def visit_If(self, node):
        if node.consequent is self.old:
            node.consequent = self.new
        if node.alternative is self.old:
            node.alternative = self.new
        return node.consequent is node.alternative and self.new in self.block.succ


def add_to_dfo(block: Block, after: Block) -> None:
    following = linked(after.next, after, "next block")
    block.prev = after
    block.next = following
    after.next = block
    following.prev = block
    block.component = after.component


def remove_from_dfo(block: Block) -> None:
    previous = linked(block.prev, block, "previous block")
    following = linked(block.next, block, "next block")
    previous.next = following
    following.prev = previous
    block.next = block.prev = None


def node_ends_block(node: Node) -> Block | None:
    """Split ``node``'s block after ``node``; the remainder becomes a new block
    immediately following it in the DFO. Returns the new block if one is made."""
    block = node.block
    last = block.last
    if last is node:
        return None
    start = linked(node.next, node, "next control position")
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
    linked(block.component, block, "component").reanalyze = True
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
    last1 = linked(block1.last, block1, "last node")
    last2 = block2.last
    successors = list(block2.succ)
    start2 = linked(block2.start, block2, "start")
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
    block2.start = block2.last = None
    block2.component = None


# Context queries


def block_start_cleanup(block: Block) -> Cleanup | None:
    start = linked(block.start, block, "start")
    return linked(start.next, start, "next node").lexenv.cleanup


def block_end_cleanup(block: Block) -> Cleanup | None:
    return linked(block.last, block, "last node").lexenv.cleanup


def block_home_lambda(block: Block) -> Lambda:
    return linked(block.last, block, "last node").lexenv.lambda_


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


def delete_lvar_use(node: Node) -> None:
    lvar = node.detach_value()
    if lvar is not None:
        reoptimize_lvar(lvar)


def add_lvar_use(node: ValuedNode, lvar: Lvar) -> None:
    aver(node.lvar is None)
    node.lvar = lvar
    lvar.uses.append(node)
    reoptimize_lvar(lvar)


def substitute_lvar(new: Lvar, old: Lvar) -> None:
    """Replace ``old`` with ``new`` as the operand of ``old``'s destination."""
    dest = old.dest
    if dest is None:
        raise IR1Error(f"{old!r} has no destination to substitute in")
    dest.accept(_SubstituteOperand(new, old))
    old.dest = None
    new.dest = dest
    reoptimize_lvar(new)


def substitute_lvar_uses(new: Lvar | None, old: Lvar) -> None:
    """Move OLD's producers into NEW, preserving NEW's other branch uses."""
    for producer in list(old.uses):
        delete_lvar_use(producer)
        if new is not None:
            add_lvar_use(producer, new)
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
    block = node.block
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
    leaf.accept(_ReleasedReference())


class _ReleasedReference(LeafVisitor):
    """A lambda that loses its last reference may now be deleted."""

    def visit_Lambda(self, leaf):
        if not leaf.refs and leaf.kind.name not in ("OPTIONAL", "DELETED", "TOPLEVEL"):
            linked(leaf.component, leaf, "component").reanalyze = True


def delete_set(node: Set) -> None:
    node.var.sets.remove(node)


def unlink_node(node: Node) -> bool:
    """Remove ``node`` from its block. Returns True when the block was deleted
    because it held only this node."""
    delete_lvar_use(node)
    ctran = node.next
    following = ctran.next if ctran is not None else None
    if ctran is not None:
        ctran.next = ctran.use = ctran.block = None
    node.next = None
    prev = linked(node.prev, node, "preceding control position")
    block = linked(prev.block, prev, "block")
    prev_kind = prev.kind
    last = block.last
    if prev_kind is CtranKind.INSIDE_BLOCK or (
        prev_kind is CtranKind.BLOCK_START and node is not last
    ):
        if node is last:
            previous_node = linked(prev.use, prev, "use")
            block.last = previous_node
            previous_node.next = None
            prev.next = prev.use = prev.block = None
        else:
            following = linked(following, node, "following node")
            prev.next = following
            following.prev = prev
            following.accept(_ReoptimizeExposedBranch())
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
        linked(block.component, block, "component").delete_blocks.remove(block)
    remove_from_dfo(block)
    block.delete_p = True
    prev.next = prev.block = None
    block.start = block.last = block.component = None
    node.prev = None
    return True


class _ReoptimizeExposedBranch(NodeVisitor):
    """A branch whose predecessor was unlinked may now test a simpler value."""

    def visit_If(self, node):
        reoptimize_lvar(node.test)


def delete_block_lazily(block: Block) -> None:
    if not block.delete_p:
        block.delete_p = True
        linked(block.component, block, "component").delete_blocks.append(block)


def mark_for_deletion(block: Block) -> None:
    component = linked(block.component, block, "component")
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
        delete_lvar_use(node)
        prev = node.prev
        if prev is not None:
            prev.next = prev.use = prev.block = None
        next_ctran = node.next
        if next_ctran is not None:
            next_ctran.next = next_ctran.use = next_ctran.block = None
        node.prev = node.next = None
    remove_from_dfo(block)
    block.start = block.last = None
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


class _SetToVar(NodeVisitor):
    """Whether a node is a binding store to ``var``."""

    def __init__(self, var: LambdaVar):
        self.var = var

    def visit_Node(self, node) -> bool:
        return False

    def visit_Set(self, node: Set) -> bool:
        return node.var is self.var and node.value is not None


def lambda_var_bound_at(leaf: LambdaVar, ref: Ref) -> bool:
    """Whether ``leaf`` is structurally guaranteed to be bound at ``ref``
    even before flow analysis."""
    if leaf.expression_temporary:
        return True
    if leaf.indirect or any(store.value is None for store in leaf.sets):
        return False
    if leaf.home is not None and leaf in leaf.home.vars:
        return True
    if ref.prev is None:
        return False
    block = ref.block
    checker = _SetToVar(leaf)
    for node in block.nodes():
        if node is ref:
            break
        if node.accept(checker):
            return True
    if (
        leaf.home is not None
        and ref.home_lambda is leaf.home
        and leaf.home.bind is not None
    ):
        bind_block = leaf.home.bind.block
        if bind_block is not block and any(
            store.prev is not None
            and store.block is bind_block
            and store.value is not None
            for store in leaf.sets
        ):
            return True
    return False


def flushable_lambda_var_ref_p(leaf: LambdaVar, ref: Ref) -> bool:
    """Whether ``ref`` to ``leaf`` is proven bound and side-effect-free."""
    if leaf.indirect or any(store.value is None for store in leaf.sets):
        return False
    if leaf.type is not None:
        return not leaf.type.may_be_unbound
    if ref.derived_type is not None and ref.derived_type is not UNKNOWN:
        return not ref.derived_type.may_be_unbound
    return lambda_var_bound_at(leaf, ref)


class _IsFlushableLeaf(LeafVisitor):
    """Whether a ``Ref`` to a leaf is proven bound and side-effect-free."""

    def __init__(self, ref: Ref):
        self.ref = ref

    def visit_Leaf(self, leaf) -> bool:
        return False

    def visit_Constant(self, leaf) -> bool:
        return True

    def visit_KnownFunction(self, leaf) -> bool:
        return True

    def visit_Lambda(self, leaf) -> bool:
        return True

    def visit_LambdaVar(self, leaf: LambdaVar) -> bool:
        return flushable_lambda_var_ref_p(leaf, self.ref)

    def visit_GlobalVar(self, leaf: GlobalVar) -> bool:
        ref = self.ref
        return (
            ref.lexenv.policy.safety < 3
            and (ref.home_lambda is None or ref.home_lambda.policy.safety < 3)
            and not any(store.value is None for store in leaf.sets)
            and ref.derived_type is not None
            and ref.derived_type is not UNKNOWN
            and not ref.derived_type.may_be_unbound
        )


class _IsFlushableRef(NodeVisitor):
    """Whether a node is a ``Ref`` whose evaluation has no side effects and
    cannot raise ``UnboundLocalError`` or ``NameError`` (``flushable-reference-p``)."""

    def visit_Node(self, node) -> bool:
        return False

    def visit_Ref(self, node: Ref) -> bool:
        if node.final or node.allow_unbound:
            return False
        return node.leaf.accept(_IsFlushableLeaf(node))


_IS_FLUSHABLE_REF = _IsFlushableRef()


def is_flushable_ref(node: Node) -> bool:
    """Whether ``node`` is a side-effect-free, proven-bound ``Ref``."""
    return node.accept(_IS_FLUSHABLE_REF)


def is_pure_bound_ref(lvar: Lvar) -> bool:
    """Whether ``lvar`` is produced by a single flushable ``Ref``."""
    return len(lvar.uses) == 1 and is_flushable_ref(lvar.uses[0])


class _FlushDeadReference(NodeVisitor):
    """Deletes an unused reference or flushable call whose evaluation has no effect."""

    def visit_Ref(self, node: Ref):
        if node.lvar is None and is_flushable_ref(node):
            delete_ref(node)
            unlink_node(node)

    def visit_Combination(self, node: Combination):
        if (
            node.lvar is None
            and node.kind is CallKind.KNOWN
            and node.fun_info is not None
            and node.fun_info.flushable
        ):
            flush_combination(node)


_FLUSH_DEAD_REFERENCE = _FlushDeadReference()


def _flush_operand(lvar: Lvar | None) -> None:
    if lvar is None:
        return
    uses = list(lvar.uses)
    flush_dest(lvar)
    for use in uses:
        if use.prev is not None:
            use.accept(_FLUSH_DEAD_REFERENCE)


def flush_combination(node: Combination) -> None:
    """Delete an unused flushable call: its operands' producers are flushed
    (and may be deleted in turn) and the node is unlinked."""
    _flush_operand(node.fun)
    for arg in node.args:
        _flush_operand(arg)
    unlink_node(node)


def replace_combination_with_lvar(node: Combination, value: Lvar) -> None:
    """Replace ``node`` with its operand ``value`` and flush its other operands."""
    aver(value in node.args, f"{value!r} is not an argument of {node!r}")
    result = node.lvar
    delete_lvar_use(node)
    value.dest = None
    _flush_operand(node.fun)
    for arg in node.args:
        if arg is not value:
            _flush_operand(arg)
    node.args = []
    if result is not None:
        substitute_lvar_uses(result, value)
    else:
        _flush_operand(value)
    unlink_node(node)


class _RetargetKnownRef(NodeVisitor):
    def __init__(self, info: FunInfo):
        self.info = info

    def visit_Ref(self, node: Ref):
        if node in node.leaf.refs:
            node.leaf.refs.remove(node)
        leaf = KnownFunction(self.info)
        node.leaf = leaf
        leaf.refs.append(node)


def retarget_known_combination(
    node: Combination, info: FunInfo, args: list[Lvar]
) -> None:
    """Rewrite ``node`` in place to call known function ``info`` with ``args``."""
    new_args = list(args)
    for old_arg in list(node.args):
        if all(old_arg is not arg for arg in new_args):
            _flush_operand(old_arg)
    for arg in new_args:
        arg.dest = node
    node.args = new_args
    node.kind = CallKind.KNOWN
    node.fun_info = info
    node.info = None
    node.predicted = None
    node.proven = False
    node.derived_type = None
    node.reoptimize = True
    for use in node.fun.uses:
        use.accept(_RetargetKnownRef(info))
    reoptimize_lvar(node.lvar)


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


def insert_cast_after(producer: ValuedNode, cast_factory) -> Lvar:
    """Check a single producer immediately, before later operand side effects."""
    result = linked(producer.lvar, producer, "asserted result")
    aver(result.uses == [producer], "a result assertion requires one producer")
    value = Lvar()
    result.uses.remove(producer)
    producer.lvar = None
    add_lvar_use(producer, value)
    cast = cast_factory(value)
    value.dest = cast
    insert_node_after(producer, cast)
    use_lvar(cast, result)
    return result


def make_lexenv_cleanup(lexenv, kind: CleanupKind, **fields) -> Cleanup:
    cleanup = Cleanup(kind, **fields)
    cleanup.parent = lexenv.cleanup
    return cleanup


def leaf_refs_in_lambda(leaf: Leaf, lambda_: Lambda) -> list[Ref]:
    return [ref for ref in leaf.refs if ref.home_lambda is lambda_]


def var_sets_in_lambda(var: BasicVar, lambda_: Lambda) -> list[Set]:
    return [node for node in var.sets if node.home_lambda is lambda_]


def lambda_block(lambda_: Lambda) -> Block:
    return linked(lambda_.bind, lambda_, "bind").block


def return_block(node: Return) -> Block:
    return node.block


def class_method_definitions(lambda_: Lambda) -> dict[str, dict[str, Lambda]]:
    """The functions a lambda's class bodies store directly into their
    namespaces, by class identity and attribute name. Decorated definitions
    store a decorator's result instead and are not included."""
    visitor = _ClassMethodDefinitions()
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            node.accept(visitor)
    return visitor.classes


class _ClassMethodDefinitions(NodeVisitor):
    def __init__(self):
        self.classes: dict[str, dict[str, Lambda]] = {}

    def visit_Combination(self, node):
        if node.kind is not CallKind.KNOWN:
            return
        name = node.fun_info.name
        if name == "%class_store":
            value = node.args[1]
            target = made_function(value.uses[0]) if len(value.uses) == 1 else None
            if target is not None:
                self.classes.setdefault(node.info.identity, {})[node.info.name] = target


def made_lambda(node: Combination) -> Lambda:
    """The lambda a ``%make_function`` call makes a function object from."""
    result = node.args[0].use().accept(_MADE_LAMBDA)
    if result is None:
        raise ValueError(f"{node!r} does not make a function from a lambda")
    return result


def made_function(node: Node) -> Lambda | None:
    """The function made by a node or its single-assignment temporary."""
    return node.accept(_MadeFunction())


class _MadeFunction(NodeVisitor, LeafVisitor):
    def __init__(self):
        self.visited: set | None = None

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf):
        if len(leaf.sets) != 1:
            return None
        if self.visited is None:
            self.visited = {leaf}
        elif leaf in self.visited:
            return None
        else:
            self.visited.add(leaf)
        value = leaf.sets[0].value
        return value.uses[0].accept(self) if len(value.uses) == 1 else None

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and node.fun_info.name == "%make_function":
            return made_lambda(node)
        return None


class _MadeLambda(NodeVisitor, LeafVisitor):
    """A function-making call's first operand must reference a lambda."""

    def visit_Node(self, node):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        return None

    def visit_Lambda(self, leaf):
        return leaf


_MADE_LAMBDA = _MadeLambda()


class _Combinations(NodeVisitor):
    def visit_Combination(self, node):
        return node


_COMBINATIONS = _Combinations()


def combinations(lambda_: Lambda) -> Iterator[Combination]:
    """The calls in ``lambda_``'s component, in block order."""
    for block in linked(lambda_.component, lambda_, "component").blocks():
        for node in block.nodes():
            call = node.accept(_COMBINATIONS)
            if call is not None:
                yield call


def known_calls(lambda_: Lambda, name: str) -> Iterator[Combination]:
    """The calls of the compiler-known operation ``name`` in ``lambda_``'s
    component, in block order."""
    for call in combinations(lambda_):
        if (
            call.kind is CallKind.KNOWN
            and linked(call.fun_info, call, "known function").name == name
        ):
            yield call


class _CloneIR1Node(NodeVisitor):
    """Clone one IR1 node into a fresh component while keeping leaf identities."""

    def __init__(
        self,
        block_map: dict[Block, Block],
        ctran_map: dict[Ctran, Ctran],
        lvar_map: dict[Lvar, Lvar],
        cleanup_map: dict[Cleanup, Cleanup],
        lexenv_map: dict[Lexenv, Lexenv],
        saved_refs: dict[Leaf, list[Ref]],
        saved_sets: dict[BasicVar, list[Set]],
    ):
        self.block_map = block_map
        self.ctran_map = ctran_map
        self.lvar_map = lvar_map
        self.cleanup_map = cleanup_map
        self.lexenv_map = lexenv_map
        self.saved_refs = saved_refs
        self.saved_sets = saved_sets
        self.node_map: dict[Node, Node] = {}
        self.valued_map: dict[ValuedNode, ValuedNode] = {}
        self.bind_map: dict[Bind, Bind] = {}
        self.return_map: dict[Return, Return] = {}
        self.entry_map: dict[Entry, Entry] = {}
        self.exit_map: dict[Exit, Exit] = {}
        self.range_map: dict[object, object] = {}

    def map_ctran(self, ctran: Ctran | None) -> Ctran | None:
        if ctran is None:
            return None
        mapped = self.ctran_map.get(ctran)
        if mapped is None:
            mapped = Ctran()
            mapped.kind = ctran.kind
            mapped.block = self.block_map.get(ctran.block, ctran.block)
            self.ctran_map[ctran] = mapped
        return mapped

    @overload
    def map_lvar(self, lvar: Lvar) -> Lvar: ...

    @overload
    def map_lvar(self, lvar: None) -> None: ...

    def map_lvar(self, lvar: Lvar | None) -> Lvar | None:
        if lvar is None:
            return None
        mapped = self.lvar_map.get(lvar)
        if mapped is None:
            mapped = Lvar()
            mapped.derived_type = lvar.derived_type
            mapped.reoptimize = False
            mapped.info = lvar.info
            self.lvar_map[lvar] = mapped
        return mapped

    def map_cleanup(self, cleanup: Cleanup | None) -> Cleanup | None:
        if cleanup is None:
            return None
        mapped = self.cleanup_map.get(cleanup)
        if mapped is None:
            mapped = Cleanup(
                cleanup.kind,
                target=self.block_map.get(cleanup.target, cleanup.target),
                binding=cleanup.binding,
                parent=self.map_cleanup(cleanup.parent),
                info=cleanup.info,
                loops=cleanup.loops,
                class_scopes=cleanup.class_scopes,
                nlx_info=list(cleanup.nlx_info),
            )
            self.cleanup_map[cleanup] = mapped
        return mapped

    def map_lexenv(self, lexenv: Lexenv | None) -> Lexenv | None:
        if lexenv is None:
            return None
        mapped = self.lexenv_map.get(lexenv)
        if mapped is None:
            mapped = Lexenv(
                vars=lexenv.vars,
                lambda_=lexenv.lambda_,
                cleanup=self.map_cleanup(lexenv.cleanup),
                policy=lexenv.policy,
                loops=tuple(
                    LoopTarget(
                        self.block_map.get(lt.continue_block, lt.continue_block),
                        self.block_map.get(lt.break_block, lt.break_block),
                        self.map_cleanup(lt.cleanup),
                    )
                    for lt in lexenv.loops
                ),
                parent=self.map_lexenv(lexenv.parent),
            )
            self.lexenv_map[lexenv] = mapped
        return mapped

    def _finish_node[N: Node](self, old: N, new: N) -> N:
        new.prev = self.map_ctran(old.prev)
        new.next = self.map_ctran(old.next)
        new.reoptimize = False
        self.node_map[old] = new
        return new

    def _finish_valued[N: ValuedNode](self, old: N, new: N) -> N:
        self._finish_node(old, new)
        new.lvar = self.map_lvar(old.lvar)
        new.derived_type = old.derived_type
        self.valued_map[old] = new
        return new

    def visit_Ref(self, node: Ref) -> Ref:
        leaf = node.leaf
        if leaf not in self.saved_refs:
            self.saved_refs[leaf] = list(leaf.refs)
        cloned = Ref(self.map_lexenv(node.lexenv), node.source, leaf, final=node.final)
        if node in leaf.refs:
            leaf.refs[leaf.refs.index(node)] = cloned
        else:
            leaf.refs.append(cloned)
        return self._finish_valued(node, cloned)

    def visit_If(self, node: If) -> If:
        cloned = If(
            self.map_lexenv(node.lexenv),
            node.source,
            self.map_lvar(node.test),
            self.block_map[node.consequent],
            self.block_map[node.alternative],
        )
        return self._finish_node(node, cloned)

    def visit_Set(self, node: Set) -> Set:
        var = node.var
        if var not in self.saved_sets:
            self.saved_sets[var] = list(var.sets)
        cloned = Set(
            self.map_lexenv(node.lexenv),
            node.source,
            var,
            self.map_lvar(node.value),
            missing_ok=node.missing_ok,
        )
        if node in var.sets:
            var.sets[var.sets.index(node)] = cloned
        else:
            var.sets.append(cloned)
        return self._finish_valued(node, cloned)

    def visit_Combination(self, node: Combination) -> Combination:
        cloned = Combination(
            self.map_lexenv(node.lexenv),
            node.source,
            self.map_lvar(node.fun),
        )
        cloned.args = [self.map_lvar(arg) for arg in node.args]
        cloned.kind = node.kind
        cloned.fun_info = node.fun_info
        if (
            node.info is not None
            and node.fun_info is not None
            and node.fun_info.name
            in ("%range_prepare", "%range_available", "%range_advance")
        ):
            mapped_info = self.range_map.get(node.info)
            if mapped_info is None:
                mapped_info = node.info.clone(self.map_lvar)
                self.range_map[node.info] = mapped_info
            cloned.info = mapped_info
        else:
            cloned.info = node.info
        cloned.template = node.template
        cloned.predicted = node.predicted
        cloned.proven = node.proven
        return self._finish_valued(node, cloned)

    def visit_Bind(self, node: Bind) -> Bind:
        cloned = Bind(self.map_lexenv(node.lexenv), node.source)
        cloned.lambda_ = node.lambda_
        self.bind_map[node] = cloned
        return self._finish_node(node, cloned)

    def visit_Return(self, node: Return) -> Return:
        cloned = Return(
            self.map_lexenv(node.lexenv),
            node.source,
            self.map_lvar(node.result),
            node.lambda_,
        )
        cloned.result_type = node.result_type
        self.return_map[node] = cloned
        return self._finish_node(node, cloned)

    def visit_Cast(self, node: Cast) -> Cast:
        cloned = Cast(
            self.map_lexenv(node.lexenv),
            node.source,
            self.map_lvar(linked(node.value, node, "value")),
            node.contract,
            node.location,
            node.safety,
            node.declaration,
        )
        cloned.type_check = node.type_check
        return self._finish_valued(node, cloned)

    def visit_Entry(self, node: Entry) -> Entry:
        cloned = Entry(self.map_lexenv(node.lexenv), node.source)
        cloned.cleanup = self.map_cleanup(node.cleanup)
        self.entry_map[node] = cloned
        return self._finish_node(node, cloned)

    def visit_Exit(self, node: Exit) -> Exit:
        cloned = Exit(
            self.map_lexenv(node.lexenv),
            node.source,
            None,
            self.map_lvar(node.value),
        )
        self.exit_map[node] = cloned
        return self._finish_valued(node, cloned)

    def visit_Enclose(self, node: Enclose) -> Enclose:
        cloned = Enclose(self.map_lexenv(node.lexenv), node.source, list(node.funs))
        return self._finish_node(node, cloned)


def clone_lambda_component(lambda_: Lambda):
    """Temporarily replace ``lambda_``'s IR1 component with a deep clone of its
    control/dataflow graph while keeping ``Lambda`` and ``Leaf`` identities."""
    old_component = linked(lambda_.component, lambda_, "component")
    old_blocks = tuple(old_component.blocks())
    new_component = Component(old_component.name)
    new_component.kind = old_component.kind
    new_component.lambdas = list(old_component.lambdas)
    new_component.reoptimize = False
    new_component.reanalyze = False
    new_component.head.number = old_component.head.number
    new_component.tail.number = old_component.tail.number
    block_map: dict[Block, Block] = {
        old_component.head: new_component.head,
        old_component.tail: new_component.tail,
    }
    for old_block in old_blocks:
        new_block = Block(component=new_component)
        new_block.number = old_block.number
        new_block.reoptimize = False
        new_block.flush_p = old_block.flush_p
        new_block.type_check = old_block.type_check
        new_block.delete_p = old_block.delete_p
        block_map[old_block] = new_block
    prev_block = new_component.head
    for old_block in old_blocks:
        nb = block_map[old_block]
        prev_block.next = nb
        nb.prev = prev_block
        prev_block = nb
    prev_block.next = new_component.tail
    new_component.tail.prev = prev_block
    for old_block in (old_component.head, *old_blocks, old_component.tail):
        nb = block_map[old_block]
        nb.pred = [block_map[p] for p in old_block.pred if p in block_map]
        nb.succ = [block_map[s] for s in old_block.succ if s in block_map]
    ctran_map: dict[Ctran, Ctran] = {}
    lvar_map: dict[Lvar, Lvar] = {}
    cleanup_map: dict[Cleanup, Cleanup] = {}
    lexenv_map: dict[Lexenv, Lexenv] = {}
    saved_refs: dict[Leaf, list[Ref]] = {}
    saved_sets: dict[BasicVar, list[Set]] = {}
    cloner = _CloneIR1Node(
        block_map,
        ctran_map,
        lvar_map,
        cleanup_map,
        lexenv_map,
        saved_refs,
        saved_sets,
    )
    for old_block in old_blocks:
        for old_node in old_block.nodes():
            old_node.accept(cloner)
    node_map = cloner.node_map
    for old_ctran, new_ctran in ctran_map.items():
        new_ctran.next = (
            node_map.get(old_ctran.next) if old_ctran.next is not None else None
        )
        new_ctran.use = (
            node_map.get(old_ctran.use) if old_ctran.use is not None else None
        )
    for old_lvar, new_lvar in lvar_map.items():
        new_lvar.dest = (
            node_map.get(old_lvar.dest) if old_lvar.dest is not None else None
        )
        new_lvar.uses = [
            cloner.valued_map[u] for u in old_lvar.uses if u in cloner.valued_map
        ]
    for old_cleanup, new_cleanup in cleanup_map.items():
        if old_cleanup.mess_up is not None:
            new_cleanup.mess_up = node_map.get(old_cleanup.mess_up, old_cleanup.mess_up)
    for old_block in old_blocks:
        nb = block_map[old_block]
        nb.start = cloner.map_ctran(old_block.start)
        nb.last = node_map.get(old_block.last) if old_block.last is not None else None
    for old_entry, new_entry in cloner.entry_map.items():
        new_entry.exits = [
            cloner.exit_map[ex] for ex in old_entry.exits if ex in cloner.exit_map
        ]
    for old_exit, new_exit in cloner.exit_map.items():
        if old_exit.entry is not None:
            new_exit.entry = cloner.entry_map.get(old_exit.entry, old_exit.entry)
    saved_lambda = (
        lambda_.component,
        lambda_.bind,
        lambda_.return_,
        list(lambda_.entries),
        list(lambda_.locals),
    )
    lambda_.component = new_component
    if lambda_.bind is not None:
        lambda_.bind = cloner.bind_map.get(lambda_.bind, lambda_.bind)
    if lambda_.return_ is not None:
        lambda_.return_ = cloner.return_map.get(lambda_.return_, lambda_.return_)
    lambda_.entries = [cloner.entry_map.get(e, e) for e in lambda_.entries]

    # Track any leaves whose refs or sets are mutated by IR1 transforms while
    # the cloned component is active so restore() leaves no dangling refs.
    def restore() -> None:
        for block in list(new_component.blocks()):
            for node in list(block.nodes()):
                node.accept(_DetachClonedLeafRefs(saved_refs, saved_sets))
        for leaf, refs in saved_refs.items():
            leaf.refs[:] = refs
        for var, sets in saved_sets.items():
            var.sets[:] = sets
        (
            lambda_.component,
            lambda_.bind,
            lambda_.return_,
            entries,
            locals_list,
        ) = saved_lambda
        lambda_.entries[:] = entries
        lambda_.locals[:] = locals_list

    return new_component, node_map, restore


class _DetachClonedLeafRefs(NodeVisitor):
    """Remove any refs/sets created during cloned-component IR1 optimization
    on leaves that were not already in ``saved_refs`` / ``saved_sets``."""

    def __init__(
        self,
        saved_refs: dict[Leaf, list[Ref]],
        saved_sets: dict[BasicVar, list[Set]],
    ):
        self.saved_refs = saved_refs
        self.saved_sets = saved_sets

    def visit_Ref(self, node: Ref):
        if node.leaf not in self.saved_refs and node in node.leaf.refs:
            node.leaf.refs.remove(node)

    def visit_Set(self, node: Set):
        if node.var not in self.saved_sets and node in node.var.sets:
            node.var.sets.remove(node)
