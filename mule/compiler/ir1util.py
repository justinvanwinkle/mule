"""IR1 graph construction and surgery, following ``src/compiler/ir1util.lisp``
and the linking functions at the top of ``ir1tran.lisp``.

Every mutation here maintains the reciprocal invariants that
``debug.check_ir1_consistency`` verifies: ctran/node linkage, lvar use/dest
links, leaf reference lists, block predecessor/successor lists, and the
depth-first block chain hanging off each component's head and tail.
"""

from __future__ import annotations

from collections.abc import Iterator

from .node import (
    BasicVar,
    Block,
    CallKind,
    Cleanup,
    CleanupKind,
    Combination,
    Component,
    Ctran,
    CtranKind,
    Exit,
    Lambda,
    Leaf,
    LeafVisitor,
    Lvar,
    Node,
    NodeVisitor,
    Ref,
    Return,
    Set,
    ValuedNode,
    linked,
)


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
    return node.args[0].use().accept(_MadeLambda(node))


def made_function(node: Node) -> Lambda | None:
    """The function made by a node or its single-assignment temporary."""
    return node.accept(_MadeFunction())


class _MadeFunction(NodeVisitor, LeafVisitor):
    def __init__(self):
        self.visited = set()

    def visit_Node(self, node):
        return None

    def visit_Leaf(self, leaf):
        return None

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_LambdaVar(self, leaf):
        if leaf in self.visited or len(leaf.sets) != 1:
            return None
        self.visited.add(leaf)
        value = leaf.sets[0].value
        return value.uses[0].accept(self) if len(value.uses) == 1 else None

    def visit_Combination(self, node):
        if node.kind is CallKind.KNOWN and node.fun_info.name == "%make_function":
            return made_lambda(node)
        return None


class _MadeLambda(NodeVisitor, LeafVisitor):
    """A function-making call's first operand must reference a lambda."""

    def __init__(self, call: Combination):
        self.call = call

    def visit_Node(self, node):
        raise ValueError(f"{self.call!r} does not make a function from a lambda")

    def visit_Ref(self, node):
        return node.leaf.accept(self)

    def visit_Leaf(self, leaf):
        raise ValueError(f"{self.call!r} does not make a function from a lambda")

    def visit_Lambda(self, leaf):
        return leaf


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
