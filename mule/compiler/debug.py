"""IR1 consistency checking, following ``src/compiler/debug.lisp``.

``check_ir1_consistency`` verifies the reciprocal links every IR1 phase relies
on. It is run after conversion and after each optimization phase in tests, and
whenever a graph rewrite is suspected of leaving the representation malformed.
"""

from __future__ import annotations

from .node import (
    Bind,
    Cast,
    Combination,
    Component,
    CtranKind,
    Entry,
    Exit,
    FunctionalKind,
    If,
    Lambda,
    Node,
    Ref,
    Return,
    Set,
    ValuedNode,
)


class InconsistentIR1(Exception):
    pass


def barf(message: str) -> None:
    raise InconsistentIR1(message)


class _Checker:
    def __init__(self, components: list[Component]):
        self.components = components
        self.seen_blocks: set[int] = set()
        self.seen_funs: set[int] = set()

    def run(self) -> None:
        for component in self.components:
            self.check_chain(component)
        self.check_fun_consistency()
        for component in self.components:
            for block in component.blocks():
                self.check_block(block)

    def check_chain(self, component: Component) -> None:
        head, tail = component.head, component.tail
        if head.pred or tail.succ:
            barf(f"{component!r} is malformed")
        previous = None
        block = head
        while block is not None:
            self.seen_blocks.add(id(block))
            if block.prev is not previous:
                barf(f"bad PREV for {block!r}, should be {previous!r}")
            if block.component is not component:
                barf(f"{block!r} is not in {component!r}")
            previous, block = block, block.next
        if previous is not tail:
            barf(f"wrong TAIL for DFO, {previous!r} in {component!r}")

    def observe_functional(self, functional) -> None:
        if id(functional) in self.seen_funs:
            barf(f"{functional!r} was seen more than once")
        if functional.kind is not FunctionalKind.DELETED:
            self.seen_funs.add(id(functional))

    def check_fun_reached(self, functional, where) -> None:
        if id(functional) not in self.seen_funs:
            barf(f"unseen function {functional!r} in {where!r}")

    def check_node_reached(self, node: Node) -> None:
        if node.prev is None or id(node.prev.block) not in self.seen_blocks:
            barf(f"{node!r} is not in a seen block")

    def check_fun_consistency(self) -> None:
        for component in self.components:
            for functional in component.new_functionals:
                self.observe_functional(functional)
            for functional in component.lambdas:
                self.observe_functional(functional)
                for let in functional.lets:
                    self.observe_functional(let)
        for component in self.components:
            for functional in [*component.new_functionals, *component.lambdas]:
                self.check_fun_stuff(functional)

    def check_fun_stuff(self, functional: Lambda) -> None:
        kind = functional.kind
        if kind is FunctionalKind.DELETED:
            return
        if kind is FunctionalKind.EXTERNAL:
            entry = functional.entry_fun
            self.check_fun_reached(entry, functional)
            if entry.entry_fun is not functional:
                barf(f"bad back-pointer in function for XEP {functional!r}")
        elif kind in (FunctionalKind.LET, FunctionalKind.ASSIGNMENT):
            self.check_fun_reached(functional.home, functional)
            if functional not in functional.home.lets:
                barf(f"the LET {functional!r} is not in LETs for its home")
        elif kind is FunctionalKind.TOPLEVEL:
            if functional.entry_fun is not functional:
                barf(f"the ENTRY-FUN in {functional!r} is not a self-pointer")
        if kind in (
            FunctionalKind.NORMAL,
            FunctionalKind.OPTIONAL,
            FunctionalKind.EXTERNAL,
            FunctionalKind.TOPLEVEL,
        ):
            for let in functional.lets:
                if let.home is not functional:
                    barf(f"the home in {let!r} is not {functional!r}")
                self.check_fun_reached(let, functional)
            if functional.home is not functional:
                barf(f"home not self-pointer in {functional!r}")
        if functional.bind is not None:
            self.check_node_reached(functional.bind)
        if functional.return_ is not None:
            self.check_node_reached(functional.return_)
        for var in functional.locals:
            for ref in var.refs:
                self.check_node_reached(ref)
            for assignment in var.sets:
                self.check_node_reached(assignment)
            if var.home is not functional:
                barf(f"HOME in {var!r} should be {functional!r}")
        for var in functional.vars:
            if var not in functional.locals:
                barf(f"parameter {var!r} is not among the locals of {functional!r}")

    def check_block(self, block) -> None:
        for pred in block.pred:
            if id(pred) not in self.seen_blocks:
                barf(f"unseen predecessor {pred!r} in {block!r}")
            if block not in pred.succ:
                barf(f"bad predecessor link {pred!r} in {block!r}")
        ctran = block.start
        last = block.last
        if ctran is None:
            barf(f"{block!r} has no START")
        if last is None:
            barf(f"{block!r} has no LAST")
        if ctran.kind is not CtranKind.BLOCK_START:
            barf(f"the START of {block!r} has the wrong kind")
        if ctran.use is not None:
            barf(f"the ctran {ctran!r} is used")
        if last.next is not None:
            barf(f"last node {last!r} of {block!r} has a next ctran")
        home = last.lexenv.lambda_
        if home.kind is not FunctionalKind.DELETED:
            self.check_fun_reached(home, block)
        while True:
            if ctran.block is not block:
                barf(f"BLOCK of {ctran!r} should be {block!r}")
            node = ctran.next
            if node is None:
                barf(f"{ctran!r} has no NEXT node")
            if node.prev is not ctran:
                barf(f"PREV in {node!r} should be {ctran!r}")
            if isinstance(node, ValuedNode) and node.lvar is not None:
                lvar = node.lvar
                if node not in lvar.uses:
                    barf(f"{node!r} is not used by its LVAR {lvar!r}")
                if lvar.dest is None:
                    barf(f"{lvar!r} does not have a dest")
            self.check_node_reached(node)
            self.check_node(node)
            following = node.next
            if following is None and node is not last:
                barf(f"{node!r} has no NEXT")
            if node is last:
                break
            if following.kind is not CtranKind.INSIDE_BLOCK:
                barf(
                    f"the interior ctran {following!r} in {block!r} has the wrong kind"
                )
            if following.next is None:
                barf(f"{following!r} has no NEXT")
            if following.use is not node:
                barf(f"USE in {following!r} should be {node!r}")
            ctran = following
        self.check_successors(block)

    def check_successors(self, block) -> None:
        last = block.last
        component = block.component
        for successor in block.succ:
            if id(successor) not in self.seen_blocks:
                barf(f"unseen successor {successor!r} in {block!r}")
            if block not in successor.pred:
                barf(f"bad successor link {successor!r} in {block!r}")
            if successor.component is not component:
                barf(
                    f"the successor {successor!r} in {block!r} is in another component"
                )
        if isinstance(last, If):
            if len(block.succ) not in (1, 2):
                barf(f"{block!r} ends in an IF without one or two successors")
            if last.consequent not in block.succ or last.alternative not in block.succ:
                barf(f"the branches of {last!r} are not the successors of {block!r}")
        elif isinstance(last, Return):
            if block.succ != [component.tail]:
                barf(f"strange successors for RETURN in {block!r}")
        elif isinstance(last, Exit):
            if len(block.succ) > 1:
                barf(f"EXIT node with strange number of successors: {last!r}")
        elif isinstance(last, Combination) and _never_returns(last):
            if block.succ:
                barf(f"{block!r} ends in a non-returning call but has successors")
        elif len(block.succ) != 1:
            barf(f"{block!r} ends in a normal node without exactly one successor")

    def check_dest(self, lvar, node) -> None:
        for use in lvar.uses:
            if id(use.block) not in self.seen_blocks:
                barf(f"node {use!r} using {lvar!r} is in an unknown block")
        if lvar.dest is not node:
            barf(f"DEST for {lvar!r} should be {node!r}")

    def check_node(self, node: Node) -> None:
        if isinstance(node, Ref):
            leaf = node.leaf
            if node not in leaf.refs:
                barf(f"{node!r} is not in the refs of {leaf!r}")
            if isinstance(leaf, Lambda):
                self.check_fun_reached(leaf, node)
        elif isinstance(node, Combination):
            self.check_dest(node.fun, node)
            for arg in node.args:
                self.check_dest(arg, node)
        elif isinstance(node, If):
            self.check_dest(node.test, node)
            if node.block.last is not node:
                barf(f"IF not at block end: {node!r}")
        elif isinstance(node, Set):
            if node.value is not None:
                self.check_dest(node.value, node)
            if node not in node.var.sets:
                barf(f"{node!r} is not in the sets of {node.var!r}")
        elif isinstance(node, Cast):
            self.check_dest(node.value, node)
        elif isinstance(node, Bind):
            self.check_fun_reached(node.lambda_, node)
        elif isinstance(node, Return):
            self.check_fun_reached(node.lambda_, node)
            self.check_dest(node.result, node)
            if node.block.last is not node:
                barf(f"RETURN not at block end: {node!r}")
        elif isinstance(node, Entry):
            if node not in node.home_lambda.entries:
                barf(f"{node!r} is not in ENTRIES for its home lambda")
        elif isinstance(node, Exit):
            if node.value is not None:
                self.check_dest(node.value, node)
            if node.entry is not None and node not in node.entry.exits:
                barf(f"{node!r} is not in the exits of its entry")


def _never_returns(node: Combination) -> bool:
    info = node.fun_info
    return info is not None and info.never_returns


def check_ir1_consistency(components) -> None:
    _Checker(list(components)).run()
