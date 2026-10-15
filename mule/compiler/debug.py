"""IR1 consistency checking, following ``src/compiler/debug.lisp``.

``check_ir1_consistency`` verifies the reciprocal links every IR1 phase relies
on. It is run after conversion and after each optimization phase in tests, and
whenever a graph rewrite is suspected of leaving the representation malformed.
"""

from __future__ import annotations

from typing import NoReturn

from .node import (
    Combination,
    Component,
    CtranKind,
    FunctionalKind,
    Lambda,
    LeafVisitor,
    Node,
    NodeVisitor,
)


class InconsistentIR1(Exception):
    pass


def barf(message: str) -> NoReturn:
    raise InconsistentIR1(message)


class _Checker(NodeVisitor, LeafVisitor):
    def __init__(self, components: list[Component]):
        self.components = components
        self.seen_blocks: set = set()
        self.seen_funs: set = set()
        self.leaf_refs: dict[object, set] = {}
        self.var_sets: dict[object, set] = {}
        self._current_ref: Node | None = None
        self._successor_checker = _SuccessorChecker()

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
        seen_blocks = self.seen_blocks
        while block is not None:
            seen_blocks.add(block)
            if block.prev is not previous:
                barf(f"bad PREV for {block!r}, should be {previous!r}")
            if block.component is not component:
                barf(f"{block!r} is not in {component!r}")
            previous, block = block, block.next
        if previous is not tail:
            barf(f"wrong TAIL for DFO, {previous!r} in {component!r}")

    def observe_functional(self, functional) -> None:
        if functional in self.seen_funs:
            barf(f"{functional!r} was seen more than once")
        if functional.kind is not FunctionalKind.DELETED:
            self.seen_funs.add(functional)

    def check_fun_reached(self, functional, where) -> None:
        if functional not in self.seen_funs:
            barf(f"unseen function {functional!r} in {where!r}")

    def check_node_reached(self, node: Node) -> None:
        prev = node.prev
        if prev is None or prev.block not in self.seen_blocks:
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
            for functional in (*component.new_functionals, *component.lambdas):
                self.check_fun_stuff(functional)

    def check_fun_stuff(self, functional: Lambda) -> None:
        kind = functional.kind
        if kind is FunctionalKind.DELETED:
            return
        if kind is FunctionalKind.EXTERNAL:
            entry = functional.entry_fun
            self.check_fun_reached(entry, functional)
            if entry is None or entry.entry_fun is not functional:
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
        locals_list = functional.locals
        for var in locals_list:
            for ref in var.refs:
                self.check_node_reached(ref)
            for assignment in var.sets:
                self.check_node_reached(assignment)
            if var.home is not functional:
                barf(f"HOME in {var!r} should be {functional!r}")
        locals_container = set(locals_list) if len(locals_list) > 8 else locals_list
        for var in functional.vars:
            if var not in locals_container:
                barf(f"parameter {var!r} is not among the locals of {functional!r}")

    def check_block(self, block) -> None:
        seen_blocks = self.seen_blocks
        for pred in block.pred:
            if pred not in seen_blocks:
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
            lvar = node.value_lvar
            if lvar is not None:
                uses = lvar.uses
                if not uses or (uses[0] is not node and node not in uses):
                    barf(f"{node!r} is not used by its LVAR {lvar!r}")
                if lvar.dest is None:
                    barf(f"{lvar!r} does not have a dest")
            node.accept(self)
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
        seen_blocks = self.seen_blocks
        for successor in block.succ:
            if successor not in seen_blocks:
                barf(f"unseen successor {successor!r} in {block!r}")
            if block not in successor.pred:
                barf(f"bad successor link {successor!r} in {block!r}")
            if successor.component is not component:
                barf(
                    f"the successor {successor!r} in {block!r} is in another component"
                )
        if last is None:
            barf(f"{block!r} has no LAST")
        checker = self._successor_checker
        checker.block = block
        last.accept(checker)
        checker.block = None

    def check_dest(self, lvar, node) -> None:
        seen_blocks = self.seen_blocks
        for use in lvar.uses:
            prev = use.prev
            if prev is None or prev.block not in seen_blocks:
                barf(f"node {use!r} using {lvar!r} is in an unknown block")
        if lvar.dest is not node:
            barf(f"DEST for {lvar!r} should be {node!r}")

    def check_node(self, node: Node) -> None:
        node.accept(self)

    def visit_Ref(self, node):
        leaf = node.leaf
        refs = leaf.refs
        if not refs:
            barf(f"{node!r} is not in the refs of {leaf!r}")
        elif refs[0] is not node:
            if len(refs) > 8:
                ref_set = self.leaf_refs.get(leaf)
                if ref_set is None:
                    ref_set = set(refs)
                    self.leaf_refs[leaf] = ref_set
                if node not in ref_set:
                    barf(f"{node!r} is not in the refs of {leaf!r}")
            elif node not in refs:
                barf(f"{node!r} is not in the refs of {leaf!r}")
        self._current_ref = node
        leaf.accept(self)
        self._current_ref = None

    def visit_Lambda(self, leaf):
        self.check_fun_reached(leaf, self._current_ref)

    def visit_Combination(self, node):
        check_dest = self.check_dest
        check_dest(node.fun, node)
        for arg in node.args:
            check_dest(arg, node)

    def visit_If(self, node):
        self.check_dest(node.test, node)
        if node.block.last is not node:
            barf(f"IF not at block end: {node!r}")

    def visit_Set(self, node):
        if node.value is not None:
            self.check_dest(node.value, node)
        var = node.var
        sets = var.sets
        if not sets:
            barf(f"{node!r} is not in the sets of {var!r}")
        elif sets[0] is not node:
            if len(sets) > 8:
                set_set = self.var_sets.get(var)
                if set_set is None:
                    set_set = set(sets)
                    self.var_sets[var] = set_set
                if node not in set_set:
                    barf(f"{node!r} is not in the sets of {var!r}")
            elif node not in sets:
                barf(f"{node!r} is not in the sets of {var!r}")

    def visit_Cast(self, node):
        self.check_dest(node.value, node)

    def visit_Bind(self, node):
        self.check_fun_reached(node.lambda_, node)

    def visit_Return(self, node):
        self.check_fun_reached(node.lambda_, node)
        self.check_dest(node.result, node)
        if node.block.last is not node:
            barf(f"RETURN not at block end: {node!r}")

    def visit_Entry(self, node):
        if node not in node.home_lambda.entries:
            barf(f"{node!r} is not in ENTRIES for its home lambda")

    def visit_Exit(self, node):
        if node.value is not None:
            self.check_dest(node.value, node)
        if node.entry is not None and node not in node.entry.exits:
            barf(f"{node!r} is not in the exits of its entry")


class _SuccessorChecker(NodeVisitor):
    """Checks a block's successors against the node that ends it."""

    def __init__(self, block=None):
        self.block = block

    def visit_Node(self, node):
        if len(self.block.succ) != 1:
            barf(f"{self.block!r} ends in a normal node without exactly one successor")

    def visit_If(self, node):
        block = self.block
        if len(block.succ) not in (1, 2):
            barf(f"{block!r} ends in an IF without one or two successors")
        if node.consequent not in block.succ or node.alternative not in block.succ:
            barf(f"the branches of {node!r} are not the successors of {block!r}")

    def visit_Return(self, node):
        if self.block.succ != [self.block.component.tail]:
            barf(f"strange successors for RETURN in {self.block!r}")

    def visit_Exit(self, node):
        if len(self.block.succ) > 1:
            barf(f"EXIT node with strange number of successors: {node!r}")

    def visit_Combination(self, node):
        if not _never_returns(node):
            self.visit_Node(node)
        elif self.block.succ:
            barf(f"{self.block!r} ends in a non-returning call but has successors")


def _never_returns(node: Combination) -> bool:
    info = node.fun_info
    return info is not None and info.never_returns


def check_ir1_consistency(components) -> None:
    _Checker(list(components)).run()
