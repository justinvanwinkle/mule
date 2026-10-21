"""Escape analysis: which values cannot outlive the scope that holds them.

SBCL asks this question in ``find-lvar-dynamic-extents``
(``src/compiler/envanal.lisp``) for values declared ``dynamic-extent``:
``ref-good-for-dx-p`` accepts a reference whose destination is a known
function marked ``dx-safe`` or a local call whose corresponding variable's
references do not escape either, and ``leaf-refs-not-escape-elsewhere-p``
checks every other reference of the same leaf. Mule asks it for every value
without a declaration, because reference counting makes each avoided escape
worth a retain and a release. ``spec/native-lowering.md`` records the
departure and its consequences.

The facts are conservative. A value escapes when any of its uses may keep
it: storing it into a variable that escapes, into a global, cell, object, or
container; returning, yielding, or raising it; or passing it to a callee
that may keep its argument. A full call to an unknown function keeps every
argument. Known functions keep their arguments unless the function database
declares otherwise (``FunInfo.borrowed_args``).

Parameters of local functions are solved together: a parameter escapes when
a use of it escapes, and a local call's argument escapes when the callee's
parameter does. Solving starts from "does not escape" and moves monotonically
to "escapes" over the uses of each body. Unknown and guarded calls remain
escaping: a predicted callee does not establish effects for its fallback.
These facts describe capture, not a backend ownership convention; callers
must separately prove an owner's lifetime before selecting a borrowed entry.
"""

from __future__ import annotations

from .node import (
    CallKind,
    Combination,
    Lambda,
    LambdaVar,
    LeafVisitor,
    Lvar,
    NodeVisitor,
    Ref,
    Set,
    linked,
)


class EscapeFacts:
    """The solved escape facts of one compilation."""

    def __init__(self):
        self._escaping: set[LambdaVar] = set()
        self._analyzed: set[LambdaVar] = set()

    def variable_escapes(self, var: LambdaVar) -> bool:
        """Whether a reference held by ``var`` may outlive its scope. A
        variable the analysis never saw is reported as escaping."""
        return var in self._escaping or var not in self._analyzed

    def parameter_escapes(self, lambda_: Lambda, index: int) -> bool:
        return self.variable_escapes(lambda_.vars[index])


def analyze_escapes(lambdas) -> EscapeFacts:
    facts = EscapeFacts()
    solver = _Solver(facts)
    for lambda_ in lambdas:
        solver.add_lambda(lambda_)
    solver.solve()
    return facts


class _Solver:
    """Worklist over variables: each variable lists the conditions under
    which it escapes; a condition is either unconditional or another
    variable escaping."""

    def __init__(self, facts: EscapeFacts):
        self.facts = facts
        # var -> variables whose escape makes ``var`` escape
        self.depends_on: dict[LambdaVar, list[LambdaVar]] = {}
        self.dependents: dict[LambdaVar, list[LambdaVar]] = {}
        self.pending: list[LambdaVar] = []
        self.uses = _UseClassifier(self)

    def add_lambda(self, lambda_: Lambda) -> None:
        for var in lambda_.locals:
            self.facts._analyzed.add(var)
            if var.indirect:
                # A cell outlives the frame; its contents escape with it.
                self.escape(var)
                continue
            for ref in var.refs:
                self.uses.classify(var, ref.lvar)

    def escape(self, var: LambdaVar) -> None:
        if var in self.facts._escaping:
            return
        self.facts._escaping.add(var)
        self.pending.append(var)

    def escapes_with(self, var: LambdaVar, other: LambdaVar) -> None:
        """``var`` escapes whenever ``other`` does."""
        if var is other:
            return
        self.dependents.setdefault(other, []).append(var)

    def solve(self) -> None:
        while self.pending:
            other = self.pending.pop()
            for var in self.dependents.get(other, ()):
                self.escape(var)


class _UseClassifier(NodeVisitor):
    """Decide what a use of a variable's value does with it. ``classify``
    follows the value from a reference to the node that consumes it."""

    def __init__(self, solver: _Solver):
        self.solver = solver
        self.var: LambdaVar | None = None
        self.lvar: Lvar | None = None

    @property
    def source(self) -> LambdaVar:
        return linked(self.var, self, "active escape-analysis variable")

    @property
    def operand(self) -> Lvar:
        return linked(self.lvar, self, "active escape-analysis operand")

    def classify(self, var: LambdaVar, lvar: Lvar | None) -> None:
        if lvar is None or lvar.dest is None:
            # The value is computed and discarded.
            return
        previous = self.var, self.lvar
        self.var, self.lvar = var, lvar
        try:
            lvar.dest.accept(self)
        finally:
            self.var, self.lvar = previous

    def visit_Node(self, node):
        # Returns, exits carrying values, and any consumer not classified
        # below may keep the value.
        self.solver.escape(self.source)

    def visit_If(self, node):
        # __bool__ and __len__ may publish their receiver through user code.
        self.solver.escape(self.source)

    def visit_Cast(self, node):
        # A cast passes its operand through: it escapes where the cast's
        # result goes.
        self.classify(self.source, node.lvar)

    def visit_Set(self, node: Set):
        target = node.var.accept(_LOCAL_TARGET)
        if target is None:
            self.solver.escape(self.source)
            return
        self.solver.escapes_with(self.source, target)

    def visit_Combination(self, node: Combination):
        if self.lvar is node.fun:
            # An arbitrary callable's __call__ may publish its receiver.
            self.solver.escape(self.source)
            return
        index = node.args.index(self.operand)
        _CALL_KIND_USES[node.kind](self, node, index)

    def known_call(self, node: Combination, index: int) -> None:
        info = node.fun_info
        if info is not None and info.borrows_argument(index):
            # The operation reads its argument; its result may still carry
            # the value onward when the function returns it.
            if info.returns_argument(index):
                self.classify(self.source, node.lvar)
            return
        self.solver.escape(self.source)

    def local_call(self, node: Combination, index: int) -> None:
        callee = _callee_lambda(node)
        if callee is None or index >= len(callee.vars):
            self.solver.escape(self.source)
            return
        self.solver.escapes_with(self.source, callee.vars[index])

    def unknown_call(self, node: Combination, index: int) -> None:
        self.solver.escape(self.source)


# How each call kind treats an argument. GUARDED calls fall back to a full
# call when the prediction fails, so they keep their arguments as one does.
_CALL_KIND_USES = {
    CallKind.KNOWN: _UseClassifier.known_call,
    CallKind.LOCAL: _UseClassifier.local_call,
    CallKind.FULL: _UseClassifier.unknown_call,
    CallKind.GUARDED: _UseClassifier.unknown_call,
    CallKind.ERROR: _UseClassifier.unknown_call,
}


class _LocalTarget(LeafVisitor):
    """The local variable a ``Set`` assigns, or None for any other target."""

    def visit_Leaf(self, leaf):
        return None

    def visit_LambdaVar(self, leaf: LambdaVar):
        return None if leaf.indirect else leaf


_LOCAL_TARGET = _LocalTarget()


class _CalleeLambda(NodeVisitor):
    def visit_Node(self, node):
        return None

    def visit_Ref(self, node: Ref):
        return node.leaf.accept(_LAMBDA_LEAF)


_CALLEE_LAMBDA = _CalleeLambda()


class _LambdaLeaf(LeafVisitor):
    def visit_Leaf(self, leaf):
        return None

    def visit_Lambda(self, leaf: Lambda):
        return leaf


_LAMBDA_LEAF = _LambdaLeaf()


def _callee_lambda(node: Combination) -> Lambda | None:
    if len(node.fun.uses) != 1:
        return None
    return node.fun.uses[0].accept(_CALLEE_LAMBDA)
